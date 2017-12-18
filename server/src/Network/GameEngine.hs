{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.GameEngine
  ( runGame
  , PlayerId
  , EngineMsg(..)
  ) where

import Control.Distributed.Process
import Control.Distributed.Process.Extras.Time
import Control.Distributed.Process.Extras.Timer
import Control.Distributed.Process.Node
import Control.Distributed.Process.Serializable
import qualified Control.Exception as Ex
import Control.Monad (void)
import Control.Monad.Loops
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Binary
import Data.Foldable
import Data.Function ((&))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Generics
import Network.Transport.InMemory
import Network.Wai.Application.Static
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WebSockets
import qualified Network.WebSockets as WS

newtype PlayerId =
  PlayerId SendPortId
  deriving (Show, Eq, Ord, Binary, Generic)

data EngineMsg msg
  = Join PlayerId
  | Leave PlayerId
  | GameTick
  | GameMsg PlayerId
            msg
  deriving (Show, Eq, Binary, Generic)

data PubSubMsg globalView playerView
  = SubGlobal (SendPort globalView)
  | SubPlayer (SendPort playerView)
  | UnsubGlobal (SendPort globalView)
  | UnsubPlayer (SendPort playerView)
  deriving (Show, Eq, Binary, Generic)

------------------------------------------------------------
-- Websocket Server & Wiring.
------------------------------------------------------------
runGame ::
     ( Serializable msg
     , Serializable globalView
     , Serializable playerView
     , FromJSON msg
     , ToJSON globalView
     , ToJSON playerView
     )
  => state
  -> (EngineMsg msg -> state -> state)
  -> (state -> (globalView, Map PlayerId playerView))
  -> IO ()
runGame initialGameState update viewFn =
  let settings = Warp.setHost "*" . Warp.setPort 8000 $ Warp.defaultSettings
  in do putStrLn "Starting up..."
        backend <- createTransport
        node <- newLocalNode backend initRemoteTable
        runProcess node $ do
          (txSubscription, rxSubscription) <- newChan
          (txGame, rxGame) <- newChan
          (txGameMsg, rxGameMsg) <- newChan
          _ <-
            spawnLocal . void . periodically (milliSeconds 500) $
            sendChan txGameMsg GameTick
          _ <- spawnLocal $ broadcaster rxGame rxSubscription
          _ <-
            spawnLocal $
            gameProcess rxGameMsg txGame update viewFn initialGameState
          liftIO . Warp.runSettings settings $
            websocketsOr
              WS.defaultConnectionOptions
              (acceptClientConnection node txGameMsg txSubscription)
              (staticApp $ defaultFileServerSettings "../client/dist")

acceptClientConnection ::
     ( Serializable msg
     , Serializable globalView
     , Serializable playerView
     , FromJSON msg
     , ToJSON globalView
     , ToJSON playerView
     )
  => LocalNode
  -> SendPort (EngineMsg msg)
  -> SendPort (PubSubMsg globalView playerView)
  -> WS.PendingConnection
  -> IO ()
acceptClientConnection node txGameMsg txSubscribe pendingConnection = do
  let path = WS.pendingRequest pendingConnection & WS.requestPath
  connection <- WS.acceptRequest pendingConnection
  if path == "/global"
    then acceptGlobal connection
    else acceptPlayer connection
  where
    acceptGlobal connection =
      runProcess node $ do
        (txToGlobal, rxFromBroadcaster) <- newChan
        let disconnectHandler = sendChan txSubscribe (UnsubGlobal txToGlobal)
        _ <-
          spawnLocal $
          receiveFromPlayer txToGlobal txGameMsg disconnectHandler connection
        sendChan txSubscribe (SubGlobal txToGlobal)
        announceToPlayer connection rxFromBroadcaster disconnectHandler
    acceptPlayer connection =
      runProcess node $ do
        (txToPlayer, rxFromBroadcaster) <- newChan
        let disconnectHandler = do
              sendChan txSubscribe (UnsubPlayer txToPlayer)
              sendChan txGameMsg $ Leave (playerId txToPlayer)
        _ <-
          spawnLocal $
          receiveFromPlayer txToPlayer txGameMsg disconnectHandler connection
        sendChan txSubscribe (SubPlayer txToPlayer)
        sendChan txGameMsg $ Join (playerId txToPlayer)
        announceToPlayer connection rxFromBroadcaster disconnectHandler

------------------------------------------------------------
-- Player
------------------------------------------------------------

playerId :: SendPort a -> PlayerId
playerId = PlayerId . sendPortId

receiveFromPlayer ::
     (Serializable msg, Serializable playerView, FromJSON msg)
  => SendPort playerView
  -> SendPort (EngineMsg msg)
  -> Process ()
  -> WS.Connection
  -> Process ()
receiveFromPlayer txToPlayer txGameMsg disconnectHandler connection = handle
  where
    handle = do
      raw <- liftIO . Ex.try $ WS.receiveDataMessage connection
      case raw of
        Left (_ :: WS.ConnectionException) -> disconnectHandler
        Right (WS.Binary _) -> handle
        Right (WS.Text text) ->
          case Aeson.decode text of
            Nothing -> handle
            Just msg -> do
              sendChan txGameMsg $ GameMsg (playerId txToPlayer) msg
              handle

announceToPlayer ::
     (Serializable view, ToJSON view)
  => WS.Connection
  -> ReceivePort view
  -> Process ()
  -> Process ()
announceToPlayer connection rx disconnectHandler = handle
  where
    handle = do
      msg <- receiveChan rx
      sent <- liftIO . Ex.try . WS.sendTextData connection $ Aeson.encode msg
      case sent of
        Left (_ :: WS.ConnectionException) -> disconnectHandler
        Right _ -> handle

------------------------------------------------------------
-- Broadcaster
------------------------------------------------------------
broadcaster ::
     (Serializable globalView, Serializable playerView)
  => ReceivePort (globalView, Map PlayerId playerView)
  -> ReceivePort (PubSubMsg globalView playerView)
  -> Process ()
broadcaster rxGame rxSubscription = iterateM_ handle (Set.empty, Set.empty)
  where
    handle (viewers, players) = do
      mergedPorts <-
        mergePortsBiased [Left <$> rxSubscription, Right <$> rxGame]
      msg <- receiveChan mergedPorts
      case msg of
        Left (SubGlobal viewer) -> return (Set.insert viewer viewers, players)
        Left (SubPlayer player) -> return (viewers, Set.insert player players)
        Left (UnsubGlobal viewer) -> return (Set.delete viewer viewers, players)
        Left (UnsubPlayer player) -> return (viewers, Set.delete player players)
        Right (globalView, playerViews) -> do
          traverse_ (`sendChan` globalView) viewers
          forM_ players $ \s ->
            case Map.lookup (playerId s) playerViews of
              Just view -> sendChan s view
              Nothing -> return ()
          return (viewers, players)

------------------------------------------------------------
-- Game
------------------------------------------------------------
gameProcess ::
     ( Serializable msg
     , Serializable globalView
     , Serializable playerView
     )
  => ReceivePort (EngineMsg msg)
  -> SendPort (globalView, Map PlayerId playerView)
  -> (EngineMsg msg -> state -> state)
  -> (state -> (globalView, Map PlayerId playerView))
  -> state
  -> Process ()
gameProcess rxGameMsg txGame updateFn viewFn = iterateM_ handle
  where
    handle game = do
      msg <- receiveChan rxGameMsg
      let game' = updateFn msg game
      case msg of
        GameTick -> sendChan txGame (viewFn game')
        _ -> return ()
      return game'
