{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import           Control.Distributed.Process
import           Control.Distributed.Process.Node
import           Control.Distributed.Process.Serializable
import qualified Control.Exception                        as Ex
import           Control.Monad.Logger
import           Control.Monad.Loops
import           Control.Monad.Trans.Resource
import           Data.Aeson                               (FromJSON, ToJSON)
import qualified Data.Aeson                               as Aeson
import           Data.Binary
import           Data.Foldable
import           Data.Monoid
import qualified Data.Set                                 as Set
import qualified Data.Text                                as T
import           Data.Time
import           GHC.Generics
import           Network.Transport.InMemory
import           Network.Wai.Application.Static
import qualified Network.Wai.Handler.Warp                 as Warp
import           Network.Wai.Handler.WebSockets
import qualified Network.WebSockets                       as WS

data EngineMsg msg
  = Join
  | Leave
  | GameMsg msg
  deriving (Show, Eq, Binary, Generic)

data PubSubMsg view
  = Sub (SendPort view)
  | Unsub (SendPort view)
  deriving (Show, Eq, Binary, Generic)

timeBetweenCommands :: NominalDiffTime
timeBetweenCommands = 0.1

------------------------------------------------------------
-- Websocket Server & Wiring.
------------------------------------------------------------
runGame
  :: (Serializable msg
     ,Serializable view
     ,Show view
     ,Show state
     ,FromJSON msg
     ,Show msg
     ,ToJSON view)
  => ((SendPortId, EngineMsg msg) -> state -> state)
  -> (state -> view)
  -> state
  -> IO ()
runGame update view initialGameState =
  let websocketPort = 8000
  in runStdoutLoggingT $
     do logInfoN "START"
        logInfoN "Booting Cloud Haskell"
        backend <- liftIO createTransport
        node <- liftIO $ newLocalNode backend initRemoteTable
        logInfoN $ "Starting listener on port: " <> T.pack (show websocketPort)
        _ <-
          liftIO . runProcess node $
          do (txSubscription, rxSubscription) <- newChan
             (txGameView, rxGameView) <- newChan
             (txGameMsg, rxGameMsg) <- newChan
             _ <- spawnLocal $ broadcastProcess rxGameView rxSubscription
             _ <- spawnLocal $ gameProcess rxGameMsg txGameView update view initialGameState
             liftIO . Warp.run websocketPort $
               websocketsOr
                 WS.defaultConnectionOptions
                 (runResourceT . acceptClientConnection node txGameMsg txSubscription)
                 (staticApp $ defaultFileServerSettings "../client/dist")
        logInfoN "END"

acceptClientConnection
  :: (MonadResource m
     ,Serializable view
     ,Show view
     ,ToJSON view
     ,Serializable msg
     ,Show msg
     ,FromJSON msg)
  => LocalNode
  -> SendPort (SendPortId, EngineMsg msg)
  -> SendPort (PubSubMsg view)
  -> WS.PendingConnection
  -> m ()
acceptClientConnection node txGameMsg txSubscribe pendingConnection = do
  (_releaseKey, connection) <-
    allocate
      (runStdoutLoggingT $
       do logInfoN "P: New connection received."
          liftIO $ WS.acceptRequest pendingConnection)
      (\_ -> putStrLn "P: Leaves")
  liftIO . runProcess node $
    do (txToPlayer, rxFromBroadcaster) <- newChan
       let disconnectHandler :: WS.ConnectionException -> Process ()
           disconnectHandler ex = do
             liftIO . putStrLn $
               "P: Socket has closed. Unsubscribing: " <> show ex
             sendChan txSubscribe (Unsub txToPlayer)
             sendChan txGameMsg (sendPortId txToPlayer, Leave)
       _ <-
         spawnLocal $
         receiveFromPlayerProcess txToPlayer txGameMsg disconnectHandler connection
       sendChan txSubscribe (Sub txToPlayer)
       sendChan txGameMsg (sendPortId txToPlayer, Join)
       announceToPlayerProcess connection rxFromBroadcaster disconnectHandler

------------------------------------------------------------
-- Player
------------------------------------------------------------
receiveFromPlayerProcess
  :: (Serializable view, Show view, Serializable msg, Show msg, FromJSON msg)
  => SendPort view
  -> SendPort (SendPortId, EngineMsg msg)
  -> (WS.ConnectionException -> Process ())
  -> WS.Connection
  -> Process ()
receiveFromPlayerProcess txToPlayer txGameMsg disconnectHandler connection = do
  liftIO $ putStrLn "P: LISTENING"
  handle Nothing
  where
    handle :: Maybe UTCTime -> Process ()
    handle lastMessageHandled = do
      raw <- liftIO . Ex.try $ WS.receiveDataMessage connection
      case raw of
        Left ex -> disconnectHandler ex
        Right (WS.Binary _) -> handle lastMessageHandled
        Right (WS.Text text) -> do
          liftIO . putStrLn $ "P: HEARD: " <> show text
          case Aeson.eitherDecode text of
            Left err -> do
              liftIO . putStrLn $ "Couldn't understand: " <> show text
              liftIO . putStrLn $ "  Error was: " <> show err
              handle lastMessageHandled
            Right msg -> do
              now <- liftIO getCurrentTime
              case lastMessageHandled of
                Nothing -> do
                  sendChan txGameMsg (sendPortId txToPlayer, GameMsg msg)
                  handle (Just now)
                Just t ->
                  if addUTCTime timeBetweenCommands t < now
                    then do
                      sendChan txGameMsg (sendPortId txToPlayer, GameMsg msg)
                      handle (Just now)
                    else handle lastMessageHandled

announceToPlayerProcess
  :: (Show view, Serializable view, ToJSON view)
  => WS.Connection
  -> ReceivePort view
  -> (WS.ConnectionException -> Process ())
  -> Process ()
announceToPlayerProcess connection rx disconnectHandler = handle
  where
    handle = do
      msg <- receiveChan rx
      sent <- liftIO . Ex.try . WS.sendTextData connection $ Aeson.encode msg
      case sent of
        Left ex -> disconnectHandler ex
        Right _ -> handle

------------------------------------------------------------
-- Broadcaster
------------------------------------------------------------
broadcastProcess
  :: (Show view, Serializable view)
  => ReceivePort view -> ReceivePort (PubSubMsg view) -> Process ()
broadcastProcess inboundGame subscriptionRequests = iterateM_ handle Set.empty
  where
    handle subscribers = do
      liftIO . putStrLn $ "B: Subscribers: " <> show subscribers
      mergedPorts <-
        mergePortsBiased [Left <$> inboundGame, Right <$> subscriptionRequests]
      msg <- receiveChan mergedPorts
      case msg of
        Left state -> do
          liftIO . putStrLn $
            "B: Broadcasting: " <> show state <> " to: " <> show subscribers
          traverse_ (`sendChan` state) $ Set.toList subscribers
          return subscribers
        Right (Sub subscriber) -> do
          liftIO . putStrLn $ "B: adding: " <> show subscriber
          return (Set.insert subscriber subscribers)
        Right (Unsub subscriber) -> do
          liftIO . putStrLn $ "B: removing: " <> show subscriber
          return (Set.delete subscriber subscribers)

------------------------------------------------------------
-- Game
------------------------------------------------------------
gameProcess
  :: (Show msg, Serializable msg, Serializable view, Show state)
  => ReceivePort (SendPortId, msg)
  -> SendPort view
  -> ((SendPortId, msg) -> state -> state)
  -> (state -> view)
  -> state
  -> Process ()
gameProcess rxGameMsg txGameView updateFn viewFn = iterateM_ handle
  where
    handle game = do
      (replyTo, msg) <- receiveChan rxGameMsg
      liftIO . putStrLn $ "G: Heard: " <> show msg
      let newGame = updateFn (replyTo, msg) game
      sendChan txGameView (viewFn newGame)
      return newGame
