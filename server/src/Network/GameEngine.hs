{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.GameEngine where

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
     ,FromJSON msg
     ,ToJSON view
     ,Show view
     ,Show state
     ,Show msg)
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
             _ <- spawnLocal $ broadcaster rxGameView rxSubscription
             _ <- spawnLocal $ gameProcess rxGameMsg txGameView update view initialGameState
             liftIO . Warp.run websocketPort $
               websocketsOr
                 WS.defaultConnectionOptions
                 (runResourceT . acceptClientConnection node txGameMsg txSubscription)
                 (staticApp $ defaultFileServerSettings "../client/dist")
        logInfoN "END"

acceptClientConnection
  :: (MonadResource m
     ,Serializable msg
     ,Serializable view
     ,FromJSON msg
     ,ToJSON view
     ,Show view
     ,Show msg)
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
         spawnLocal $ receiveFromPlayer txToPlayer txGameMsg disconnectHandler connection
       sendChan txSubscribe (Sub txToPlayer)
       sendChan txGameMsg (sendPortId txToPlayer, Join)
       announceToPlayer connection rxFromBroadcaster disconnectHandler

------------------------------------------------------------
-- Player
------------------------------------------------------------
-- TODO This process could do with some refactoring.
receiveFromPlayer
  :: (Serializable msg, Serializable view, Show msg, Show view, FromJSON msg)
  => SendPort view
  -> SendPort (SendPortId, EngineMsg msg)
  -> (WS.ConnectionException -> Process ())
  -> WS.Connection
  -> Process ()
receiveFromPlayer txToPlayer txGameMsg disconnectHandler connection = do
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

announceToPlayer
  :: (Show view, Serializable view, ToJSON view)
  => WS.Connection
  -> ReceivePort view
  -> (WS.ConnectionException -> Process ())
  -> Process ()
announceToPlayer connection rx disconnectHandler = handle
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
broadcaster
  :: (Show view, Serializable view)
  => ReceivePort view -> ReceivePort (PubSubMsg view) -> Process ()
broadcaster inboundGame subscriptionRequests = iterateM_ handle Set.empty
  where
    handle subscribers = do
      liftIO . putStrLn $ "B: Subscribers: " <> show subscribers
      mergedPorts <-
        mergePortsBiased [Left <$> subscriptionRequests, Right <$> inboundGame]
      msg <- receiveChan mergedPorts
      case msg of
        Left (Sub subscriber) -> do
          liftIO . putStrLn $ "B: adding: " <> show subscriber
          return (Set.insert subscriber subscribers)
        Left (Unsub subscriber) -> do
          liftIO . putStrLn $ "B: removing: " <> show subscriber
          return (Set.delete subscriber subscribers)
        Right state -> do
          liftIO . putStrLn $
            "B: Broadcasting: " <> show state <> " to: " <> show subscribers
          traverse_ (`sendChan` state) $ Set.toList subscribers
          return subscribers

------------------------------------------------------------
-- Game
------------------------------------------------------------
gameProcess
  :: (Serializable msg, Serializable view, Show msg, Show state)
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
