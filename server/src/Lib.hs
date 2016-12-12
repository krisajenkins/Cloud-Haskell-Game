{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import           Control.Distributed.Process

import           Control.Distributed.Process.Node
import           Control.Distributed.Process.Serializable
import qualified Control.Exception                        as Ex
import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Loops
import           Control.Monad.Trans.Resource
import           Data.Aeson                               (FromJSON, ToJSON)
import qualified Data.Aeson                               as Aeson
import           Data.Binary
import           Data.Foldable
import           Data.Monoid
import qualified Data.Set                                 as Set
import           Data.Time
import           GHC.Generics
import           Network.Transport.InMemory
import qualified Network.WebSockets                       as WS

data EngineMsg msg view
  = GameMetaMsg msg
  | BroadcasterMsg (PubSubMsg view)
  deriving (Show, Eq, Binary, Generic)

data PubSubMsg a
  = Sub (SendPort a)
  | Unsub (SendPort a)
  deriving (Show, Eq, Binary, Generic)

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
  => ((SendPortId, msg) -> state -> state) -> (state -> view) -> state -> IO ()
runGame update view initialGameState =
  let host = "0.0.0.0"
      websocketPort = 9000
  in runStdoutLoggingT $
     do logInfoN "START"
        logInfoN "Booting Cloud Haskell"
        backend <- liftIO createTransport
        logInfoN "Creating Node"
        node <- liftIO $ newLocalNode backend initRemoteTable
        logInfoN "Forking listener."
        _ <-
          liftIO . runProcess node $
          do (sendSubGameState, receiveSubGameState) <- newChan
             (txGameState, rxGameState) <- newChan
             (sendGameMsg, receiveGameMsg) <- newChan
             _ <- spawnLocal $ broadcastProcess rxGameState receiveSubGameState
             _ <-
               spawnLocal $ gameProcess receiveGameMsg txGameState update view initialGameState
             liftIO . WS.runServer host websocketPort $
               runResourceT . acceptClientConnection node sendGameMsg sendSubGameState
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
  -> SendPort (SendPortId, msg)
  -> SendPort (PubSubMsg view)
  -> WS.PendingConnection
  -> m ()
acceptClientConnection node txGameMsg txSubscribe pendingConnection = do
  (_releaseKey, connection) <-
    allocate
      (runStdoutLoggingT $
       do liftIO . putStrLn $ "P: New connection received."
          liftIO $ WS.acceptRequest pendingConnection)
      (\_ -> putStrLn "P: Leaves")
  liftIO . runProcess node $
    do (sendToMe, receiveFromBroadcaster) <- newChan
       _ <-
         spawnLocal $ receiveFromPlayerProcess txSubscribe sendToMe txGameMsg connection
       sendChan txSubscribe (Sub sendToMe)
       announceToPlayerProcess connection receiveFromBroadcaster

------------------------------------------------------------
-- Player
------------------------------------------------------------
receiveFromPlayerProcess
  :: (Serializable view, Show view, Serializable msg, Show msg, FromJSON msg)
  => SendPort (PubSubMsg view)
  -> SendPort view
  -> SendPort (SendPortId, msg)
  -> WS.Connection
  -> Process ()
receiveFromPlayerProcess txSubscribe sendToMe txGameMsg connection = do
  liftIO $ putStrLn "P: LISTENING"
  handle Nothing
  where
    handle :: Maybe UTCTime -> Process ()
    handle lastMessageHandled = do
      raw :: Either WS.ConnectionException WS.DataMessage <-
        liftIO . Ex.try $ WS.receiveDataMessage connection
      case raw of
        Left ex -> do
          liftIO . putStrLn $ "P: Socket has closed. Unsubscribing: " <> show ex
          sendChan txSubscribe (Unsub sendToMe)
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
                  sendChan txGameMsg (sendPortId sendToMe, msg)
                  handle (Just now)
                Just t ->
                  if addUTCTime timeBetweenCommands t < now
                    then do
                      sendChan txGameMsg (sendPortId sendToMe, msg)
                      handle (Just now)
                    else handle lastMessageHandled

timeBetweenCommands :: NominalDiffTime
timeBetweenCommands = 0.1

announceToPlayerProcess
  :: (Show view, Serializable view, ToJSON view)
  => WS.Connection -> ReceivePort view -> Process ()
announceToPlayerProcess connection rx =
  forever $
  do msg <- receiveChan rx
     liftIO $ WS.sendTextData connection (Aeson.encode msg)

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
gameProcess rxGameMsg txGameState updateFn viewFn = iterateM_ handle
  where
    handle game = do
      (replyTo, msg) <- receiveChan rxGameMsg
      liftIO . putStrLn $ "G: Heard: " <> show msg
      let newGame = updateFn (replyTo, msg) game
      sendChan txGameState (viewFn newGame)
      return newGame
