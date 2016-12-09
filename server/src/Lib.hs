{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import           Control.Distributed.Process
import           Control.Distributed.Process.Debug
import           Control.Distributed.Process.Node
import           Control.Distributed.Process.Serializable
import qualified Control.Exception                        as Ex
import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Loops
import           Control.Monad.Trans.Resource
import           Data.Aeson                               (ToJSON)
import qualified Data.Aeson                               as Aeson
import           Data.Binary
import           Data.Foldable
import           Data.Monoid
import qualified Data.Set                                 as Set
import           Data.Text                                (Text)
import qualified Data.Text                                as T
import qualified Data.Text.Lazy                           as LT
import qualified Data.Text.Lazy.Encoding                  as LTE
import           GHC.Generics
import           Network.Transport.InMemory
import qualified Network.WebSockets                       as WS

-- TODO Could really do with some monitoring.
-- TODO JSON Handling.
-- TODO All the channel/process names are horrible.
------------------------------------------------------------
-- Websocket Server & Wiring.
------------------------------------------------------------
runGame
  :: (Serializable state, Serializable view, Show view, ToJSON view)
  => (Text -> state -> state) -> (state -> view) -> state -> IO ()
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
  :: (MonadResource m, Serializable state, Show state,ToJSON state)
  => LocalNode
  -> SendPort Text
  -> SendPort (PubSubMsg state)
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
-- TODO Here we will decode GameMsgs from the JSON.
receiveFromPlayerProcess
  :: (Serializable state, Show state)
  => SendPort (PubSubMsg state)
  -> SendPort state
  -> SendPort Text
  -> WS.Connection
  -> Process ()
receiveFromPlayerProcess txSubscribe sendToMe txGameMsg connection = do
  liftIO $ putStrLn "P: LISTENING"
  handle
  where
    handle = do
      raw :: Either WS.ConnectionException WS.DataMessage <-
        liftIO . Ex.try $ WS.receiveDataMessage connection
      case raw of
        Left ex -> do
          liftIO . putStrLn $ "P: Socket has closed. Unsubscribing: " <> show ex
          sendChan txSubscribe (Unsub sendToMe)
        Right (WS.Binary _) -> handle
        Right (WS.Text text) -> do
          liftIO . putStrLn $ "P: HEARD: " <> show text
          sendChan txGameMsg $ LT.toStrict (LTE.decodeUtf8 text)
          handle

-- TODO Here we will encode the GameState to JSON.
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
data PubSubMsg a
  = Sub (SendPort a)
  | Unsub (SendPort a)
  deriving (Show, Eq, Binary, Generic)

broadcastProcess
  :: (Show a, Serializable a)
  => ReceivePort a -> ReceivePort (PubSubMsg a) -> Process b
broadcastProcess inboundGame subscriptionRequests = do
  setTraceFlags $
    defaultTraceFlags
    { traceSpawned = traceOn
    , traceRecv = traceOn
    }
  iterateM_ handle Set.empty
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
  :: (Show msg, Serializable msg, Serializable view)
  => ReceivePort msg
  -> SendPort view
  -> (msg -> state -> state)
  -> (state -> view)
  -> state
  -> Process ()
gameProcess rxGameMsg txGameState updateFn viewFn = iterateM_ handle
  where
    handle game = do
      msg <- receiveChan rxGameMsg
      liftIO . putStrLn $ "G: Heard: " <> show msg
      let newGame = updateFn msg game
      sendChan txGameState (viewFn newGame)
      return newGame
