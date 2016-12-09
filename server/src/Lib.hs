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
-- TODO All the channel/process names are horrible.
------------------------------------------------------------
-- Websocket Server & Wiring.
------------------------------------------------------------
runGame :: IO ()
runGame =
  let host = "127.0.0.1"
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
               spawnLocal $ gameProcess update view receiveGameMsg txGameState initialGameState
             liftIO . WS.runServer host websocketPort $
               runResourceT . acceptClientConnection node sendGameMsg sendSubGameState
        logInfoN "END"

acceptClientConnection
  :: MonadResource m
  => LocalNode
  -> SendPort GameMsg
  -> SendPort (PubSubMsg GameState)
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
  :: SendPort (PubSubMsg GameState)
  -> SendPort GameState
  -> SendPort GameMsg
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
          sendChan txGameMsg $ GameMsg (LT.toStrict (LTE.decodeUtf8 text))
          handle

-- TODO Here we will encode the GameState to JSON.
announceToPlayerProcess
  :: (Show a, Serializable a)
  => WS.Connection -> ReceivePort a -> Process b
announceToPlayerProcess connection rx =
  forever $
  do msg <- receiveChan rx
     liftIO $ WS.sendTextData connection (T.pack (show msg))

------------------------------------------------------------
-- Broadcaster
------------------------------------------------------------
data PubSubMsg a
  = Sub (SendPort a)
  | Unsub (SendPort a)
  deriving (Show, Eq, Binary, Generic)

data BroadcasterMsg a
  = NewState a
  | Subscription (PubSubMsg a)
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
      liftIO . putStrLn $ "B: Broadcasting to: " <> show subscribers
      mergedPorts <-
        mergePortsRR
          [NewState <$> inboundGame, Subscription <$> subscriptionRequests]
      msg <- receiveChan mergedPorts
      case msg of
        NewState state -> do
          liftIO . putStrLn $ "B: I should broadcast: " <> show state
          liftIO . putStrLn $ "B: ...to: " <> show subscribers
          traverse_ (`sendChan` state) $ Set.toList subscribers
          return subscribers
        Subscription (Sub subscriber) -> do
          liftIO . putStrLn $ "B: adding: " <> show subscriber
          return (Set.insert subscriber subscribers)
        Subscription (Unsub subscriber) -> do
          liftIO . putStrLn $ "B: removing: " <> show subscriber
          return (Set.delete subscriber subscribers)

------------------------------------------------------------
-- Game
------------------------------------------------------------
gameProcess
  :: (Show msg, Serializable msg, Serializable a, Serializable b)
  => (msg -> a -> a)
  -> (a -> b)
  -> ReceivePort msg
  -> SendPort b
  -> a
  -> Process ()
gameProcess updateFn viewFn rxGameMsg txGameState = iterateM_ handle
  where
    handle game = do
      msg <- receiveChan rxGameMsg
      liftIO . putStrLn $ "G: Heard: " <> show msg
      let newGame = updateFn msg game
      sendChan txGameState (viewFn newGame)
      return newGame

------------------------------------------------------------
-- This Specific Game
------------------------------------------------------------
data GameMsg =
  GameMsg Text
  deriving (Show, Eq, Binary, Generic)

data GameState = GameState
  { lastMsg  :: Maybe GameMsg
  , msgCount :: Int
  } deriving (Show, Eq, Binary, Generic)

initialGameState :: GameState
initialGameState =
  GameState
  { lastMsg = Nothing
  , msgCount = 0
  }

update :: GameMsg -> GameState -> GameState
update msg state =
  GameState
  { lastMsg = Just msg
  , msgCount = msgCount state + 1
  }
view :: GameState -> GameState
view = id
