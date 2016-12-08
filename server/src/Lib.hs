{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Lib where

import           Control.Distributed.Process
import           Control.Distributed.Process.Node
import           Control.Distributed.Process.Serializable
import           Control.Monad
import           Control.Monad.Logger
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

-- TODO All the channel/process names are horrible.
-- TODO Disconnections don't detatch from the broadcaster, which sounds wrong.
-- TODO Could really do with some monitoring.

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
             _ <- spawnLocal $ gameProcess receiveGameMsg txGameState initialGameState
             liftIO . WS.runServer host websocketPort $
               runResourceT . acceptClientConnection node sendGameMsg sendSubGameState
        logInfoN "END"

acceptClientConnection
  :: MonadResource m
  => LocalNode
  -> SendPort GameMsg
  -> SendPort (SendPort GameState)
  -> WS.PendingConnection
  -> m ()
acceptClientConnection node txGameMsg txSubscribe pendingConnection = do
  (_releaseKey, connection) <-
    allocate
      (runStdoutLoggingT $
       do logInfoN "New connection received."
          liftIO $ WS.acceptRequest pendingConnection)
      (\_ -> runStdoutLoggingT $ logInfoN "Leaves")
  liftIO . runProcess node $
    do _ <- spawnLocal $ receiveFromPlayerProcess txGameMsg connection
       (sendToMe, receiveFromBroadcaster) <- newChan
       sendChan txSubscribe sendToMe
       announceToPlayerProcess connection receiveFromBroadcaster

-- TODO Here we will decode GameMsgs from the JSON.
receiveFromPlayerProcess :: SendPort GameMsg -> WS.Connection -> Process ()
receiveFromPlayerProcess txGameMsg connection = do
  liftIO $ putStrLn "LISTENING"
  forever $
    do raw <- liftIO $ WS.receiveDataMessage connection
       case raw of
         WS.Binary _ -> return ()
         WS.Text text -> do
           liftIO . putStrLn $ "HEARD: " <> show text
           sendChan txGameMsg $ GameMsg (LT.toStrict (LTE.decodeUtf8 text))

-- TODO Here we will encode the GameState to JSON.
announceToPlayerProcess
  :: (Show a, Serializable a)
  => WS.Connection -> ReceivePort a -> Process b
announceToPlayerProcess connection rx =
  forever $
  do msg <- receiveChan rx
     liftIO $ WS.sendTextData connection (T.pack (show msg))

data BroadcasterMsg a
  = NewState a
  | Subscribe (SendPort a)
  deriving (Show, Eq, Binary, Generic)

broadcastProcess
  :: (Show a, Serializable a)
  => ReceivePort a -> ReceivePort (SendPort a) -> Process b
broadcastProcess inboundGame subscriptionRequests = loop Set.empty
  where
    loop subscribers = do
      liftIO . putStrLn $ "B: Broadcasting to: " <> show subscribers
      mergedPorts <-
        mergePortsRR
          [NewState <$> inboundGame, Subscribe <$> subscriptionRequests]
      msg <- receiveChan mergedPorts
      case msg of
        NewState state -> do
          liftIO . putStrLn $ "B: I should broadcast: " <> show state
          liftIO . putStrLn $ "B: ...to: " <> show subscribers
          traverse_ (`sendChan` state) $ Set.toList subscribers
          loop subscribers
        Subscribe newSubscriber -> do
          liftIO . putStrLn $ "B: adding: " <> show newSubscriber
          loop (Set.insert newSubscriber subscribers)

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

-- TODO Here we could split out a pure handler function.
gameProcess :: ReceivePort GameMsg
            -> SendPort GameState
            -> GameState
            -> Process ()
gameProcess rxGameMsg txGameState = loop
  where
    loop game = do
      msg <- receiveChan rxGameMsg
      liftIO . putStrLn $ "G: Heard: " <> show msg
      let newGame =
            GameState
            { lastMsg = Just msg
            , msgCount = msgCount game + 1
            }
      sendChan txGameState newGame
      loop newGame
