{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Lib where

import           Control.Distributed.Process
import           Control.Distributed.Process.Closure
import           Control.Distributed.Process.Node
import           Control.Distributed.Process.Serializable
import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource
import           Data.Binary
import           Data.Monoid
import qualified Data.Set                                 as Set
import           Data.Text                                (Text)
import qualified Data.Text                                as T
import qualified Data.Text.Lazy                           as LT
import qualified Data.Text.Lazy.Encoding                  as LTE
import           Data.Typeable
import           GHC.Generics
import           Network.Transport.InMemory
import qualified Network.WebSockets                       as WS

data PlayerMessage =
  Hello Int
  deriving (Show, Eq, Generic, Binary, Typeable)

instance Serializable PlayerMessage

-- TODO Disconnections don't detatch from the broadcaster, which sounds wrong.

------------------------------------------------------------
data Echo =
  Echo String
  deriving (Show, Binary, Generic)

echoProcess :: ReceivePort Echo -> Process ()
echoProcess receiver = do
  liftIO $ putStrLn "ECHO SERVER STARTING"
  self <- getSelfPid -- get our own process id
  liftIO . putStrLn $ "ECHO SERVER IS: " <> show self
  forever $
    do msg <- receiveChan receiver
       liftIO . putStrLn $ "ECHO Process Heard: " <> show msg

------------------------------------------------------------
remotable []

myRemoteTable :: RemoteTable
myRemoteTable = __remoteTable initRemoteTable

------------------------------------------------------------
acceptClientConnection
  :: MonadResource m
  => LocalNode
  -> SendPort Echo
  -> SendPort GameMsg
  -> SendPort (SendPort GameState)
  -> WS.PendingConnection
  -> m ()
acceptClientConnection node echoProcessChannel txGameMsg txSubscribe pendingConnection = do
  (_releaseKey, connection) <-
    allocate
      (runStdoutLoggingT $
       do logInfoN "New connection received."
          liftIO $ WS.acceptRequest pendingConnection)
      (\_ -> runStdoutLoggingT $ logInfoN "Leaves")
  void . liftIO . runProcess node $
    do _ <- spawnLocal $ playerMsgProcess echoProcessChannel txGameMsg connection
       (sendToMe, receiveFromBroadcaster) <- newChan
       sendChan txSubscribe sendToMe
       announceProcess connection receiveFromBroadcaster

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
          do (sendToEcho, receiveFromEcho) <- newChan
             (sendSubGameState, receiveSubGameState) <- newChan
             (txGameState :: SendPort GameState, rxGameState :: ReceivePort GameState) <-
               newChan
             (sendGameMsg, receiveGameMsg) <- newChan
             _ <- spawnLocal $ echoProcess receiveFromEcho
             _ <- spawnLocal $ broadcastProcess rxGameState receiveSubGameState
             _ <-
               spawnLocal $
               gameProcess
                 receiveGameMsg
                 txGameState
                 GameState
                 { lastMsg = Nothing
                 , msgCount = 0
                 }
             liftIO . WS.runServer host websocketPort $
               runResourceT .
               acceptClientConnection node sendToEcho sendGameMsg sendSubGameState
        logInfoN "END"

playerMsgProcess :: SendPort Echo
                 -> SendPort GameMsg
                 -> WS.Connection
                 -> Process ()
playerMsgProcess echoProcessPort txGameMsg connection = do
  liftIO $ putStrLn "LISTENING"
  sendChan echoProcessPort (Echo "Hello Echo Service I Am A Client")
  forever $
    do raw <- liftIO $ WS.receiveDataMessage connection
       case raw of
         WS.Text text -> do
           liftIO . putStrLn $ "HEARD: " <> show text
           let echo = Echo (show text)
           liftIO . putStrLn $ "SENDING TO ECHO: " <> show echo
           sendChan echoProcessPort echo
           sendChan txGameMsg $ GameMsg (LT.toStrict (LTE.decodeUtf8 text))
         WS.Binary _ -> return ()

announceProcess :: WS.Connection -> ReceivePort GameState -> Process ()
announceProcess connection rx =
  forever $
  do msg <- receiveChan rx
     liftIO $ WS.sendTextData connection (T.pack (show msg))

data BroadcasterMsg a
  = NewState a
  | Subscribe (SendPort a)
  deriving (Show, Eq, Binary, Generic)

broadcastProcess :: ReceivePort GameState
                 -> ReceivePort (SendPort GameState)
                 -> Process ()
broadcastProcess inboundGame subscriptionRequests = loop Set.empty
  where
    loop subscribers = do
      liftIO . putStrLn $ "B: Broadcasting to: " <> show subscribers
      mergedPorts <-
        mergePortsRR
          [NewState <$> inboundGame, Subscribe <$> subscriptionRequests]
      msg :: BroadcasterMsg GameState <- receiveChan mergedPorts
      case msg of
        NewState state -> do
          liftIO . putStrLn $ "B: I should broadcast: " <> show state
          liftIO . putStrLn $ "B: ...to: " <> show subscribers
          mapM_ (`sendChan` state) $ Set.toList subscribers
          loop subscribers
        Subscribe newSubscriber -> do
          liftIO . putStrLn $ "B: adding: " <> show newSubscriber
          loop (Set.insert newSubscriber subscribers)

data GameMsg =
  GameMsg Text
  deriving (Show, Eq, Binary, Generic)

data GameState = GameState
  { lastMsg  :: Maybe GameMsg
  , msgCount :: Int
  } deriving (Show, Eq, Binary, Generic)

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
