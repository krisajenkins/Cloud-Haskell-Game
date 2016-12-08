{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Lib where

import           Control.Concurrent                       (myThreadId,
                                                           threadDelay)
import           Control.Distributed.Process
import qualified Data.Text                                as T
--import           Control.Distributed.Process.Debug
import           Control.Distributed.Process.Closure
import           Control.Distributed.Process.Node
import           Control.Distributed.Process.Serializable
import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource
import           Data.Binary

-- import qualified Data.ByteString.Lazy                     as LBS
import           Data.Monoid

-- import qualified Data.Text.Lazy                           as LT
-- import qualified Data.Text.Lazy.Encoding                  as LTE
import           Data.Typeable
import           GHC.Generics
import           Network.Transport.InMemory

--import           Network.Transport.TCP
import qualified Network.WebSockets                       as WS

data PlayerMessage =
  Hello Int
  deriving (Show, Eq, Generic, Binary, Typeable)

instance Serializable PlayerMessage

------------------------------------------------------------
data Echo =
  Echo String
  deriving (Show, Binary, Generic)

echoProcess :: Process ()
echoProcess = do
  liftIO $ putStrLn "ECHO SERVER STARTING"
  self <- getSelfPid -- get our own process id
  liftIO . putStrLn $ "ECHO SERVER IS: " <> show self
  forever $
    do msg <- expect :: Process Echo
       liftIO . putStrLn $ "ECHO: " <> show msg

------------------------------------------------------------
remotable []

myRemoteTable :: RemoteTable
myRemoteTable = __remoteTable initRemoteTable

------------------------------------------------------------
acceptClientConnection
  :: MonadResource m
  => LocalNode -> ProcessId -> WS.PendingConnection -> m ()
acceptClientConnection node echoProcessId pendingConnection = do
  (_releaseKey, connection) <-
    allocate
      (runStdoutLoggingT $
       do logInfoN "New connection received."
          liftIO $ WS.acceptRequest pendingConnection)
      (\_ -> runStdoutLoggingT $ logInfoN "Leaves")
  void $
    liftIO $
    do myId <- myThreadId
       putStrLn ("My Thread: " ++ show myId)
       runProcess node $
         do _ <- spawnLocal $ listenProcess echoProcessId connection
            announceProcess connection

runGame :: IO ()
runGame =
  let host = "127.0.0.1"
      websocketPort = 9000
  in runStdoutLoggingT $
     do logInfoN "START"
        logInfoN "Booting Cloud Haskell"
        backend <- liftIO $ createTransport
        logInfoN "Creating Node"
        node <- liftIO $ newLocalNode backend initRemoteTable
        logInfoN "Forking listener."
        _ <-
          liftIO $
          runProcess node $
          do echoProcessId <- spawnLocal $ echoProcess
             liftIO $
               WS.runServer host websocketPort $
               runResourceT . acceptClientConnection node echoProcessId
        logInfoN "END"

listenProcess :: ProcessId -> WS.Connection -> Process ()
listenProcess echoProcessId connection = do
  liftIO $ putStrLn "LISTENING"
  send echoProcessId (Echo "Hello Echo Service I Am A Client")
  forever $
    do raw <- liftIO $ WS.receiveDataMessage connection
       case raw of
         WS.Text text -> do
           liftIO $ putStrLn $ "HEARD: " <> show text
           let echo = Echo (show text)
           liftIO $ putStrLn ("SENDING TO ECHO: " <> show echo)
           send echoProcessId echo
         WS.Binary _ -> return ()

announceProcess ::  WS.Connection -> Process ()
announceProcess connection =
  forever $
  liftIO $
  do threadDelay (1000 * 1000 * 2)
     WS.sendTextData connection ("Hello You!" :: T.Text)
