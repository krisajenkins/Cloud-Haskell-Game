{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( runGame
  ) where

import           Control.Distributed.Process
import           Control.Distributed.Process.Debug
import           Control.Distributed.Process.Node
import           Control.Distributed.Process.Serializable
import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource
import           Data.Binary
import qualified Data.ByteString.Lazy                     as LBS
import           Data.Monoid
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

acceptClientConnection
  :: MonadResource m
  => WS.PendingConnection -> m ()
acceptClientConnection pendingConnection = do
  (_releaseKey, connection) <-
    allocate
      (runStdoutLoggingT $
       do logInfoN "New connection received."
          liftIO $ WS.acceptRequest pendingConnection)
      (\_ -> runStdoutLoggingT $ logInfoN "Leaves")
  forever $
    runStdoutLoggingT $
    do msg <- liftIO $ WS.receiveDataMessage connection
       case msg of
         (WS.Text text) -> do
           logInfoN $ "Received: " <> LT.toStrict (LTE.decodeUtf8 text)
           liftIO $ WS.sendTextData connection ("You said: " <> text)
         _ -> do
           logInfoN "Received binary data."
           liftIO $
             WS.sendTextData
               connection
               ("Sorry, I don't understand." :: LBS.ByteString)

runGame :: IO ()
runGame =
  let host = "127.0.0.1"
      websocketPort = 8000
  in runStdoutLoggingT $
     do logInfoN "START"
        logInfoN "Booting Cloud Haskell"
        backend <- liftIO $ createTransport
        node <- liftIO $ newLocalNode backend initRemoteTable
        logInfoN "Created Node"
        processId :: ProcessId <- liftIO $ forkProcess node simpleProcess
        logInfoN $ "Forked process: " <> T.pack (show processId)
        logInfoN "----"
        logInfoN "Starting Websockets"
        liftIO $ WS.runServer host websocketPort $ runResourceT . acceptClientConnection
        logInfoN "END"

simpleProcess :: Process ()
simpleProcess = do
  self <- getSelfPid -- get our own process id
  enableTrace self
  send self ("hello" :: String)
  hello <- expect :: Process String
  liftIO $ putStrLn hello
