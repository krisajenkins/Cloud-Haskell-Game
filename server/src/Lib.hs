{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( runGame
  ) where

import           Control.Distributed.Process
import           Control.Distributed.Process.Serializable
import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource
import           Data.Binary
import qualified Data.ByteString.Lazy                     as LBS
import           Data.Monoid
import qualified Data.Text.Lazy                           as LT
import qualified Data.Text.Lazy.Encoding                  as LTE
import           Data.Typeable
import           GHC.Generics
import qualified Network.WebSockets                       as WS

data PlayerMessage =
  Hello Int
  deriving (Show, Eq, Generic, Binary, Typeable)

instance Serializable PlayerMessage

-- main :: IO ()
-- main = do
--   [host, port] <- getArgs
--   backend <- initializeBackend host port initRemoteTable
--   node <- newLocalNode backend
--   peers <- findPeers backend 1000000
--   runProcess node $
--     forM_ peers $ \peer -> nsendRemote peer "echo-server" "hello!"
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
  runStdoutLoggingT $
  do logInfoN "START"
     liftIO $ WS.runServer "127.0.0.1" 8000 $ runResourceT . acceptClientConnection
