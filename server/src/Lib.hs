{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( runGame
  ) where
import           Control.Distributed.Process.Serializable
import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource
import qualified Data.ByteString.Lazy                     as LBS
import           Data.Monoid
import qualified Network.WebSockets                       as WS

import           Control.Distributed.Process
import           Data.Binary
import           Data.Typeable
import           GHC.Generics

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
       do conn <- liftIO $ WS.acceptRequest pendingConnection
          logInfoN $ "Joined"
          return conn)
      (\_ -> runStdoutLoggingT $ logInfoN "Leaves")
  forever $ liftIO $
    do msg <- WS.receiveDataMessage connection
       case msg of
         (WS.Text text) -> WS.sendTextData connection ("You said: " <> text)
         _ ->
           WS.sendDataMessage connection $
           WS.Text ("Sorry, I don't understand." :: LBS.ByteString)

runGame :: IO ()
runGame =
  runStdoutLoggingT $
  do logInfoN "START"
     server <-
       liftIO $
       WS.runServer "127.0.0.1" 8000 $ (runResourceT . acceptClientConnection)
     logInfoN "END"
