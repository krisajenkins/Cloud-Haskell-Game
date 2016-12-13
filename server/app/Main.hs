{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified EchoGame
import qualified Lib
import qualified PresentDrop
import           System.Random

oldMain :: IO ()
oldMain = Lib.runGame EchoGame.update EchoGame.view EchoGame.init

main :: IO ()
main = do
  stdGen <- getStdGen
  Lib.runGame PresentDrop.update PresentDrop.view (PresentDrop.init stdGen)
