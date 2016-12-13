{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified EchoGame
import qualified Lib
import qualified PresentDrop
import           System.Random

oldMain :: IO ()
oldMain = Lib.runGame EchoGame.update EchoGame.view EchoGame.initialModel

main :: IO ()
main = do
  stdGen <- getStdGen
  Lib.runGame
    PresentDrop.update
    PresentDrop.view
    (PresentDrop.initialModel stdGen)
