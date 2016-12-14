{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified EchoGame
import qualified Network.GameEngine
import qualified PresentDrop
import           System.Random

oldMain :: IO ()
oldMain =
  Network.GameEngine.runGame EchoGame.initialModel EchoGame.update EchoGame.view

main :: IO ()
main = do
  stdGen <- getStdGen
  Network.GameEngine.runGame
    (PresentDrop.initialModel stdGen)
    PresentDrop.update
    PresentDrop.view
