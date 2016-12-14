{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified EchoGame
import qualified Network.GameEngine
import qualified PresentDrop
import           System.Random

oldMain :: IO ()
oldMain = Network.GameEngine.runGame EchoGame.update EchoGame.view EchoGame.initialModel

main :: IO ()
main = do
  stdGen <- getStdGen
  Network.GameEngine.runGame
    PresentDrop.update
    PresentDrop.view
    (PresentDrop.initialModel stdGen)
