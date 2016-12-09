{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson  as Aeson
import qualified EchoGame
import qualified Lib
import qualified PresentDrop

oldMain :: IO ()
oldMain = Lib.runGame EchoGame.update EchoGame.view EchoGame.init

main :: IO ()
main = Lib.runGame PresentDrop.update PresentDrop.view PresentDrop.init
