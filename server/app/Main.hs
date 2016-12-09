module Main where

import qualified EchoGame
import qualified Lib

main :: IO ()
main = Lib.runGame EchoGame.update EchoGame.view EchoGame.init
