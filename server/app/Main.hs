module Main where

import qualified ElfPrison
import qualified Network.GameEngine
import           System.Random

main :: IO ()
main = do
  stdGen <- getStdGen
  Network.GameEngine.runGame
    (ElfPrison.initialModel stdGen)
    ElfPrison.update
    ElfPrison.view
