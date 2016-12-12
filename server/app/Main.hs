{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Lib
import qualified PresentDrop
import           System.Random

main :: IO ()
main = do
  stdGen <- getStdGen
  Lib.runGame PresentDrop.update PresentDrop.view (PresentDrop.init stdGen)
