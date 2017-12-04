{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Position where

import GHC.Generics
import Data.Binary

data Position = Position
  { _x :: Double
  , _y :: Double
  } deriving (Show, Eq, Binary, Generic)
