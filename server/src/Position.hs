{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
module Position where

import Data.Aeson (FromJSON,ToJSON,parseJSON,toJSON,genericParseJSON,genericToJSON)
import Data.Aeson.Casing (aesonDrop,camelCase)
import GHC.Generics (Generic)
import Data.Binary (Binary)
import Control.Lens (makeLenses, view)

data Position = Position
  { _x :: Double
  , _y :: Double
  } deriving (Show, Eq, Binary, Generic)

makeLenses ''Position

instance FromJSON Position where
  parseJSON = genericParseJSON $ aesonDrop 1 camelCase

instance ToJSON Position where
  toJSON = genericToJSON $ aesonDrop 1 camelCase

distanceBetween :: Position -> Position -> Double
distanceBetween a b = hypotenuse dx dy
  where
    dx = view x a - view x b
    dy = view y a - view y b

hypotenuse
  :: Floating r
  => r -> r -> r
hypotenuse dx dy = sqrt $ (dx ^ (2 :: Int)) + (dy ^ (2 :: Int))

normalise
  :: (Floating r, Ord r)
  => (r, r) -> (r, r)
normalise (dx, dy) =
  if h <= 1
    then (dx, dy)
    else (dx / h, dy / h)
  where
    h = hypotenuse dx dy
