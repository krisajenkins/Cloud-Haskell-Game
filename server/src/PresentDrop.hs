{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module PresentDrop where

import           Control.Distributed.Process
import           Control.Lens                as Lens
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Binary
import           Data.Map.Lazy               (Map)
import qualified Data.Map.Lazy               as Map
import           Data.Text                   (Text)
import           GHC.Generics

------------------------------------------------------------
-- This Specific Game
------------------------------------------------------------
data Coords = Coords
  { _x :: Float
  , _y :: Float
  } deriving (Show, Eq, Binary, Generic)

data Player = Player
  { _position :: Coords
  , _score    :: Integer
  , _name     :: Text
  } deriving (Show, Eq, Binary, Generic)

data Model = Model
  { _players :: Map SendPortId Player
  } deriving (Show, Eq, Binary, Generic)

makeLenses ''Player

makeLenses ''Model

instance ToJSON Coords where
  toJSON = genericToJSON $ aesonDrop 1 id

instance ToJSON Player where
  toJSON = genericToJSON $ aesonDrop 1 id

type View = [Player]

init :: Model
init =
  Model
  { _players = Map.empty
  }

newPlayer :: Player
newPlayer =
  Player
  { _name = "<Your Name Here>"
  ,_score = 0
  , _position =
    Coords
    { _x = 0
    , _y = 0
    }
  }

data Msg
  = Join
  | Leave
  | SetName Text
  deriving (Show, Eq, Binary, Generic, FromJSON, ToJSON)

update :: (SendPortId, Msg) -> Model -> Model
update (playerId, Join) model = set (players . at playerId) (Just newPlayer) model
update (playerId, Leave) model = set (players . at playerId) Nothing model
update (playerId, SetName newName) model =
  set (players . ix playerId . name) newName model

view :: Model -> View
view model = Lens.toListOf (players . traverse) model
