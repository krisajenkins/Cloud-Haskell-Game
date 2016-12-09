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

data Coords = Coords
  { _x :: Double
  , _y :: Double
  } deriving (Show, Eq, Binary, Generic, FromJSON)

data Player = Player
  { _position :: Coords
  , _score    :: Integer
  , _name     :: Text
  } deriving (Show, Eq, Binary, Generic)

data Model = Model
  { _players :: Map SendPortId Player
  , _radars  :: [Coords]
  , _present :: Coords
  } deriving (Show, Eq, Binary, Generic)

makeLenses ''Coords

makeLenses ''Player

makeLenses ''Model

instance ToJSON Coords where
  toJSON = genericToJSON $ aesonDrop 1 id

instance ToJSON Player where
  toJSON = genericToJSON $ aesonDrop 1 id

data View = View
  { viewPlayers        :: [Player]
  , viewRadars         :: [ViewRadar]
  , viewSampleCommands :: [Msg]
  } deriving (Show, Eq, Binary, Generic)

data ViewRadar = ViewRadar
  { viewRadarPosition :: Coords
  , viewRadarDistance :: Double
  } deriving (Show, Eq, Binary, Generic)

instance ToJSON View where
  toJSON = genericToJSON $ aesonDrop 4 camelCase

instance ToJSON ViewRadar where
  toJSON = genericToJSON $ aesonDrop 9 camelCase

init :: Model
init =
  Model
  { _players = Map.empty
  , _radars = [Coords (-10) (-15), Coords 3 (-5), Coords 12 3]
  , _present = Coords 5 3
  }

newPlayer :: Player
newPlayer =
  Player
  { _name = "<Your Name Here>"
  , _score = 0
  , _position = Coords 0 0
  }

data Msg
  = Join
  | Leave
  | SetName Text
  | Move Coords
  deriving (Show, Eq, Binary, Generic, FromJSON, ToJSON)

update :: (SendPortId, Msg) -> Model -> Model
update = handleMsg

distanceBetween :: Coords -> Coords -> Double
distanceBetween a b = sqrt $ (dx ^ (2 :: Int)) + (dy ^ (2 :: Int))
  where
    dx = Lens.view x a - Lens.view x b
    dy = Lens.view y a - Lens.view y b

handleMsg :: (SendPortId, Msg) -> Model -> Model
handleMsg (playerId, Join) model =
  set (players . at playerId) (Just newPlayer) model
handleMsg (playerId, Leave) model = set (players . at playerId) Nothing model
handleMsg (playerId, SetName newName) model =
  set (players . ix playerId . name) newName model
handleMsg (playerId, Move moveTo) model =
  over (players . ix playerId . position) updatePosition model
  where
    updatePosition = over x ((+) dx) . over y ((+) dy)
    dx = max (-1) $ min 1 $ Lens.view x moveTo
    dy = max (-1) $ min 1 $ Lens.view y moveTo

view :: Model -> View
view model =
  View
  { viewPlayers = toListOf (players . traverse) model
  , viewRadars = viewRadar (Lens.view present model) <$> Lens.view radars model
  , viewSampleCommands = [Join, Leave, SetName "Kris", Move $ Coords 1.2 5.3]
  }

viewRadar :: Coords -> Coords -> ViewRadar
viewRadar presentPosition radar =
  ViewRadar
  { viewRadarPosition = radar
  , viewRadarDistance = distanceBetween presentPosition radar
  }
