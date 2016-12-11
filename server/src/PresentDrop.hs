{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module PresentDrop where

import           Control.Distributed.Process
import           Control.Lens                (at, ix, makeLenses, over, set,
                                              toListOf)
import qualified Control.Lens                as Lens
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Binary
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
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
  , _gpss    :: [Coords]
  , _present :: Coords
  } deriving (Show, Eq, Binary, Generic)

makeLenses ''Coords

makeLenses ''Player

makeLenses ''Model

instance ToJSON Coords where
  toJSON = genericToJSON $ aesonDrop 1 camelCase

instance ToJSON Player where
  toJSON = genericToJSON $ aesonDrop 1 camelCase

data View = View
  { viewPlayers        :: [Player]
  , viewGpss           :: [ViewGps]
  , viewSampleCommands :: [Msg]
  } deriving (Show, Eq, Binary, Generic)

data ViewGps = ViewGps
  { viewGpsPosition :: Coords
  , viewGpsDistance :: Double
  } deriving (Show, Eq, Binary, Generic)

instance ToJSON View where
  toJSON = genericToJSON $ aesonPrefix camelCase

instance ToJSON ViewGps where
  toJSON = genericToJSON $ aesonDrop 7 camelCase

init :: Model
init =
  Model
  { _players = Map.empty
  , _gpss = [Coords (-10) (-15), Coords 3 (-5), Coords 12 3]
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
update msg = handleWin . handleMsg msg

handleWin :: Model -> Model
handleWin model =
  if null overlappingPlayers
    then model
    else let modelWithIncrementedScores =
               Map.foldlWithKey
                 (\model' portId _ -> over (players . ix portId . score) (+ 1) model')
                 model
                 overlappingPlayers
             modelWithScoresAndMovesPresent =
               over present randomisePresentPosition modelWithIncrementedScores
             randomisePresentPosition :: Coords -> Coords
             randomisePresentPosition (Coords {_x
                                              ,_y}) = Coords _y _x -- TODO This is rubbish randomisation!
         in modelWithScoresAndMovesPresent
  where
    overlappingPlayers :: Map SendPortId Player
    overlappingPlayers = Map.filter inRange (Lens.view players model)
    inRange :: Player -> Bool
    inRange player =
      distanceBetween (Lens.view present model) (Lens.view position player) < 0.5


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
  , viewGpss = viewGps (Lens.view present model) <$> Lens.view gpss model
  , viewSampleCommands = [Join, Leave, SetName "Kris", Move $ Coords 1.0 (-2.0)]
  }

viewGps :: Coords -> Coords -> ViewGps
viewGps presentPosition gps =
  ViewGps
  { viewGpsPosition = gps
  , viewGpsDistance = distanceBetween presentPosition gps
  }
