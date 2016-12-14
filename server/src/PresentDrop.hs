{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module PresentDrop
  ( initialModel
  , update
  , view
  ) where

import Control.Distributed.Process
import Control.Lens (at, ix, makeLenses, over, set, toListOf)
import qualified Control.Lens as Lens
import Data.Aeson
import Data.Aeson.Casing
import Data.Binary
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Generics
import Network.GameEngine
import System.Random

data Coords = Coords
  { _x :: Double
  , _y :: Double
  } deriving (Show, Eq, Binary, Generic)

data Player = Player
  { _position :: Coords
  , _score :: Integer
  , _name :: Text
  , _color :: Text
  } deriving (Show, Eq, Binary, Generic)

data Gps = Gps
  { _gpsPosition :: Coords
  , _variance :: Double
  } deriving (Show, Eq, Binary, Generic)

data Model = Model
  { _players :: Map SendPortId Player
  , _gpss :: [Gps]
  , _present :: Coords
  , _rng :: StdGen
  } deriving (Show)

makeLenses ''Coords

makeLenses ''Player

makeLenses ''Gps

makeLenses ''Model

instance FromJSON Coords where
  parseJSON = genericParseJSON $ aesonDrop 1 camelCase

instance ToJSON Coords where
  toJSON = genericToJSON $ aesonDrop 1 camelCase

instance ToJSON Player where
  toJSON = genericToJSON $ aesonDrop 1 camelCase

data View = View
  { viewPlayers :: [Player]
  , viewGpss :: [ViewGps]
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
  ------------------------------------------------------------

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

distanceBetween :: Coords -> Coords -> Double
distanceBetween a b = hypotenuse dx dy
  where
    dx = Lens.view x a - Lens.view x b
    dy = Lens.view y a - Lens.view y b

randomPair
  :: (RandomGen g, Random a)
  => (a, a) -> g -> ((a, a), g)
randomPair range stdGen = ((a, b), stdGen'')
  where
    (a, stdGen') = randomR range stdGen
    (b, stdGen'') = randomR range stdGen'

------------------------------------------------------------
initialModel :: StdGen -> Model
initialModel stdGen =
  Model
  { _players = Map.empty
  , _gpss =
    [ Gps
      { _gpsPosition = Coords (-10) (-8)
      , _variance = 0
      }
    , Gps
      { _gpsPosition = Coords 12 (-5)
      , _variance = 0
      }
    , Gps
      { _gpsPosition = Coords (-4) 9
      , _variance = 0
      }
    ]
  , _present = Coords 5 3
  , _rng = stdGen
  }

newPlayer :: Player
newPlayer =
  Player
  { _name = "<Your Name Here>"
  , _score = 0
  , _position = Coords 0 0
  , _color = "white"
  }

data Msg
  = SetName Text
  | SetColor Text
  | Move Coords
  deriving (Show, Eq, Binary, Generic, FromJSON, ToJSON)

update :: EngineMsg Msg -> Model -> Model
update msg = handleWin . handleMsg msg

handleWin :: Model -> Model
handleWin model =
  if null overlappingPlayers
    then model
    else let withIncrementedScores aModel =
               Map.foldlWithKey
                 (\aModel' portId _ ->
                     over (players . ix portId . score) (+ 1) aModel')
                 aModel
                 overlappingPlayers
         in movePresent . withIncrementedScores $ model
  where
    overlappingPlayers :: Map SendPortId Player
    overlappingPlayers = Map.filter inRange (Lens.view players model)
    inRange :: Player -> Bool
    inRange player =
      distanceBetween (Lens.view present model) (Lens.view position player) < 1

movePresent :: Model -> Model
movePresent model = set rng newRng $ set present (Coords newX newY) model
  where
    ((newX, newY), newRng) = randomPair (-10, 10) $ Lens.view rng model

handleMsg :: EngineMsg Msg -> Model -> Model
handleMsg (Join playerId) = set (players . at playerId) (Just newPlayer)
handleMsg (Leave playerId) = set (players . at playerId) Nothing
handleMsg (GameMsg playerId (SetName newName)) =
  set (players . ix playerId . name) newName
handleMsg (GameMsg playerId (SetColor text)) =
  set (players . ix playerId . color) text
handleMsg (GameMsg playerId (Move moveTo)) =
  over (players . ix playerId . position) updatePosition
  where
    updatePosition = over x (dx +) . over y (dy +)
    (dx, dy) = normalise (Lens.view x moveTo, Lens.view y moveTo)

view :: Model -> View
view model =
  View
  { viewPlayers = toListOf (players . traverse) model
  , viewGpss = viewGps (Lens.view present model) <$> Lens.view gpss model
  , viewSampleCommands =
    [SetName "Kris", SetColor "#ff0000", Move $ Coords 1.0 (-2.0)]
  }

viewGps :: Coords -> Gps -> ViewGps
viewGps presentPosition gps =
  ViewGps
  { viewGpsPosition = Lens.view gpsPosition gps
  , viewGpsDistance =
    distanceBetween presentPosition (Lens.view gpsPosition gps) +
    Lens.view variance gps
  }
