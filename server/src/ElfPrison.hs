{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module ElfPrison
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

data Play
  = Betray
  | Cooperate
  deriving (Show, Eq, Binary, Generic, FromJSON, ToJSON)

data PlayerPlays = PlayerPlays
  { _north :: Maybe Play
  , _east  :: Maybe Play
  , _south :: Maybe Play
  , _west  :: Maybe Play
  } deriving (Show, Eq, Binary, Generic)

makeLenses ''PlayerPlays

instance ToJSON PlayerPlays where
  toJSON = genericToJSON $ aesonDrop 1 camelCase

data MatchResult = MatchResult
  { matchResultNorth :: Maybe (Text, Play)
  , matchResultEast  :: Maybe (Text, Play)
  , matchResultSouth :: Maybe (Text, Play)
  , matchResultWest  :: Maybe (Text, Play)
  } deriving (Show, Eq, Binary, Generic)

instance ToJSON MatchResult  where
  toJSON = genericToJSON $ aesonPrefix camelCase

data Player = Player
  { _score     :: Integer
  , _name      :: Text
  , _color     :: Text
  , _plays     :: PlayerPlays
  , _lastRound :: Maybe MatchResult
  } deriving (Show, Eq, Binary, Generic)

makeLenses ''Player

instance ToJSON Player where
  toJSON = genericToJSON $ aesonDrop 1 camelCase

data Model = Model
  { _players :: Map PlayerId Player
  , _rng     :: StdGen
  } deriving (Show)

makeLenses ''Model

data GlobalView = GlobalView
  { viewPlayers        :: [Player]
  , viewSampleCommands :: [Msg]
  } deriving (Show, Eq, Binary, Generic)

instance ToJSON GlobalView where
  toJSON = genericToJSON $ aesonDrop 4 camelCase

data PlayerView = PlayerView
  { viewNorth     :: Maybe Text
  , viewSouth     :: Maybe Text
  , viewEast      :: Maybe Text
  , viewWest      :: Maybe Text
  , viewLastRound :: Maybe MatchResult
  } deriving (Show, Eq, Binary, Generic)

instance ToJSON PlayerView where
  toJSON = genericToJSON $ aesonDrop 4 camelCase

data Msg
  = SetName Text
  | SetColor Text
  | MakeChoice Play Direction
  deriving (Show, Eq, Binary, Generic, FromJSON, ToJSON)

data Direction
  = North
  | East
  | South
  | West
  deriving (Show, Eq, Binary, Generic, FromJSON, ToJSON)

------------------------------------------------------------

initialModel :: StdGen -> Model
initialModel stdGen =
  Model
  { _players = Map.empty
  , _rng = stdGen
  }

newPlayer :: Player
newPlayer =
  Player
  { _name = "<Your Name Here>"
  , _score = 0
  , _color = "white"
  , _plays = emptyPlays
  , _lastRound = Nothing
  }

emptyPlays :: PlayerPlays
emptyPlays =
  PlayerPlays
  {_north = Nothing, _east = Nothing, _south = Nothing, _west = Nothing}

update :: EngineMsg Msg -> Model -> Model
update (Join playerId) = set (players . at playerId) (Just newPlayer)
update (Leave playerId) = set (players . at playerId) Nothing
update GameTick = id
update (GameMsg playerId (SetName newName)) =
  set (players . ix playerId . name) newName
update (GameMsg playerId (SetColor text)) =
  set (players . ix playerId . color) text
update (GameMsg playerId (MakeChoice play dir)) = id

globalView :: Model -> GlobalView
globalView model =
  GlobalView
  { viewPlayers = toListOf (players . traverse) model
  , viewSampleCommands =
      [ SetName "Kris"
      , SetColor "#ff0000"
      , MakeChoice Betray North
      , MakeChoice Cooperate West
      ]
  }

playerView :: Model -> Player -> PlayerView
playerView model playerId = PlayerView Nothing Nothing Nothing Nothing Nothing

view :: Model -> (GlobalView, Map PlayerId PlayerView)
view model = (globalView model, playerView model <$> _players model)
