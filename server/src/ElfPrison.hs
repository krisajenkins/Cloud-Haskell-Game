{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module ElfPrison where

import Control.Arrow ((>>>))
import Control.Distributed.Process
import Control.Lens (at, ix, makeLenses, over, set, toListOf)
import qualified Control.Lens as Lens
import Data.Aeson
import Data.Aeson.Casing
import Data.Binary
import Data.Function ((&))
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Generics
import Network.GameEngine
import System.Random

type Position = (Int, Int)

data Direction
  = North
  | East
  | South
  | West
  deriving (Show, Eq, Binary, Generic, FromJSON, ToJSON)

data Play
  = Betray
  | Cooperate
  deriving (Show, Eq, Binary, Generic, FromJSON, ToJSON)

data PlayerPlays = PlayerPlays
  { _north :: Maybe Play
  , _east :: Maybe Play
  , _south :: Maybe Play
  , _west :: Maybe Play
  } deriving (Show, Eq, Binary, Generic)

makeLenses ''PlayerPlays

instance ToJSON PlayerPlays where
  toJSON = genericToJSON $ aesonDrop 1 camelCase

data MatchResult = MatchResult
  { matchResultNorth :: Maybe (Text, Play)
  , matchResultEast :: Maybe (Text, Play)
  , matchResultSouth :: Maybe (Text, Play)
  , matchResultWest :: Maybe (Text, Play)
  } deriving (Show, Eq, Binary, Generic)

instance ToJSON MatchResult  where
  toJSON = genericToJSON $ aesonPrefix camelCase

data Player = Player
  { _score :: Integer
  , _name :: Text
  , _color :: Text
  , _position :: Position
  , _plays :: PlayerPlays
  , _lastRound :: Maybe MatchResult
  } deriving (Show, Eq, Binary, Generic)

makeLenses ''Player

instance ToJSON Player where
  toJSON = genericToJSON $ aesonDrop 1 camelCase

data Model = Model
  { _players :: Map PlayerId Player
  , _playerPositions :: Map Position PlayerId
  , _nextPosition :: Position
  , _rng :: StdGen
  } deriving (Show)

makeLenses ''Model

data GlobalView = GlobalView
  { viewPlayers :: [Player]
  , viewSampleCommands :: [Msg]
  } deriving (Show, Eq, Binary, Generic)

instance ToJSON GlobalView where
  toJSON = genericToJSON $ aesonDrop 4 camelCase

data PlayerView = PlayerView
  { viewNorth :: Maybe Text
  , viewSouth :: Maybe Text
  , viewEast :: Maybe Text
  , viewWest :: Maybe Text
  , viewLastRound :: Maybe MatchResult
  } deriving (Show, Eq, Binary, Generic)

instance ToJSON PlayerView where
  toJSON = genericToJSON $ aesonDrop 4 camelCase

data Msg
  = SetName Text
  | SetColor Text
  | MakeChoice Play
               Direction
  deriving (Show, Eq, Binary, Generic, FromJSON, ToJSON)

------------------------------------------------------------

initialModel :: StdGen -> Model
initialModel stdGen =
  Model
  { _players = Map.empty
  , _playerPositions = Map.empty
  , _nextPosition = (0, 0)
  , _rng = stdGen
  }

nextSpiral :: Position -> Position
nextSpiral (0, 0) = (1, 0)
nextSpiral (x, y)
  | x > 0 && x > abs y = (x, y - 1)
  | y > 0 && y >= abs x = (x + 1, y)
  | x < 0 && -x <= abs y = (x, y + 1)
  | y < 0 && -y >= abs x = (x - 1, y)

newPlayer :: Position -> Player
newPlayer pos =
  Player
  { _name = "<Your Name Here>"
  , _score = 0
  , _color = "white"
  , _position = pos
  , _plays = emptyPlays
  , _lastRound = Nothing
  }

emptyPlays :: PlayerPlays
emptyPlays =
  PlayerPlays
  {_north = Nothing, _east = Nothing, _south = Nothing, _west = Nothing}

update :: EngineMsg Msg -> Model -> Model
update (Join playerId) = addPlayer playerId
update (Leave playerId) = removePlayer playerId
update GameTick = tick
update (GameMsg playerId (SetName newName)) =
  set (players . ix playerId . name) newName
update (GameMsg playerId (SetColor text)) =
  set (players . ix playerId . color) text
update (GameMsg playerId (MakeChoice play dir)) = makeChoice playerId play dir

addPlayer :: PlayerId -> Model -> Model
addPlayer playerId model =
  let pos = _nextPosition model
  in model & set (players . at playerId) (Just (newPlayer pos)) &
     set (playerPositions . at pos) (Just playerId) &
     set nextPosition (nextSpiral pos)

removePlayer :: PlayerId -> Model -> Model
removePlayer playerId model =
  let pos = _position <$> Lens.view (players . at playerId) model
  in case pos of
       Nothing -> model
       Just pos ->
         model & set (players . at playerId) Nothing &
         set (playerPositions . at pos) Nothing

makeChoice :: PlayerId -> Play -> Direction -> Model -> Model
makeChoice playerId play dir =
  let d =
        case dir of
          North -> north
          East -> east
          South -> south
          West -> west
  in set (players . ix playerId . plays . d) (Just play)

tick :: Model -> Model
tick model = model {_players = Map.mapWithKey (tickPlayer model) $ _players model}

tickPlayer :: Model -> PlayerId -> Player -> Player
tickPlayer model playerId =
  updateLastRound model playerId >>> updateScore >>> resetPlays

updateLastRound :: Model -> PlayerId -> Player -> Player
updateLastRound model playerId player =
  let (x, y) = Lens.view position player
      result =
        MatchResult
          (getResult (x, y - 1) north)
          (getResult (x + 1, y) east)
          (getResult (x, y + 1) south)
          (getResult (x - 1, y) west)
  in set lastRound (Just result) player
  where
    getResult pos d = do
      opponentId <- Map.lookup pos (_playerPositions model)
      opponent <- Map.lookup opponentId (_players model)
      play <- Lens.view (plays . d) opponent
      return (Lens.view name opponent, play)

updateScore :: Player -> Player
updateScore player =
  let result = _lastRound player
  in case result of
       Nothing -> player
       Just result ->
         let getScore d1 d2 =
               scoreMatch (Lens.view (plays . d1) player) (snd <$> d2 result)
             score' =
               Lens.view score player + getScore north matchResultNorth +
               getScore east matchResultEast +
               getScore south matchResultSouth +
               getScore west matchResultWest
         in set score score' player

resetPlays :: Player -> Player
resetPlays = set plays emptyPlays

scoreMatch :: Maybe Play -> Maybe Play -> Integer
scoreMatch Nothing Nothing = 0
scoreMatch Nothing (Just Betray) = 0
scoreMatch Nothing (Just Cooperate) = 0
scoreMatch (Just Betray) Nothing = 0
scoreMatch (Just Cooperate) Nothing = 0
scoreMatch (Just Betray) (Just Betray) = 0
scoreMatch (Just Betray) (Just Cooperate) = 0
scoreMatch (Just Cooperate) (Just Betray) = 0
scoreMatch (Just Cooperate) (Just Cooperate) = 0

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
