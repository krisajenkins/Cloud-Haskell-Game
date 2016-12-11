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
import           Data.Text.Encoding
import           GHC.Generics
import           System.Random
import qualified Text.Regex.PCRE.Light       as Regex

data CssColor = CssColor Text
  deriving (Show, Eq, Binary, Generic, FromJSON)

mkCssColor :: Text -> Maybe CssColor
mkCssColor text =
  case Regex.match cssColorRegex (encodeUtf8 text) [] of
    Nothing -> Nothing
    Just _ -> Just (CssColor text)
  where
    cssColorRegex :: Regex.Regex
    cssColorRegex = Regex.compile "^#[0-9a-f]{6}$" [Regex.caseless]

data Coords = Coords
  { _x :: Double
  , _y :: Double
  } deriving (Show, Eq, Binary, Generic, FromJSON)

data Player = Player
  { _position :: Coords
  , _score    :: Integer
  , _name     :: Text
  , _color    :: Maybe CssColor
  } deriving (Show, Eq, Binary, Generic)

data Model = Model
  { _players :: Map SendPortId Player
  , _gpss    :: [Coords]
  , _present :: Coords
  , _rng     :: StdGen
  } deriving (Show)

makeLenses ''Coords

makeLenses ''Player

makeLenses ''Model

instance ToJSON CssColor where
  toJSON = genericToJSON $ aesonDrop 1 camelCase

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

init :: StdGen ->  Model
init stdGen =
  Model
  { _players = Map.empty
  , _gpss = [Coords (-10) (-15), Coords 3 (-5), Coords 12 3]
  , _present = Coords 5 3
  , _rng = stdGen
  }

newPlayer :: Player
newPlayer =
  Player
  { _name = "<Your Name Here>"
  , _score = 0
  , _position = Coords 0 0
  , _color = Nothing
  }

data Msg
  = Join
  | Leave
  | SetName Text
  | SetColor Text
  | Move Coords
  deriving (Show, Eq, Binary, Generic, FromJSON, ToJSON)

update :: (SendPortId, Msg) -> Model -> Model
update msg = handleWin . handleMsg msg

randomPair
  :: (RandomGen g, Random a)
  => (a, a) -> g -> ((a, a), g)
randomPair range stdGen = ((a, b), stdGen'')
  where
    (a, stdGen') = randomR range stdGen
    (b, stdGen'') = randomR range stdGen'


handleWin :: Model -> Model
handleWin model =
  if null overlappingPlayers
    then model
    else let ((newX, newY), stdGen) = randomPair (-10, 10) $ Lens.view rng model
             withIncrementedScores aModel =
               Map.foldlWithKey
                 (\aModel' portId _ ->
                     over (players . ix portId . score) (+ 1) aModel')
                 aModel
                 overlappingPlayers
             withMovedPresent = set present (Coords newX newY)
             withNewStdGen = set rng stdGen
         in withNewStdGen . withMovedPresent . withIncrementedScores $ model
  where
    overlappingPlayers :: Map SendPortId Player
    overlappingPlayers = Map.filter inRange (Lens.view players model)
    inRange :: Player -> Bool
    inRange player =
      distanceBetween (Lens.view present model) (Lens.view position player) < 0.5

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

handleMsg :: (SendPortId, Msg) -> Model -> Model
handleMsg (playerId, Join) model =
  set (players . at playerId) (Just newPlayer) model
handleMsg (playerId, Leave) model = set (players . at playerId) Nothing model
handleMsg (playerId, SetName newName) model =
  set (players . ix playerId . name) newName model
handleMsg (playerId, SetColor text) model =
  set (players . ix playerId . color) (mkCssColor text) model
handleMsg (playerId, Move moveTo) model =
  over (players . ix playerId . position) updatePosition model
  where
    updatePosition = over x (dx +) . over y (dy +)
    (dx, dy) = normalise (Lens.view x moveTo, Lens.view y moveTo)

view :: Model -> View
view model =
  View
  { viewPlayers = toListOf (players . traverse) model
  , viewGpss = viewGps (Lens.view present model) <$> Lens.view gpss model
  , viewSampleCommands =
    [ Join
    , Leave
    , SetName "Kris"
    , Move $ Coords 1.0 (-2.0)
    , SetColor "#ff0000"
    ]
  }

viewGps :: Coords -> Coords -> ViewGps
viewGps presentPosition gps =
  ViewGps
  { viewGpsPosition = gps
  , viewGpsDistance = distanceBetween presentPosition gps
  }
