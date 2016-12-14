{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module EchoGame
  ( initialModel
  , update
  , view
  ) where

import Control.Distributed.Process
import Data.Binary
import Data.Text (Text)
import GHC.Generics
import Network.GameEngine

------------------------------------------------------------
-- This Specific Game
------------------------------------------------------------
data Model = Model
  { lastMsg :: Maybe (String, Text)
  , msgCount :: Int
  } deriving (Show, Eq, Binary, Generic)

initialModel :: Model
initialModel =
  Model
  { lastMsg = Nothing
  , msgCount = 0
  }

update :: EngineMsg Text -> Model -> Model
update (Join _) model = model
update (Leave _) model = model
update (GameMsg portId msg) model =
  Model
  { lastMsg = Just (show portId, msg)
  , msgCount = msgCount model + 1
  }

type View = (Maybe (String, Text), Int)

view :: Model -> View
view Model {..} = (lastMsg, msgCount)
