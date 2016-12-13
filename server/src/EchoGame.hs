{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module EchoGame where

import           Control.Distributed.Process
import           Data.Binary
import           Data.Text                   (Text)
import           GHC.Generics
import           Lib

------------------------------------------------------------
-- This Specific Game
------------------------------------------------------------
data Model = Model
  { lastMsg  :: Maybe (String, Text)
  , msgCount :: Int
  } deriving (Show, Eq, Binary, Generic)

init :: Model
init =
  Model
  { lastMsg = Nothing
  , msgCount = 0
  }

update :: (SendPortId, EngineMsg Text) -> Model -> Model
update (_, Join) model = model
update (_, Leave) model = model
update (portId, GameMsg msg) model =
  Model
  { lastMsg = Just (show portId, msg)
  , msgCount = msgCount model + 1
  }

type View = (Maybe (String, Text), Int)

view :: Model -> View
view Model {..} = (lastMsg, msgCount)
