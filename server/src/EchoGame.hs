{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module EchoGame where

import           Control.Distributed.Process
import           Data.Binary
import           Data.Text                   (Text)
import           GHC.Generics

------------------------------------------------------------
-- This Specific Game
------------------------------------------------------------
data GameState = GameState
  { lastMsg  :: Maybe (String, Text)
  , msgCount :: Int
  } deriving (Show, Eq, Binary, Generic)

init :: GameState
init =
  GameState
  { lastMsg = Nothing
  , msgCount = 0
  }

update :: (SendPortId, Text) -> GameState -> GameState
update (portId,msg) state =
  GameState
  { lastMsg = Just (show portId,msg)
  , msgCount = msgCount state + 1
  }

type GameView = (Maybe (String, Text), Int)

view :: GameState -> GameView
view GameState {..} = (lastMsg, msgCount)
