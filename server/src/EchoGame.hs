{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module EchoGame where

import           Data.Binary
import           Data.Text    (Text)
import           GHC.Generics

------------------------------------------------------------
-- This Specific Game
------------------------------------------------------------
data GameState = GameState
  { lastMsg  :: Maybe Text
  , msgCount :: Int
  } deriving (Show, Eq, Binary, Generic)

init :: GameState
init =
  GameState
  { lastMsg = Nothing
  , msgCount = 0
  }

update :: Text -> GameState -> GameState
update msg state =
  GameState
  { lastMsg = Just msg
  , msgCount = msgCount state + 1
  }

type GameView = (Maybe Text, Int)

view :: GameState -> GameView
view GameState {..} = (lastMsg, msgCount)
