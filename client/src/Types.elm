module Types exposing (..)

import RemoteData exposing (RemoteData)


type alias Coords =
    { x : Float
    , y : Float
    }


type alias Gps =
    { distance : Float
    , position : Coords
    }


type alias Player =
    { name : String
    , score : Int
    , position : Coords
    , color : Maybe String
    }


type alias Board =
    { gpss : List Gps
    , players : List Player
    }


type GameMsg
    = SetName String
    | SetColor String
    | Move Coords


type Msg
    = GameMsg GameMsg
    | Receive (RemoteData String Board)
    | KeepAlive


type alias Model =
    RemoteData String Board
