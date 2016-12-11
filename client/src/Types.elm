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
    }


type alias Board =
    { gpss : List Gps
    , players : List Player
    }


type Msg
    = SetName String
    | Join
    | Leave
    | Move Coords
    | Receive (RemoteData String Board)


type alias Model =
    RemoteData String Board
