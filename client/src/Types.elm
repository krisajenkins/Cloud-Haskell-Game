module Types exposing (..)

import RemoteData exposing (RemoteData)


type alias Coords =
    { x : Float
    , y : Float
    }


type alias Player =
    { score : Int
    , name : String
    , color : Maybe String
    , position : Coords
    , plays : PlayerPlays
    , lastRound : Maybe MatchResult
    }


type Play
    = Betray
    | StayLoyal


type alias PlayerPlays =
    { north : Maybe Play
    , east : Maybe Play
    , south : Maybe Play
    , west : Maybe Play
    }


type alias MatchResult =
    { north : Maybe ( String, Play )
    , east : Maybe ( String, Play )
    , south : Maybe ( String, Play )
    , west : Maybe ( String, Play )
    }


type alias Board =
    { players : List Player
    }


type Msg
    = Receive (RemoteData String Board)
    | KeepAlive


type alias Model =
    RemoteData String Board
