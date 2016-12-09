module Types exposing (..)

import Json.Encode as E
import RemoteData exposing (RemoteData)


type Msg
    = SetName String
    | Join
    | Leave
    | Receive (RemoteData String E.Value)


type alias Model =
    { lastMsg : RemoteData String E.Value }
