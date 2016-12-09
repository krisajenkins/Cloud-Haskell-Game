module Types exposing (..)

import Json.Encode as E


type Msg
    = Send String
    | Receive (Result String E.Value)


type alias Model =
    { lastMsg : Maybe (Result String E.Value) }
