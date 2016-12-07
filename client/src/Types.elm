module Types exposing (..)


type Msg
    = Send String
    | Receive String


type alias Model =
    { lastMsg : Maybe String }
