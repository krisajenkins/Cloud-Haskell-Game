module Types exposing (..)


type Msg
    = Receive String
    | KeepAlive


type alias Model =
    { lastMessage : Maybe String }
