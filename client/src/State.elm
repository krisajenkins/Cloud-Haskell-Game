module State exposing (init, update, subscriptions)

import Response exposing (..)
import Types exposing (..)
import WebSocket


websocketEndpoint : String
websocketEndpoint =
    "ws://localhost:8000"


init : Response Model Msg
init =
    ( { lastMsg = Nothing }
    , Cmd.none
    )


update : Msg -> Model -> Response Model Msg
update msg model =
    case msg of
        Send string ->
            ( model
            , WebSocket.send websocketEndpoint string
            )

        Receive string ->
            ( { model | lastMsg = Just string }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen websocketEndpoint Receive
