module State exposing (init, update, subscriptions)

import Json.Decode as D
import Json.Encode as E
import RemoteData exposing (RemoteData(..))
import Response exposing (..)
import Types exposing (..)
import WebSocket


websocketEndpoint : String
websocketEndpoint =
    "ws://localhost:9000"


init : Response Model Msg
init =
    ( { lastMsg = NotAsked }
    , Cmd.none
    )


update : Msg -> Model -> Response Model Msg
update msg model =
    case msg of
        Join ->
            ( model
            , WebSocket.send websocketEndpoint
                (E.object [ ( "tag", E.string "Join" ), ( "contents", E.list [] ) ] |> E.encode 0)
            )

        Leave ->
            ( model
            , WebSocket.send websocketEndpoint
                (E.object [ ( "tag", E.string "Leave" ), ( "contents", E.list [] ) ] |> E.encode 0)
            )

        SetName string ->
            ( model
            , WebSocket.send websocketEndpoint
                (E.object
                    [ ( "tag", E.string "SetName" )
                    , ( "contents", E.string string )
                    ]
                    |> E.encode 0
                )
            )

        Receive response ->
            ( { model | lastMsg = response }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen websocketEndpoint (D.decodeString D.value >> RemoteData.fromResult >> Receive)
