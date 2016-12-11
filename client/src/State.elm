module State exposing (init, update, subscriptions)

import Json.Decode as D exposing (Decoder)
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)
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
    ( NotAsked, Cmd.none )


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

        SetColor string ->
            ( model
            , WebSocket.send websocketEndpoint
                (E.object
                    [ ( "tag", E.string "SetColor" )
                    , ( "contents", E.string string )
                    ]
                    |> E.encode 0
                )
            )

        Move to ->
            ( model
            , WebSocket.send websocketEndpoint
                (E.object
                    [ ( "tag", E.string "Move" )
                    , ( "contents"
                      , E.object
                            [ ( "_x", E.float to.x )
                            , ( "_y", E.float to.y )
                            ]
                      )
                    ]
                    |> E.encode 0
                )
            )

        Receive response ->
            ( response
            , Cmd.none
            )


decodeCoords : Decoder Coords
decodeCoords =
    decode Coords
        |> required "x" D.float
        |> required "y" D.float


decodeGps : Decoder Gps
decodeGps =
    decode Gps
        |> required "distance" D.float
        |> required "position" decodeCoords


decodePlayer : Decoder Player
decodePlayer =
    decode Player
        |> required "name" D.string
        |> required "score" D.int
        |> required "position" decodeCoords


decodeBoard : Decoder Board
decodeBoard =
    decode Board
        |> required "gpss" (D.list decodeGps)
        |> required "players" (D.list decodePlayer)


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen websocketEndpoint (D.decodeString decodeBoard >> RemoteData.fromResult >> Receive)
