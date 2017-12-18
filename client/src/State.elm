module State exposing (init, update, subscriptions)

import Json.Decode as D exposing (Decoder)
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)
import RemoteData exposing (RemoteData(..))
import Response exposing (..)
import Types exposing (..)
import WebSocket


websocketEndpoint : String
websocketEndpoint =
    "ws://localhost:8000/global"


init : Response Model Msg
init =
    ( NotAsked, Cmd.none )


update : Msg -> Model -> Response Model Msg
update msg model =
    case msg of
        KeepAlive ->
            ( model, Cmd.none )

        Receive response ->
            ( response
            , Cmd.none
            )


decodeCoords : Decoder Coords
decodeCoords =
    D.map2 Coords (D.index 0 D.float) (D.index 1 D.float)


decodePlay : Decoder Play
decodePlay =
    D.string
        |> D.andThen
            (\s ->
                case s of
                    "Betray" ->
                        D.succeed Betray

                    "StayLoyal" ->
                        D.succeed StayLoyal

                    _ ->
                        D.fail "Invalid play value"
            )


decodePlayerPlays : Decoder PlayerPlays
decodePlayerPlays =
    decode PlayerPlays
        |> required "north" (D.maybe decodePlay)
        |> required "east" (D.maybe decodePlay)
        |> required "south" (D.maybe decodePlay)
        |> required "west" (D.maybe decodePlay)


decodeResult : Decoder ( String, Play )
decodeResult =
    D.map2 (,) (D.index 0 D.string) (D.index 1 decodePlay)


decodeMatchResult : Decoder MatchResult
decodeMatchResult =
    decode MatchResult
        |> required "north" (D.maybe decodeResult)
        |> required "east" (D.maybe decodeResult)
        |> required "south" (D.maybe decodeResult)
        |> required "west" (D.maybe decodeResult)


decodePlayer : Decoder Player
decodePlayer =
    decode Player
        |> required "score" D.int
        |> required "name" D.string
        |> required "color" (D.maybe D.string)
        |> required "position" decodeCoords
        |> required "plays" decodePlayerPlays
        |> required "lastRound" (D.maybe decodeMatchResult)


decodeBoard : Decoder Board
decodeBoard =
    decode Board
        |> required "players" (D.list decodePlayer)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ WebSocket.listen websocketEndpoint
            (D.decodeString decodeBoard
                >> RemoteData.fromResult
                >> Receive
            )
        , WebSocket.keepAlive websocketEndpoint
            |> Sub.map (always KeepAlive)
        ]
