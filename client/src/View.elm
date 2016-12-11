module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import RemoteData exposing (..)
import Types exposing (..)


root : Model -> Html Msg
root model =
    div []
        [ case model of
            NotAsked ->
                text ""

            Loading ->
                text "Loading."

            Failure err ->
                text err

            Success board ->
                boardView board
        , controls
        ]


boardView : Board -> Html Msg
boardView model =
    div []
        [ gpsList model.gpss
        , playerList model.players
        ]


debuggingView : a -> Html msg
debuggingView datum =
    div [] [ code [] [ text <| toString datum ] ]


gpsList : List Gps -> Html msg
gpsList gpss =
    ul [] (List.map gpsView gpss)


gpsView : Gps -> Html msg
gpsView gps =
    li []
        [ text <| toString gps.position
        , text " "
        , text <| toString gps.distance
        ]


playerList : List Player -> Html msg
playerList players =
    ul [] (List.map playerView players)


playerView : Player -> Html msg
playerView player =
    li []
        [ text player.name
        , text " "
        , text <| toString player.score
        , text " "
        , text <| toString player.position
        ]


controls : Html Msg
controls =
    div []
        [ button
            [ type_ "button"
            , onClick Join
            ]
            [ text "Join"
            ]
        , button
            [ type_ "button"
            , onClick Leave
            ]
            [ text "Leave"
            ]
        , button
            [ type_ "button"
            , onClick (SetName "Kris")
            ]
            [ text "Set Name"
            ]
        , button
            [ type_ "button"
            , onClick <| Move <| Coords 1.2 4.1
            ]
            [ text "Move 1.2 4.1"
            ]
        , button
            [ type_ "button"
            , onClick <| Move <| Coords -4.1 -1.2
            ]
            [ text "Move -4.1 -1.2 "
            ]
        , button
            [ type_ "button"
            , onClick <| Move <| Coords -1.0 0
            ]
            [ text "Left"
            ]
        , button
            [ type_ "button"
            , onClick <| Move <| Coords 0.0 -1.0
            ]
            [ text "Down"
            ]
        , button
            [ type_ "button"
            , onClick <| Move <| Coords 0.0 1.0
            ]
            [ text "Up"
            ]
        , button
            [ type_ "button"
            , onClick <| Move <| Coords 1.0 0
            ]
            [ text "Right"
            ]
        ]
