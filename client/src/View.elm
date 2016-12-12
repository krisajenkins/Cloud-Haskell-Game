module View exposing (..)

import CDN exposing (bootstrap)
import Exts.Html exposing (nbsp)
import Formatting as F exposing ((<>))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import RemoteData exposing (..)
import Types exposing (..)
import View.Scene


root : Model -> Html Msg
root model =
    div
        [ style
            [ ( "padding", "15px" )
            , ( "display", "flex" )
            , ( "flex-direction", "column" )
            ]
        ]
        [ bootstrap.css
        , controls
        , remoteDataView boardView model
        ]


remoteDataView : (a -> Html msg) -> RemoteData String a -> Html msg
remoteDataView view remoteData =
    case remoteData of
        NotAsked ->
            text ""

        Loading ->
            text "Loading."

        Failure err ->
            text err

        Success data ->
            view data


boardView : Board -> Html Msg
boardView board =
    div
        [ style
            [ ( "display", "flex" )
            ]
        ]
        [ div
            [ style
                [ ( "width", "70vw" )
                , ( "padding", "15px" )
                ]
            ]
            [ View.Scene.root board ]
        , div [ style [ ( "padding", "15px" ) ] ]
            [ playerList board.players ]
        ]


debuggingView : a -> Html msg
debuggingView datum =
    div [] [ code [] [ text <| toString datum ] ]


gpsList : List Gps -> Html msg
gpsList gpss =
    ul [ class "list-group" ]
        (List.map gpsView gpss)


gpsView : Gps -> Html msg
gpsView gps =
    li [ class "list-group-item" ]
        [ text <| toString gps.position
        , text " "
        , text <| toString gps.distance
        ]


playerList : List Player -> Html msg
playerList players =
    ul [ class "list-group" ]
        (players
            |> List.sortBy (.score >> ((*) -1))
            |> List.map playerView
        )


playerView : Player -> Html msg
playerView player =
    li
        [ class "list-group-item"
        , style
            [ ( "display", "flex" )
            , ( "align-items", "center" )
            ]
        ]
        [ div
            [ style
                [ ( "display", "inline-block" )
                , ( "border", "solid 1px black" )
                , ( "background-color", (Maybe.withDefault "white" player.color) )
                , ( "height", "15px" )
                , ( "width", "15px" )
                , ( "margin-right", "15px" )
                ]
            ]
            [ text nbsp ]
        , F.html (F.s "(" <> F.int <> F.s ") " <> F.string)
            player.score
            player.name
        ]


controls : Html Msg
controls =
    div
        [ class "btn-group"
        , style [ ( "margin", "0 auto" ) ]
        ]
        [ button
            [ type_ "button"
            , class "btn btn-default btn-sm"
            , onClick Join
            ]
            [ text "Join"
            ]
        , button
            [ type_ "button"
            , class "btn btn-default btn-sm"
            , onClick Leave
            ]
            [ text "Leave"
            ]
        , button
            [ type_ "button"
            , class "btn btn-default btn-sm"
            , onClick (SetName "Kris")
            ]
            [ text "Set Name"
            ]
        , button
            [ type_ "button"
            , class "btn btn-default btn-sm"
            , onClick (SetColor "#0000ff")
            ]
            [ text "Set Color"
            ]
        , button
            [ type_ "button"
            , class "btn btn-default btn-sm"
            , onClick <| Move <| Coords 1.2 4.1
            ]
            [ text "Move 1.2 4.1"
            ]
        , button
            [ type_ "button"
            , class "btn btn-default btn-sm"
            , onClick <| Move <| Coords -4.1 -1.2
            ]
            [ text "Move -4.1 -1.2 "
            ]
        , button
            [ type_ "button"
            , class "btn btn-default btn-sm"
            , onClick <| Move <| Coords -1.0 0
            ]
            [ text "Left"
            ]
        , button
            [ type_ "button"
            , class "btn btn-default btn-sm"
            , onClick <| Move <| Coords 0.0 -1.0
            ]
            [ text "Up"
            ]
        , button
            [ type_ "button"
            , class "btn btn-default btn-sm"
            , onClick <| Move <| Coords 0.0 1.0
            ]
            [ text "Down"
            ]
        , button
            [ type_ "button"
            , class "btn btn-default btn-sm"
            , onClick <| Move <| Coords 1.0 0
            ]
            [ text "Right"
            ]
        ]
