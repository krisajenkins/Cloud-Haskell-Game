module View exposing (root)

import CDN exposing (bootstrap)
import Exts.Html exposing (nbsp)
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
        , heading
        , remoteDataView boardView model
        ]


remoteDataView : (a -> Html msg) -> RemoteData String a -> Html msg
remoteDataView view remoteData =
    case remoteData of
        NotAsked ->
            text "Initialising."

        Loading ->
            text "Loading."

        Failure err ->
            text err

        Success data ->
            view data


heading : Html Msg
heading =
    div [ style [ ( "text-align", "center" ) ] ]
        [ h1 [] [ text "Triangulation!" ]
        , Html.map GameMsg controls
        ]


boardView : Board -> Html Msg
boardView board =
    div [ style [ ( "display", "flex" ) ] ]
        [ div
            [ style
                [ ( "width", "70vw" )
                , ( "padding", "15px" )
                ]
            ]
            [ View.Scene.root board ]
        , div
            [ style
                [ ( "padding", "15px" )
                , ( "flex-grow", "1" )
                ]
            ]
            [ playerList board.players ]
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
                , ( "transition", "all 1s" )
                , ( "height", "15px" )
                , ( "width", "15px" )
                , ( "margin-right", "15px" )
                ]
            ]
            [ text nbsp ]
        , div [ style [ ( "flex-grow", "1" ) ] ]
            [ text player.name ]
        , div [ class "badge" ]
            [ text <| toString player.score ]
        ]


controls : Html GameMsg
controls =
    let
        simpleButton label msg =
            button
                [ type_ "button"
                , class "btn btn-default btn-sm"
                , onClick msg
                ]
                [ text label ]
    in
        div
            [ class "btn-group"
            , style [ ( "margin", "0 auto" ) ]
            ]
            (List.map (uncurry simpleButton)
                [ ( "Set Name", SetName "Kris" )
                , ( "Set Color", SetColor "#0000ff" )
                , ( "Left", Move <| Coords -1.0 0 )
                , ( "Up", Move <| Coords 0.0 -1.0 )
                , ( "Down", Move <| Coords 0.0 1.0 )
                , ( "Right", Move <| Coords 1.0 0 )
                ]
            )
