module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Types exposing (..)


root : Model -> Html Msg
root model =
    div []
        [ div []
            [ code []
                [ text <| toString model ]
            ]
        , button
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
        ]
