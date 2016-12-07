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
            , onClick (Send "Hello")
            ]
            [ text "Hello"
            ]
        , button
            [ type_ "button"
            , onClick (Send "Goodbye")
            ]
            [ text "Goodbye"
            ]
        ]
