module View exposing (root)

import CDN exposing (bootstrap)
import Html exposing (..)
import Html.Attributes exposing (..)
import Types exposing (..)


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
        , div [] [ code [] [ text <| toString model ] ]
        ]
