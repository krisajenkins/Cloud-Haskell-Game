module View.Scene exposing (root)

import Formatting as F exposing ((<>))
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Types exposing (..)


root : Board -> Svg msg
root board =
    svg
        [ width "70vw"
        , height "70vh"
        , viewBox "-30 -30 60 60"
        ]
        [ g [] (List.map playerView board.players)
        ]


playerView : Player -> Svg msg
playerView player =
    circle
        [ cx <| toString player.position.x
        , cy <| toString player.position.y
        , r "1"
        , opacity "0.8"
        , stroke "black"
        , strokeWidth "0.05"
        , fill (Maybe.withDefault "white" player.color)
        , Svg.Attributes.style "transition: all 200ms"
        ]
        []
