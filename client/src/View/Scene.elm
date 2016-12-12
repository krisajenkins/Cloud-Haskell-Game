module View.Scene exposing (root)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Types exposing (..)


root : Board -> Svg msg
root board =
    svg
        [ width "70vw"
        , height "70vw"
        , viewBox "-30 -30 60 60"
        ]
        [ g [] (List.map playerView board.players)
        , g [] (List.map gpsView board.gpss)
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


gpsView : Gps -> Svg msg
gpsView gps =
    g []
        [ circle
            [ cx <| toString gps.position.x
            , cy <| toString gps.position.y
            , r "0.25"
            , fill "green"
            , Svg.Attributes.style "transition: all 1s"
            ]
            []
        , circle
            [ cx <| toString gps.position.x
            , cy <| toString gps.position.y
            , r <| toString gps.distance
            , opacity "0.6"
            , stroke "#0697e8"
            , strokeWidth "0.5"
            , fill "none"
            , Svg.Attributes.style "transition: all 1s"
            ]
            []
        ]
