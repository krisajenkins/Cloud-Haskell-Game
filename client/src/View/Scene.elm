module View.Scene exposing (root)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Types exposing (..)


root : Board -> Svg msg
root board =
    svg
        [ width "70vw"
        , height "70vh"
        , viewBox "-4 -4 8 8"
        ]
        [ g [] (List.map playerView board.players)
        ]


inset : Float
inset =
    0.1


playerView : Player -> Svg msg
playerView player =
    let
        weightedInset =
            inset
                * (if player.score < 500 then
                    3.5
                   else if player.score < 1000 then
                    2.5
                   else if player.score < 2000 then
                    1.5
                   else
                    1
                  )

        thePlayer =
            rect
                [ x <| toString <| player.position.x + weightedInset
                , y <| toString <| player.position.y + weightedInset
                , width <| toString <| 1 - (2 * weightedInset)
                , height <| toString <| 1 - (2 * weightedInset)
                , opacity "0.8"
                , stroke (Maybe.withDefault "black" player.color)
                , strokeWidth "0.1"
                , fill (Maybe.withDefault "white" player.color)
                , Svg.Attributes.style "transition: all 200ms"
                ]
                []

        theWalls =
            List.map wallView
                [ ( player.plays.north, player.position.x + inset, player.position.x + 1 - inset, player.position.y + inset, player.position.y + inset )
                , ( player.plays.south, player.position.x + inset, player.position.x + 1 - inset, player.position.y + 1 - inset, player.position.y + 1 - inset )
                , ( player.plays.west, player.position.x + inset, player.position.x + inset, player.position.y + inset, player.position.y + 1 - inset )
                , ( player.plays.east, player.position.x + 1 - inset, player.position.x + 1 - inset, player.position.y + inset, player.position.y + 1 - inset )
                ]
    in
    g []
        (thePlayer :: theWalls)


wallView : ( Maybe Play, Float, Float, Float, Float ) -> Svg msg
wallView ( play, theX1, theX2, theY1, theY2 ) =
    line
        [ x1 <| toString theX1
        , x2 <| toString theX2
        , y1 <| toString theY1
        , y2 <| toString theY2
        , stroke
            (case play of
                Just Betray ->
                    "red"

                Just StayLoyal ->
                    "green"

                _ ->
                    "transparent"
            )
        , strokeWidth <| toString <| inset
        , strokeLinecap "square"
        ]
        []
