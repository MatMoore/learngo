module View exposing (..)

import List exposing (map)
import Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Game exposing (Point, Game)


view : Game -> Html.Html a
view model =
    svg
        [ width "300", height "300", viewBox "0 0 100 100" ]
        ([ rect
            [ x "0", y "0", width "100", height "100", fill "#dc7" ]
            []
         ]
            ++ (grid nineByNineConfig)
        )


type alias BoardConfig =
    { size : Int
    , padding : Float
    , lineWidth : Float
    , starRadius : Float
    , stars : List Point
    }


nineByNineConfig : BoardConfig
nineByNineConfig =
    { size = 9
    , lineWidth = 0.5
    , starRadius = 1.25
    , padding = 5
    , stars = [ ( 2, 2 ), ( 6, 6 ), ( 2, 6 ), ( 6, 2 ), ( 2, 4 ), ( 4, 4 ), ( 6, 2 ), ( 4, 6 ), ( 6, 4 ), ( 4, 2 ) ]
    }


lineSpacing : BoardConfig -> Float
lineSpacing config =
    (100 - (2 * config.padding)) / ((toFloat config.size) - 1.0)


grid : BoardConfig -> List (Svg a)
grid config =
    let
        top =
            config.padding

        bottom =
            100.0 - config.padding

        spacing =
            (lineSpacing config)

        left =
            top

        right =
            bottom

        gridSize =
            right - left

        linePositions =
            map (\number -> top + ((toFloat number) * spacing)) [1..(config.size - 2)]

        verticalLine x =
            line
                [ x1 (toString x)
                , x2 (toString x)
                , y1 (toString top)
                , y2 (toString bottom)
                , stroke "black"
                , strokeWidth (toString config.lineWidth)
                ]
                []

        horizontalLine y =
            line
                [ y1 (toString y)
                , y2 (toString y)
                , x1 (toString left)
                , x2 (toString right)
                , stroke "black"
                , strokeWidth (toString config.lineWidth)
                ]
                []

        verticalLines =
            map verticalLine linePositions

        horizontalLines =
            map horizontalLine linePositions

        star point =
            let
                ( x, y ) =
                    point

                xf =
                    (toFloat x)

                yf =
                    (toFloat y)
            in
                circle
                    [ cx (toString (left + xf * spacing))
                    , cy (toString (top + yf * spacing))
                    , r (toString config.starRadius)
                    ]
                    []

        stars =
            map star config.stars
    in
        [ rect
            [ x (toString left)
            , y (toString top)
            , width (toString gridSize)
            , height (toString gridSize)
            , stroke "black"
            , strokeWidth (toString config.lineWidth)
            , fill "transparent"
            ]
            []
        ]
            ++ verticalLines
            ++ horizontalLines
            ++ stars
