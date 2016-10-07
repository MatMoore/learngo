module View exposing (view)

import List exposing (map)
import Dict
import Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Game exposing (Point, Game, Board, Stone(..))


type alias BoardConfig =
    { size : Int
    , padding : Float
    , lineWidth : Float
    , starRadius : Float
    , stars : List Point
    }


type alias SVGPoint =
    { x : String, y : String }


stoneColor : Stone -> String
stoneColor stone =
    case stone of
        Black ->
            "black"

        White ->
            "white"


lineSpacing : BoardConfig -> Float
lineSpacing config =
    (100 - (2 * config.padding)) / ((toFloat config.size) - 1.0)


boardPosition : BoardConfig -> Point -> SVGPoint
boardPosition config point =
    let
        ( x, y ) =
            point

        spacing =
            lineSpacing config
    in
        { x = toString (config.padding + (toFloat x) * spacing)
        , y = toString (config.padding + (toFloat y) * spacing)
        }


nineByNineConfig : BoardConfig
nineByNineConfig =
    { size = 9
    , lineWidth = 0.5
    , starRadius = 1.25
    , padding = 5
    , stars =
        [ ( 2, 2 )
        , ( 2, 4 )
        , ( 2, 6 )
        , ( 4, 2 )
        , ( 4, 4 )
        , ( 4, 6 )
        , ( 6, 2 )
        , ( 6, 4 )
        , ( 6, 6 )
        ]
    }


gridLine : BoardConfig -> SVGPoint -> SVGPoint -> Svg a
gridLine config start end =
    line
        [ x1 start.x
        , x2 end.x
        , y1 start.y
        , y2 end.y
        , stroke "black"
        , strokeWidth (toString config.lineWidth)
        ]
        []


verticalLine : BoardConfig -> Int -> Svg a
verticalLine config x =
    let
        top =
            boardPosition config ( x, 0 )

        bottom =
            boardPosition config ( x, config.size - 1 )
    in
        gridLine config top bottom


horizontalLine : BoardConfig -> Int -> Svg a
horizontalLine config y =
    let
        left =
            boardPosition config ( 0, y )

        right =
            boardPosition config ( config.size - 1, y )
    in
        gridLine config left right


stoneCircle : BoardConfig -> SVGPoint -> String -> Svg a
stoneCircle config svgPoint fillColor =
    let
        spacing =
            (lineSpacing config)

        stoneSize =
            (toString (spacing * 0.45))
    in
        circle
            [ cx svgPoint.x
            , cy svgPoint.y
            , r stoneSize
            , fill fillColor
            ]
            []


starCircle : BoardConfig -> SVGPoint -> Svg a
starCircle config centre =
    circle
        [ cx centre.x
        , cy centre.y
        , r (toString config.starRadius)
        ]
        []


grid : BoardConfig -> List (Svg a)
grid config =
    let
        topLeft =
            boardPosition config ( 0, 0 )

        gridSize =
            toString (100 - (2 * config.padding))

        linePositions =
            [0..(config.size - 1)]

        verticalLines =
            map (verticalLine config) linePositions

        horizontalLines =
            map (horizontalLine config) linePositions

        stars =
            map ((boardPosition config) >> (starCircle config)) config.stars

        background =
            rect
                [ x "0", y "0", width "100", height "100", fill "#dc7" ]
                []
    in
        (background :: verticalLines) ++ horizontalLines ++ stars


stones : BoardConfig -> Board -> List (Svg a)
stones config board =
    let
        viewStone point stone =
            (stoneCircle config (boardPosition config point) (stoneColor stone))
    in
        Dict.foldl
            (\point stone result -> (viewStone point stone) :: result)
            []
            board


view : Game -> Html.Html a
view model =
    svg
        [ width "300", height "300", viewBox "0 0 100 100" ]
        ([]
            ++ (grid nineByNineConfig)
            ++ (stones nineByNineConfig model.boardStones)
        )
