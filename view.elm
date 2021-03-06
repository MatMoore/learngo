module View exposing (view)

import List exposing (map)
import Dict
import Html
import Set
import Svg.Events as Events
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Game.Types exposing (Game, GameMessage(..))
import Game.Log exposing (notes)
import Board exposing (Point, Board, Player(..))
import Group exposing (liberties)
import Types exposing (..)


type alias BoardConfig =
    { size : Int
    , padding : Float
    , lineWidth : Float
    , starRadius : Float
    , stoneRadius : Float
    , stars : List Point
    }


type alias SVGPoint =
    { x : String, y : String }


stoneColor : Player -> String
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
    , stoneRadius = 0.46
    , padding = 8
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


stoneCircle : BoardConfig -> SVGPoint -> { fillColor : String, onClick : Maybe a } -> Svg a
stoneCircle config svgPoint options =
    let
        spacing =
            (lineSpacing config)

        stoneSize =
            (toString (spacing * config.stoneRadius))

        coreOptions =
            [ cx svgPoint.x
            , cy svgPoint.y
            , r stoneSize
            , fill options.fillColor
            ]

        extraOptions =
            List.filterMap
                identity
                [ Maybe.map Events.onClick options.onClick ]
    in
        circle
            (coreOptions ++ extraOptions)
            []


starCircle : BoardConfig -> SVGPoint -> Svg a
starCircle config centre =
    circle
        [ cx centre.x
        , cy centre.y
        , r (toString config.starRadius)
        ]
        []


annotatePoint : BoardConfig -> SVGPoint -> String -> List (Svg a)
annotatePoint config centre str =
    [ circle
        [ cx centre.x
        , cy centre.y
        , r "4"
        , fill "dodgerblue"
        ]
        []
    , text_
        [ x centre.x
        , y centre.y
        , fontSize "6"
        , textAnchor "middle"
        , alignmentBaseline "central"
        ]
        [ text str ]
    ]


grid : BoardConfig -> List (Svg a)
grid config =
    let
        topLeft =
            boardPosition config ( 0, 0 )

        gridSize =
            toString (100 - (2 * config.padding))

        linePositions =
            List.range 0 (config.size - 1)

        verticalLines =
            List.map (verticalLine config) linePositions

        horizontalLines =
            List.map (horizontalLine config) linePositions

        stars =
            List.map ((boardPosition config) >> (starCircle config)) config.stars

        background =
            rect
                [ x "0", y "0", width "100", height "100", fill "#dc7" ]
                []
    in
        (background :: verticalLines) ++ horizontalLines ++ stars


stones : BoardConfig -> Board -> List (Svg a)
stones config board =
    let
        addStone : ( Point, Player ) -> List (Svg a) -> List (Svg a)
        addStone ( point, player ) elements =
            (viewPlayer point player) :: elements

        viewPlayer : Point -> Player -> Svg a
        viewPlayer point player =
            stoneCircle
                config
                (boardPosition config point)
                { fillColor = (stoneColor player), onClick = Nothing }
    in
        List.foldl addStone [] (Board.stones board)


buttons : BoardConfig -> List (Svg GameMessage)
buttons config =
    let
        numbers =
            List.range 0 (config.size - 1)

        allPoints =
            List.concatMap
                (\x -> List.map (\y -> ( x, y )) numbers)
                numbers

        playButton point =
            let
                options =
                    { fillColor = "transparent", onClick = Just (UserPlay point) }
            in
                stoneCircle config (boardPosition config point) options
    in
        List.map
            (\point -> (playButton point))
            allPoints


annotate : BoardConfig -> Board -> List (Svg GameMessage)
annotate config board =
    let
        annotateLiberty point =
            annotatePoint config (boardPosition config point) "1"

        annotateLiberties ( point, _ ) =
            List.concatMap annotateLiberty (Set.toList (liberties point board))
    in
        List.concatMap annotateLiberties (Board.annotations board)


view : Model -> Html.Html GameMessage
view model =
    Html.div
        []
        [ boardView model.game
        , chatView model.game
        ]


chatView : Game -> Html.Html GameMessage
chatView game =
    Html.ul
        []
        (List.map chatItemView (notes game.log))


chatItemView : String -> Html.Html GameMessage
chatItemView item =
    Html.li
        []
        [ text item ]


boardView : Game -> Html.Html GameMessage
boardView game =
    svg
        [ width "300", height "300", viewBox "0 0 100 100" ]
        ([]
            ++ (grid nineByNineConfig)
            ++ (stones nineByNineConfig game.board)
            ++ (annotate nineByNineConfig game.board)
            ++ (buttons nineByNineConfig)
        )
