module Main exposing (..)

import List exposing (map)
import Dict exposing (Dict)
import Html.App exposing (programWithFlags)
import Html.App exposing (program)
import Html
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Point =
    ( Int, Int )


type Stone
    = Black
    | White


type Msg
    = NotImplemented


type alias Model =
    { size : Int, stones : Dict Point Stone }


type alias Flags =
    { size : Int }



--init : Flags -> ( Model, Cmd Msg )
--init flags =
--    ( { size = flags.size, stones = Dict.empty }
--    , Cmd.none
--    )


init : ( Model, Cmd Msg )
init =
    ( { size = 9, stones = Dict.empty }
    , Cmd.none
    )


update msg model =
    ( model, Cmd.none )


subscriptions model =
    Sub.none


type alias BoardConfig =
    { size : Int
    , padding : Float
    , lineWidth : Float
    , starRadius : Float
    , stars : List Point
    }


defaultConfig : BoardConfig
defaultConfig =
    { size = 9, lineWidth = 0.5, starRadius = 1.25, padding = 5, stars = [ ( 2, 2 ), ( 6, 6 ), ( 2, 6 ), ( 6, 2 ), ( 2, 4 ), ( 4, 4 ), ( 6, 2 ), ( 4, 6 ), ( 6, 4 ), ( 4, 2 ) ] }


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
                , x1 (toString top)
                , x2 (toString bottom)
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


view : Model -> Html.Html a
view model =
    svg
        [ width "300", height "300", viewBox "0 0 100 100" ]
        ([ rect
            [ x "0", y "0", width "100", height "100", fill "#dc7" ]
            []
         ]
            ++ (grid defaultConfig)
        )


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
