module Main exposing (..)

--import Html.App exposing (programWithFlags)

import Html.App exposing (program)
import Game exposing (..)
import View exposing (view)
import AI exposing (generateMove, Strategy(..))


type alias Flags =
    { size : Int }



--init : Flags -> ( Model, Cmd Msg )
--init flags =
--    ( { size = flags.size, stones = Dict.empty }
--    , Cmd.none
--    )


init : ( Game, Cmd GameMessage )
init =
    ( newGame 9
    , Cmd.none
    )


update : GameMessage -> Game -> ( Game, Cmd GameMessage )
update msg game =
    case msg of
        UserPlay point ->
            let
                newGame =
                    playMove game ( Black, Play point )
            in
                generateMove ( RandomStone, White ) newGame

        ComputerPlay move ->
            let
                newGame =
                    playMove game move
            in
                ( newGame, Cmd.none )


subscriptions model =
    Sub.none


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
