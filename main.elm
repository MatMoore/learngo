module Main exposing (..)

--import Html.App exposing (programWithFlags)

import Html.App exposing (program)
import Game exposing (..)
import View exposing (view)
import AI exposing (generateMove, Strategy(..))
import Time exposing (millisecond)


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
            ( { game | pendingMove = Just move }, Cmd.none )

        Tick time ->
            case game.pendingMove of
                Just move ->
                    let
                        newGame =
                            playMove game move
                    in
                        ( { newGame | pendingMove = Nothing }, Cmd.none )

                Nothing ->
                    ( game, Cmd.none )


subscriptions model =
    Time.every (500 * millisecond) Tick


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
