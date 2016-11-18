module Main exposing (..)

import Html exposing (programWithFlags)
import Game exposing (..)
import View exposing (view)
import AI exposing (generateMove, Strategy(..))
import Time exposing (millisecond)
import Board exposing (Player(..))


init : StartingStones -> ( Game, Cmd GameMessage )
init startingStones =
    ( newGameWithStones 9 startingStones
    , Cmd.none
    )


update : GameMessage -> Game -> ( Game, Cmd GameMessage )
update msg game =
    case msg of
        UserPlay point ->
            let
                result =
                    playMove game ( Black, Play point )
            in
                case result of
                    Ok newGame ->
                        generateMove ( RandomStone, White ) newGame

                    Err game ->
                        ( game, Cmd.none )

        ComputerPlay move ->
            ( { game | pendingMove = Just move }, Cmd.none )

        Tick time ->
            case game.pendingMove of
                Just move ->
                    let
                        result =
                            playMove game move
                    in
                        case result of
                            Ok newGame ->
                                ( { newGame | pendingMove = Nothing }, Cmd.none )

                            Err game ->
                                ( addMessage game "AI got confused :/", Cmd.none )

                Nothing ->
                    ( game, Cmd.none )


subscriptions model =
    Time.every (500 * millisecond) Tick


main =
    programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
