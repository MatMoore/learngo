module Game.Api exposing (new, play)

import Game.Types exposing (..)
import Game.Rules exposing (defaultRuleset)
import Game.Record exposing (addMessage, addMove)
import Dict exposing (Dict)
import Board exposing (Player(..), nextPlayer)


new : Int -> Game
new boardSize =
    { board = Board.new boardSize
    , capturedStones = Dict.empty
    , gameRecord = NotStarted
    , rules = defaultRuleset
    , currentPlayer = Black
    }


applyRules : Move -> Game -> Result String MoveInProgress
applyRules move game =
    let
        initial : Result String MoveInProgress
        initial =
            Ok { provisionalBoard = game.board, currentMove = move, game = game }

        foldStep (Rule rule) result =
            case result of
                Ok moveInProgress ->
                    rule moveInProgress

                error ->
                    error
    in
        List.foldl foldStep initial game.rules


play : Move -> Game -> Result Game Game
play move game =
    let
        result =
            applyRules move game
    in
        case result of
            Ok moveInProgress ->
                Ok { game | board = moveInProgress.provisionalBoard, gameRecord = (addMove move game.gameRecord), currentPlayer = nextPlayer game.currentPlayer }

            Err msg ->
                Err { game | gameRecord = addMessage game.gameRecord msg }
