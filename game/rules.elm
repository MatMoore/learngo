module Game.Rules
    exposing
        ( play
        )

{-| Module Game.Rules defines some a ruleset for Go.

# Play a move
@docs play
-}

import Game.Types exposing (..)
import Board exposing (Player(..), Point, Board, Annotation(..), nextPlayer)
import Group exposing (removeDeadNeighbors, removeDead)
import Game.Log exposing (addNote, addMove)
import Set


{-| Represents a move that has not yet been accepted.
-}
type alias MoveInProgress =
    { provisionalBoard : Board, currentMove : Move, game : Game }


{-|
Play a move
-}
play : Move -> Game -> Result Game Game
play move game =
    let
        initial =
            Ok { provisionalBoard = game.board, currentMove = move, game = game }

        result =
            initial
                |> Result.andThen placePlayer
                |> Result.andThen onePlayerPerTurnRule
                |> Result.andThen oneStonePerPointRule
                |> Result.andThen captureRule
                |> Result.andThen suicideRule
    in
        case result of
            Ok moveInProgress ->
                Ok { game | board = moveInProgress.provisionalBoard, log = (addMove move game.log), currentPlayer = nextPlayer game.currentPlayer }

            Err msg ->
                Err { game | log = addNote msg game.log }


placePlayer : MoveInProgress -> Result String MoveInProgress
placePlayer inProgress =
    case inProgress.currentMove of
        ( player, Play point ) ->
            Ok
                { inProgress
                    | provisionalBoard = Board.place player point inProgress.provisionalBoard
                }

        _ ->
            Ok inProgress


onePlayerPerTurnRule : MoveInProgress -> Result String MoveInProgress
onePlayerPerTurnRule inProgress =
    let
        ( player, action ) =
            inProgress.currentMove
    in
        if player == inProgress.game.currentPlayer then
            Ok inProgress
        else
            Err "It's not your turn"


oneStonePerPointRule : MoveInProgress -> Result String MoveInProgress
oneStonePerPointRule inProgress =
    let
        ( player, action ) =
            inProgress.currentMove

        board =
            inProgress.game.board
    in
        case action of
            Play point ->
                if Board.isFilled point board then
                    Err "You can't put a stone on top of another stone"
                else
                    Ok inProgress

            _ ->
                Ok inProgress


captureRule : MoveInProgress -> Result String MoveInProgress
captureRule inProgress =
    let
        ( player, action ) =
            inProgress.currentMove

        game =
            inProgress.game
    in
        case action of
            Play point ->
                let
                    newBoard =
                        removeDeadNeighbors point inProgress.provisionalBoard
                in
                    Ok { inProgress | provisionalBoard = newBoard }

            _ ->
                Ok inProgress


suicideRule : MoveInProgress -> Result String MoveInProgress
suicideRule inProgress =
    let
        { currentMove, provisionalBoard } =
            inProgress
    in
        case currentMove of
            ( player, Play point ) ->
                let
                    boardAfterRule =
                        removeDead point provisionalBoard
                in
                    if (Board.stoneAt point boardAfterRule) == Just player then
                        Ok inProgress
                    else
                        Err "Cannot place a stone with no liberties"

            _ ->
                Ok inProgress
