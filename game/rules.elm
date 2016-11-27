module Game.Rules
    exposing
        ( getRuleset
        , defaultRuleset
        )

import Game.Types exposing (..)
import Board exposing (Player(..), Point, Board, Annotation(..))


getRuleset : String -> Maybe Ruleset
getRuleset name =
    Just defaultRuleset


defaultRuleset : Ruleset
defaultRuleset =
    [ Rule placePlayer, Rule onePlayerPerTurnRule, Rule oneStonePerPointRule, Rule captureRule ]


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
                        Board.removeDeadNeighbors point inProgress.provisionalBoard
                in
                    Ok { inProgress | provisionalBoard = newBoard }

            _ ->
                Ok inProgress
