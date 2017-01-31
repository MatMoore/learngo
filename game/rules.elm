module Game.Rules
    exposing
        ( getRuleset
        , defaultRuleset
        )

{-| Module Game.Rules defines some rulesets for Go, using the types defined in Game.Types.

# Retrieving rules
@docs getRuleset, defaultRuleset
-}

import Game.Types exposing (..)
import Board exposing (Player(..), Point, Board, Annotation(..))
import Group exposing (removeDeadNeighbors, liberties)
import Set


{-| Get a ruleset by name.
-}
getRuleset : String -> Maybe Ruleset
getRuleset name =
    Just defaultRuleset


{-| Get the default ruleset.
-}
defaultRuleset : Ruleset
defaultRuleset =
    [ Rule placePlayer, Rule onePlayerPerTurnRule, Rule oneStonePerPointRule, Rule captureRule, Rule suicideRule ]


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
            ( _, Play point ) ->
                if (Set.isEmpty (liberties point provisionalBoard)) then
                    Err "Cannot place a stone with no liberties"
                else
                    Ok inProgress

            _ ->
                Ok inProgress
