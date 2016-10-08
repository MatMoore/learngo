module Game exposing (..)

import Dict exposing (Dict)
import Array exposing (Array)


type Player
    = Black
    | White


type alias Point =
    ( Int, Int )


type Action
    = Play Point
    | Pass
    | Resign


type alias Move =
    ( Player, Action )


type GameMessage
    = UserPlay Point


type alias Board =
    Dict Point Player


type alias MoveRecord =
    { move : Move
    , notes : List String
    }


type GameRecord
    = LastMove (Array MoveRecord) Int
    | NotStarted


type alias Game =
    { boardPlayers : Board
    , capturedStones : Dict Player Int
    , boardSize : Int
    , gameRecord : GameRecord
    , currentPlayer : Player
    , rules : List Rule
    }


type alias MoveInProgress =
    { provisionalBoard : Board, currentMove : Move, game : Game }


type Rule
    = Rule (MoveInProgress -> Result String MoveInProgress)


newGame : Int -> Game
newGame boardSize =
    { boardSize = boardSize
    , boardPlayers = Dict.empty
    , capturedStones = Dict.empty
    , gameRecord = NotStarted
    , rules = [ Rule placePlayer, Rule onePlayerPerTurnRule ]
    , currentPlayer = Black
    }


newMoveRecord : Move -> MoveRecord
newMoveRecord move =
    { move = move, notes = [] }


pushMoveRecord : MoveRecord -> GameRecord -> GameRecord
pushMoveRecord moveRecord gameRecord =
    case gameRecord of
        LastMove moves idx ->
            LastMove (Array.push moveRecord moves) (idx + 1)

        NotStarted ->
            LastMove (Array.fromList [ moveRecord ]) 0


applyRules : Game -> Move -> Result String MoveInProgress
applyRules game move =
    let
        initial : Result String MoveInProgress
        initial =
            Ok { provisionalBoard = game.boardPlayers, currentMove = move, game = game }

        foldStep (Rule rule) result =
            case result of
                Ok moveInProgress ->
                    rule moveInProgress

                error ->
                    error
    in
        List.foldl foldStep initial game.rules


nextPlayer : Player -> Player
nextPlayer player =
    case player of
        Black ->
            White

        White ->
            Black


playMove : Game -> Move -> Game
playMove game move =
    let
        result =
            applyRules game move
    in
        case result of
            Ok moveInProgress ->
                { game | boardPlayers = moveInProgress.provisionalBoard, gameRecord = pushMoveRecord (newMoveRecord move) game.gameRecord, currentPlayer = nextPlayer game.currentPlayer }

            _ ->
                game


placePlayer : MoveInProgress -> Result String MoveInProgress
placePlayer inProgress =
    case inProgress.currentMove of
        ( stone, Play point ) ->
            Ok
                { inProgress
                    | provisionalBoard = (Dict.insert point stone inProgress.provisionalBoard)
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
