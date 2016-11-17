module Game exposing (..)

import Random
import Dict exposing (Dict)
import Array exposing (Array)
import Maybe
import Time exposing (Time)
import Debug exposing (log)
import Board exposing (Player(..), Point, Board, Annotation(..))


type Action
    = Play Point
    | Pass
    | Resign


type alias Move =
    ( Player, Action )


type GameMessage
    = UserPlay Point
    | ComputerPlay Move
    | Tick Time


type alias MoveRecord =
    { move : Move
    , notes : List String
    }


type GameRecord
    = MoveSequence (List MoveRecord) MoveRecord (List MoveRecord)
    | NotStarted


addMessage : Game -> String -> Game
addMessage game msg =
    case game.gameRecord of
        NotStarted ->
            game

        MoveSequence prev current next ->
            let
                newRecord =
                    MoveSequence prev { current | notes = msg :: current.notes } next
            in
                { game | gameRecord = newRecord }


chatItems : Game -> List String
chatItems game =
    let
        moveToChatItem moveRecord =
            case moveRecord.move of
                ( White, Pass ) ->
                    "White pass" :: moveRecord.notes

                _ ->
                    moveRecord.notes
    in
        case game.gameRecord of
            MoveSequence prev current next ->
                (List.concatMap moveToChatItem (prev ++ (current :: next)))

            _ ->
                []


type alias Game =
    { board : Board
    , capturedStones : Dict Player Int
    , gameRecord : GameRecord
    , currentPlayer : Player
    , rules : List Rule
    , pendingMove : Maybe Move
    }


type alias MoveInProgress =
    { provisionalBoard : Board, currentMove : Move, game : Game }


type Rule
    = Rule (MoveInProgress -> Result String MoveInProgress)


newGame : Int -> Game
newGame boardSize =
    { board = Board.new boardSize
    , capturedStones = Dict.empty
    , gameRecord = NotStarted
    , rules = [ Rule placePlayer, Rule onePlayerPerTurnRule, Rule oneStonePerPointRule, Rule captureRule ]
    , currentPlayer = Black
    , pendingMove = Nothing
    }


type alias StartingStones =
    { black : List Point
    , white : List Point
    , liberties : List Point
    }


newGameWithStones : Int -> StartingStones -> Game
newGameWithStones size startingStones =
    let
        game =
            newGame size

        boardWithBlack =
            List.foldl (Board.place Black) game.board startingStones.black

        boardWithBoth =
            List.foldl (Board.place White) boardWithBlack startingStones.white

        annotate point =
            ( point, LibertyCount )

        board =
            Board.annotateMany LibertyCount startingStones.liberties boardWithBoth
    in
        { game | board = board }


newMoveRecord : Move -> MoveRecord
newMoveRecord move =
    { move = move, notes = [] }


pushMoveRecord : MoveRecord -> GameRecord -> GameRecord
pushMoveRecord moveRecord gameRecord =
    case gameRecord of
        MoveSequence prev current next ->
            MoveSequence (prev ++ [ current ]) moveRecord []

        NotStarted ->
            MoveSequence [] moveRecord []


applyRules : Game -> Move -> Result String MoveInProgress
applyRules game move =
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


nextPlayer : Player -> Player
nextPlayer player =
    case player of
        Black ->
            White

        White ->
            Black


playMove : Game -> Move -> Result Game Game
playMove game move =
    let
        result =
            applyRules game move
    in
        case result of
            Ok moveInProgress ->
                Ok { game | board = moveInProgress.provisionalBoard, gameRecord = pushMoveRecord (newMoveRecord move) game.gameRecord, currentPlayer = nextPlayer game.currentPlayer }

            Err msg ->
                Err (addMessage game msg)


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
                        Board.removeDeadNeighbors inProgress.provisionalBoard point
                in
                    Ok { inProgress | provisionalBoard = newBoard }

            _ ->
                Ok inProgress
