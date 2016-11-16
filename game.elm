module Game exposing (..)

import Random
import Dict exposing (Dict)
import Array exposing (Array)
import Maybe
import Time exposing (Time)
import Debug exposing (log)


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
    | ComputerPlay Move
    | Tick Time


type alias Board =
    Dict Point Player


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


type Annotation
    = LibertyCount


type alias Game =
    { boardStones : Board
    , capturedStones : Dict Player Int
    , boardSize : Int
    , gameRecord : GameRecord
    , currentPlayer : Player
    , rules : List Rule
    , pendingMove : Maybe Move
    , annotations : Dict Point Annotation
    }


type alias MoveInProgress =
    { provisionalBoard : Board, currentMove : Move, game : Game }


type Rule
    = Rule (MoveInProgress -> Result String MoveInProgress)


newGame : Int -> Game
newGame boardSize =
    { boardSize = boardSize
    , boardStones = Dict.empty
    , capturedStones = Dict.empty
    , gameRecord = NotStarted
    , rules = [ Rule placePlayer, Rule onePlayerPerTurnRule, Rule oneStonePerPointRule, Rule captureRule ]
    , currentPlayer = Black
    , pendingMove = Nothing
    , annotations = Dict.empty
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

        insertColor color stone board =
            Dict.insert stone color board

        boardWithBlack =
            List.foldl (insertColor Black) game.boardStones startingStones.black

        boardWithBoth =
            List.foldl (insertColor White) boardWithBlack startingStones.white

        annotate point =
            ( point, LibertyCount )
    in
        { game | boardStones = boardWithBoth, annotations = Dict.fromList (List.map annotate startingStones.liberties) }


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
            Ok { provisionalBoard = game.boardStones, currentMove = move, game = game }

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
                Ok { game | boardStones = moveInProgress.provisionalBoard, gameRecord = pushMoveRecord (newMoveRecord move) game.gameRecord, currentPlayer = nextPlayer game.currentPlayer }

            Err msg ->
                Err (addMessage game msg)


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


oneStonePerPointRule : MoveInProgress -> Result String MoveInProgress
oneStonePerPointRule inProgress =
    let
        ( player, action ) =
            inProgress.currentMove

        board =
            inProgress.game.boardStones
    in
        case action of
            Play point ->
                if Dict.member point board then
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
                        removeDead game.boardSize inProgress.provisionalBoard point
                in
                    Ok { inProgress | provisionalBoard = newBoard }

            _ ->
                Ok inProgress


removeDead : Int -> Board -> Point -> Board
removeDead size board origin =
    let
        isFilled : Point -> Bool
        isFilled point =
            Dict.member point board

        candidates : List Point
        candidates =
            List.filter isFilled (neighbours size origin)

        removeOne : Point -> Board -> Board
        removeOne point board =
            if List.isEmpty (liberties board size point) then
                Dict.remove point board
            else
                board
    in
        List.foldl removeOne board candidates


neighbours : Int -> Point -> List Point
neighbours size point =
    let
        ( x, y ) =
            point

        possibles =
            [ ( x + 1, y ), ( x - 1, y ), ( x, y + 1 ), ( x, y - 1 ) ]

        fits ( x_, y_ ) =
            (x_ >= 0) && (y_ >= 0) && (x_ < size) && (y_ < size)
    in
        List.filter fits possibles


liberties : Board -> Int -> Point -> List Point
liberties board size point =
    let
        pointNeighbours =
            (neighbours size point)
    in
        List.filter (\point -> (Dict.get point board) == Nothing) pointNeighbours
