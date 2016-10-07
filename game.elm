module Game exposing (..)

import Dict exposing (Dict)
import Array exposing (Array)


type alias Point =
    ( Int, Int )


type Stone
    = Black
    | White


type Move
    = Play Point Stone
    | Pass Stone
    | Resign Stone


type MoveRecord
    = Commented Move String
    | Uncommented Move


type GameRecord
    = LastMove (Array MoveRecord) Int
    | NotStarted


type alias Board =
    Dict Point Stone


type alias Game =
    { boardStones : Board
    , capturedStones : Dict Stone Int
    , boardSize : Int
    , gameRecord : GameRecord
    }


addMove : Game -> Move -> Game
addMove game move =
    let
        moveRecord =
            Uncommented move

        newGameRecord =
            case game.gameRecord of
                LastMove moves idx ->
                    LastMove (Array.push moveRecord moves) (idx + 1)

                NotStarted ->
                    LastMove (Array.fromList [ moveRecord ]) 0

        newBoard =
            case move of
                Play point stone ->
                    Dict.insert point stone game.boardStones

                _ ->
                    game.boardStones
    in
        { game | boardStones = newBoard, gameRecord = newGameRecord }


newGame : Int -> Game
newGame boardSize =
    { boardSize = boardSize
    , boardStones = Dict.empty
    , capturedStones = Dict.empty
    , gameRecord = NotStarted
    }
