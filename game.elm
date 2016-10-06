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


type alias Game =
    { boardStones : Dict Point Stone
    , capturedStones : Dict Stone Int
    , boardSize : Int
    , gameRecord : GameRecord
    }


newGame : Int -> Game
newGame boardSize =
    { boardSize = boardSize
    , boardStones = Dict.empty
    , capturedStones = Dict.empty
    , gameRecord = NotStarted
    }
