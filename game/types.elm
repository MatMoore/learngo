module Game.Types exposing (..)

import Dict exposing (Dict)
import Board exposing (Player(..), Point, Board, Annotation(..))
import Time exposing (Time)


type alias MoveInProgress =
    { provisionalBoard : Board, currentMove : Move, game : Game }


type Rule
    = Rule (MoveInProgress -> Result String MoveInProgress)


type alias Ruleset =
    List Rule


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


type alias Game =
    { board : Board
    , capturedStones : Dict Player Int
    , gameRecord : GameRecord
    , currentPlayer : Player
    , rules : Ruleset
    , pendingMove : Maybe Move
    }
