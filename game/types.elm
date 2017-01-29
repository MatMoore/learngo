module Game.Types exposing (..)

{-| Module Game.Types defines the core types for a game of Go.

# Moves
@docs Action, Move

# Messages
@docs GameMessage

# Rules
@docs MoveInProgress, Rule, Ruleset

# Game records
@docs MoveRecord, GameRecord, Game
-}

import Dict exposing (Dict)
import Board exposing (Player(..), Point, Board, Annotation(..))
import Time exposing (Time)


{-| Represents a move that has not yet been accepted.
-}
type alias MoveInProgress =
    { provisionalBoard : Board, currentMove : Move, game : Game }


{-| A function that tries to do something with a provisional move, and fails if the move is not valid.
-}
type Rule
    = Rule (MoveInProgress -> Result String MoveInProgress)


{-| Several rules that should be used for a game.
-}
type alias Ruleset =
    List Rule


{-| Actions the player can take on their turn.
-}
type Action
    = Play Point
    | Pass
    | Resign


{-| A move made by a player.
-}
type alias Move =
    ( Player, Action )


{-| Messages that can be received by the game's update function.
-}
type GameMessage
    = UserPlay Point
    | ComputerPlay Move
    | Tick Time


{-| A move with comments.
-}
type alias MoveRecord =
    { move : Move
    , notes : List String
    }


{-| A record of all the moves made in a game.
-}
type GameRecord
    = MoveSequence (List MoveRecord) MoveRecord (List MoveRecord)
    | NotStarted


{-| A record storing all game state.
-}
type alias Game =
    { board : Board
    , capturedStones : Dict Player Int
    , gameRecord : GameRecord
    , currentPlayer : Player
    , rules : Ruleset
    }
