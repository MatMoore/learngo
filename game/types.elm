module Game.Types exposing (..)

{-| Module Game.Types defines the core types for a game of Go.

# Moves
@docs Action, Move

# Messages
@docs GameMessage

# Rules
@docs MoveInProgress

# Game records
@docs Event, Log

# Game state
@docs Game
-}

import Dict exposing (Dict)
import Board exposing (Player(..), Point, Board, Annotation(..))
import Time exposing (Time)


{-| Represents a move that has not yet been accepted.
-}
type alias MoveInProgress =
    { provisionalBoard : Board, currentMove : Move, game : Game }


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


{-| A record of something that happened in the game
-}
type Event
    = PlayEvent Move
    | NoteEvent String


{-| A record of all the things that happened in a game.
-}
type alias Log =
    List Event


{-| A record storing all game state.
-}
type alias Game =
    { board : Board
    , capturedStones : Dict Player Int
    , log : Log
    , currentPlayer : Player
    }
