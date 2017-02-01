module Game.Api exposing (new, play)

{-| Game.Api defines basic functions for playing a game of Go.

# Starting a new game
@docs new

# Playing a move
@docs play
-}

import Game.Types exposing (..)
import Game.Rules
import Dict exposing (Dict)
import Board exposing (Player(..), nextPlayer)


{-| Create a game
-}
new : Int -> Game
new boardSize =
    { board = Board.new boardSize
    , capturedStones = Dict.empty
    , gameRecord = []
    , currentPlayer = Black
    }


{-| Attempt to play a move.
-}
play : Move -> Game -> Result Game Game
play =
    Game.Rules.play
