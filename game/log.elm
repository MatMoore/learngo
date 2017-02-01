module Game.Log exposing (addNote, addMove, notes)

{-| Game.Log manages a log of the game.
# Adding moves to the log
@docs addMove

# Adding notes to the game
@docs addNote

# Showing notes
@docs notes
-}

import Game.Types exposing (..)
import Board exposing (Player(..))


{-| Add a message to the log.
-}
addNote : String -> Log -> Log
addNote msg log =
    NoteEvent msg :: log


{-| Retrieve all notes from the log, including player passes.
-}
notes : Log -> List String
notes log =
    let
        extractNote event =
            case event of
                NoteEvent msg ->
                    Just msg

                PlayEvent ( player, Pass ) ->
                    Just "White pass"

                _ ->
                    Nothing
    in
        List.filterMap extractNote log


{-| Add a move to the log.
-}
addMove : Move -> Log -> Log
addMove move log =
    PlayEvent move :: log
