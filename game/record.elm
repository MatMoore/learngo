module Game.Record exposing (addMessage, addMove, chatItems)

{-| Game.Record manages a log of the game.
# Adding moves to the record
@docs addMove

# Commenting on the game
@docs addMessage

# Showing text
@docs chatItems
-}

import Game.Types exposing (..)
import Board exposing (Player(..))


{-| Add a message to the game record.
-}
addMessage : Log -> String -> Log
addMessage gameRecord msg =
    NoteEvent msg :: gameRecord


{-| Retrieve all items from the game record, including player passes.
-}
chatItems : Game -> List String
chatItems game =
    let
        extractNote event =
            case event of
                NoteEvent msg ->
                    Just msg

                _ ->
                    Nothing
    in
        List.filterMap extractNote game.gameRecord


{-| Add a move to the game record.
-}
addMove : Move -> Log -> Log
addMove move gameRecord =
    PlayEvent move :: gameRecord
