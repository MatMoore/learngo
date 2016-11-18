module Game.Record exposing (addMessage, addMove, chatItems)

import Game.Types exposing (..)
import Board exposing (Player(..))


addMessage : GameRecord -> String -> GameRecord
addMessage gameRecord msg =
    case gameRecord of
        NotStarted ->
            gameRecord

        MoveSequence prev current next ->
            MoveSequence prev { current | notes = msg :: current.notes } next


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


addMove : Move -> GameRecord -> GameRecord
addMove move gameRecord =
    let
        moveRecord =
            { move = move, notes = [] }
    in
        case gameRecord of
            MoveSequence prev current next ->
                MoveSequence (prev ++ [ current ]) moveRecord []

            NotStarted ->
                MoveSequence [] moveRecord []
