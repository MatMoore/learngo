module AI exposing (Strategy, generateMove, getStrategy)

import Board exposing (Player, Point)
import Game.Types exposing (Game, Move, Action(..), GameMessage(..))
import Random
import Maybe
import Array
import Dict
import Task
import Array exposing (Array)


type Strategy
    = AlwaysPass
    | RandomStone



-- Consider all captures first, order by number captures + number atari
-- If no capture, atari
-- If no atari, random
-- | AlwaysCapture


type alias GameState =
    ( Game, Random.Seed )


getStrategy : String -> Maybe Strategy
getStrategy name =
    case name of
        "always-pass" ->
            Just AlwaysPass

        "random-stone" ->
            Just RandomStone

        _ ->
            Nothing


moveCommand : Move -> Cmd GameMessage
moveCommand move =
    Task.perform ComputerPlay (Task.succeed move)


generateMove : Maybe Strategy -> Player -> Game -> Cmd GameMessage
generateMove strategy player game =
    case strategy of
        Just AlwaysPass ->
            moveCommand ( player, Pass )

        Just RandomStone ->
            let
                possibleMoves =
                    Array.map (\point -> ( player, Play point )) (getFreeSpaces game)

                generator =
                    moveGenerator ( player, Pass ) possibleMoves
            in
                Random.generate ComputerPlay generator

        Nothing ->
            Cmd.none


moveGenerator : Move -> Array Move -> Random.Generator Move
moveGenerator defaultMove possibleMoves =
    let
        totalMoves : Int
        totalMoves =
            Array.length possibleMoves

        indexGenerator : Random.Generator Int
        indexGenerator =
            Random.int 0 (totalMoves - 1)

        lookupMove : Int -> Move
        lookupMove i =
            Maybe.withDefault defaultMove (Array.get i possibleMoves)
    in
        Random.map lookupMove indexGenerator


getFreeSpaces : Game -> Array Point
getFreeSpaces game =
    Array.fromList (List.filter (pointIsEmpty game) (Board.points game.board))


pointIsEmpty : Game -> Point -> Bool
pointIsEmpty game point =
    not (Board.isFilled point game.board)
