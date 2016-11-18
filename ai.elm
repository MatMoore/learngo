module AI exposing (AI, generateMove, Strategy(..))

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


type alias AI =
    ( Strategy, Player )


type alias GameState =
    ( Game, Random.Seed )


moveCommand : Move -> Cmd GameMessage
moveCommand move =
    Task.perform ComputerPlay (Task.succeed move)


generateMove : AI -> Game -> ( Game, Cmd GameMessage )
generateMove ( strategy, player ) game =
    case strategy of
        AlwaysPass ->
            ( game, moveCommand ( player, Pass ) )

        RandomStone ->
            let
                possibleMoves =
                    Array.map (\point -> ( player, Play point )) (getFreeSpaces game)

                generator =
                    moveGenerator ( player, Pass ) possibleMoves

                cmd =
                    Random.generate ComputerPlay generator
            in
                ( game, cmd )


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
