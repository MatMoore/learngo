module AI exposing (AI, generateMove, Strategy(..))

import Game exposing (Game, Move, Player, Action(..), playMove, GameMessage(..), Point)
import Random
import Maybe
import Array
import Dict
import Array exposing (Array)


type Strategy
    = AlwaysPass
    | RandomStone


type alias AI =
    ( Strategy, Player )


type alias GameState =
    ( Game, Random.Seed )


generateMove : AI -> Game -> ( Game, Cmd GameMessage )
generateMove ( strategy, player ) game =
    case strategy of
        AlwaysPass ->
            ( playMove game ( player, Pass ), Cmd.none )

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
    let
        allPoints =
            cartesian [0..game.boardSize - 1] [0..game.boardSize - 1]
    in
        Array.fromList (List.filter (pointIsEmpty game) allPoints)


pointIsEmpty : Game -> Point -> Bool
pointIsEmpty game point =
    not (Dict.member point game.boardStones)


cartesian : List a -> List b -> List ( a, b )
cartesian xs ys =
    List.concatMap
        (\x -> List.map (\y -> ( x, y )) ys)
        xs
