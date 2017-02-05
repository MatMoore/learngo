module Board
    exposing
        ( Point
        , Player(..)
        , Annotation(..)
        , Board
        , nextPlayer
        , new
        , place
        , remove
        , annotate
        , annotateMany
        , isFilled
        , annotations
        , stones
        , points
        , stoneAt
        , neighbors
        , friendlyNeighbors
        , hostileNeighbors
        , liberties
        )

{-| This module models a standard Go board and the stones a player places on it.

# Players
@docs Player, nextPlayer

# Points
@docs Point

# Boards

@docs Board

## Create a board
@docs new

## Placing and removing stones
@docs place, remove, stones, stoneAt

## Querying the board
@docs isFilled, points

## Annotating points
@docs Annotation, annotate, annotateMany, annotations

## Connectivity
@docs neighbors, friendlyNeighbors, hostileNeighbors, liberties
-}

import Dict exposing (Dict)
import Set exposing (Set)


{-| The Player uses stones of one color: black or white.
-}
type Player
    = Black
    | White


{-| Next player defines the turn order
-}
nextPlayer : Player -> Player
nextPlayer player =
    case player of
        Black ->
            White

        White ->
            Black


{-| A point is supposed to reference somewhere a stone can be placed on the board. We don't strictly enforce the bounds of Points.
-}
type alias Point =
    ( Int, Int )


{-| A board encapsulates a bunch of points you can place stones on, and the connections between them.
-}
type Board
    = SquareGrid Int (Dict Point Player) (Dict Point Annotation)


{-| An annotation marks a point on the board to explain something. It is cleared when a stone is placed or removed.
-}
type Annotation
    = LibertyCount


{-| Create a square grid. Size is assumed to be positive.
-}
new : Int -> Board
new size =
    SquareGrid size Dict.empty Dict.empty


{-| Mark a point on the board.
-}
annotate : Annotation -> Point -> Board -> Board
annotate annotation point (SquareGrid size stones annotations) =
    SquareGrid size stones (Dict.insert point annotation annotations)


{-| Mark multiple points on the board with the same annotation.
-}
annotateMany : Annotation -> List Point -> Board -> Board
annotateMany annotation points board =
    List.foldl (annotate annotation) board points


{-| Place the player's stone at any point on the board. This replaces a stone that is already there.
-}
place : Player -> Point -> Board -> Board
place player point (SquareGrid size stones annotations) =
    SquareGrid size (Dict.insert point player stones) (Dict.remove point annotations)


{-| Remove any stone at a given point on the board. Does nothing if the point is empty.
-}
remove : Point -> Board -> Board
remove point (SquareGrid size stones annotations) =
    SquareGrid size (Dict.remove point stones) (Dict.remove point annotations)


{-| Look for a stone at a point on the board
-}
stoneAt : Point -> Board -> Maybe Player
stoneAt point (SquareGrid _ stones _) =
    Dict.get point stones


{-| Is a point on the board filled?
-}
isFilled : Point -> Board -> Bool
isFilled point (SquareGrid size stones annotations) =
    Dict.member point stones


{-| List all the annotations on the board.
-}
annotations : Board -> List ( Point, Annotation )
annotations (SquareGrid _ _ result) =
    Dict.toList result


{-| List all the stones on the board.
-}
stones : Board -> List ( Point, Player )
stones (SquareGrid _ result _) =
    Dict.toList result


{-| List all the points on the board, regardless of whether there are stones on them.
-}
points : Board -> List Point
points (SquareGrid size _ _) =
    cartesian (List.range 0 (size - 1)) (List.range 0 (size - 1))


{-| List all points on the board that are connected to the specified point.
-}
neighbors : Point -> Board -> List Point
neighbors ( x, y ) (SquareGrid size _ _) =
    let
        possibles =
            [ ( x + 1, y ), ( x - 1, y ), ( x, y + 1 ), ( x, y - 1 ) ]

        fits ( x_, y_ ) =
            (x_ >= 0) && (y_ >= 0) && (x_ < size) && (y_ < size)
    in
        List.filter fits possibles


{-| List all the points that are connected to the specified point and are the same color.
-}
friendlyNeighbors : Point -> Board -> List Point
friendlyNeighbors point board =
    let
        maybePlayer =
            stoneAt point board

        samePlayer newPoint =
            (stoneAt newPoint board) == maybePlayer
    in
        case maybePlayer of
            Just player ->
                List.filter samePlayer (neighbors point board)

            Nothing ->
                []


{-| List all the points that are connected to the specified point and are not the same color.
-}
hostileNeighbors : Point -> Board -> List Point
hostileNeighbors point board =
    let
        maybePlayer =
            stoneAt point board

        isHostile newPoint =
            let
                maybeOther =
                    stoneAt newPoint board
            in
                maybePlayer /= maybeOther
    in
        case maybePlayer of
            Just player ->
                List.filter isHostile (neighbors point board)

            Nothing ->
                []


{-| List all the empty points that are connected to a specified point
-}
liberties : Point -> Board -> Set Point
liberties point board =
    let
        notFilled point =
            not (isFilled point board)
    in
        Set.fromList (List.filter notFilled (neighbors point board))


cartesian : List a -> List b -> List ( a, b )
cartesian xs ys =
    List.concatMap
        (\x -> List.map (\y -> ( x, y )) ys)
        xs
