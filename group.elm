module Group
    exposing
        ( removeDead
        , removeDeadNeighbors
        , liberties
        )

{-| This module models groups of connected stones on a go board.
-}

import Board
    exposing
        ( Player(..)
        , Board
        , Point
        , isFilled
        , stoneAt
        , neighbors
        , friendlyNeighbors
        , hostileNeighbors
        )
import Set exposing (Set)
import Debug exposing (log)


{-| A connected group of stones of the same colour.
-}
type alias Group =
    { board : Board
    , points : Set Point
    , owner : Player
    }


{-| Look for a connected group of stones at a point on the board
-}
groupAt : Point -> Board -> Maybe Group
groupAt point board =
    case stoneAt point board of
        Just player ->
            Just (growGroup (Set.singleton point) { board = board, points = Set.empty, owner = player })

        Nothing ->
            Nothing


{-| Take a partially constructed group and expand it to include all stones of the same color that are connected to it. This is a helper function for groupAt.
-}
growGroup : Set Point -> Group -> Group
growGroup frontier group =
    let
        neighborStones : Point -> List Point
        neighborStones point =
            friendlyNeighbors point group.board

        frontierNeighbors : Set Point
        frontierNeighbors =
            Set.fromList (List.concatMap neighborStones (Set.toList frontier))

        newFrontier : Set Point
        newFrontier =
            Set.diff frontierNeighbors group.points

        newGroup =
            { group | points = Set.union group.points frontier }
    in
        if Set.isEmpty newFrontier then
            newGroup
        else
            growGroup newFrontier newGroup


{-| Remove a group if it has no liberties. This is a no-op if the point is empty or it contains a living group.
-}
removeDead : Point -> Board -> Board
removeDead point board =
    let
        maybeGroup =
            log "group" (groupAt point board)
    in
        case maybeGroup of
            Just group ->
                if Set.isEmpty (sharedLiberties group) then
                    Set.foldl Board.remove board group.points
                else
                    board

            Nothing ->
                board


{-| Remove neighbors of a point if they have no liberties. Treats all stones independently ignoring groups.
-}
removeDeadNeighbors : Point -> Board -> Board
removeDeadNeighbors point board =
    List.foldl removeDead board (hostileNeighbors point board)


{-| List all the points that are connected to stones in a group
-}
sharedLiberties : Group -> Set Point
sharedLiberties group =
    let
        boardLiberties point =
            Set.toList (liberties point group.board)

        libertiesForStones =
            List.concatMap boardLiberties (Set.toList group.points)
    in
        Set.fromList libertiesForStones


{-| List all the points that are connected to a specified point and are empty. DEPRECATED - use `sharedLiberties` instead?
-}
liberties : Point -> Board -> Set Point
liberties point board =
    let
        notFilled point =
            not (isFilled point board)
    in
        Set.fromList (List.filter notFilled (neighbors point board))
