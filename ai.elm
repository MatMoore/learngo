module AI exposing (AI, generateMove, generatePass)

import Game exposing (Game, Move, Player, Action(..), playMove)


type Strategy
    = AlwaysPass


type alias AI =
    ( Strategy, Player )


generateMove : AI -> Game -> Game
generateMove ( AlwaysPass, player ) game =
    playMove game ( player, Pass )


generatePass : Player -> Game -> Game
generatePass player =
    generateMove ( AlwaysPass, player )
