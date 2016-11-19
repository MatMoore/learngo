module Types exposing (..)

import Game.Types exposing (..)
import Board exposing (Player(..), Annotation(..), Point)
import AI exposing (Strategy)


type alias Flags =
    { black : List Point
    , white : List Point
    , liberties : List Point
    , strategy : String
    }


type alias Model =
    { game : Game
    , strategy : Maybe Strategy
    , pendingMove : Maybe Move
    }
