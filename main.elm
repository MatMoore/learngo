module Main exposing (..)

import Html exposing (programWithFlags)
import Game.Api as Game
import Game.Types exposing (..)
import View exposing (view)
import AI exposing (generateMove, Strategy(..))
import Time exposing (millisecond)
import Board exposing (Player(..), Annotation(..), Point)
import Game.Record exposing (addMessage)


init : StartingStones -> ( Game, Cmd GameMessage )
init startingStones =
    ( newGameWithStones 9 startingStones
    , Cmd.none
    )


update : GameMessage -> Game -> ( Game, Cmd GameMessage )
update msg game =
    case msg of
        UserPlay point ->
            let
                result =
                    Game.play ( Black, Play point ) game
            in
                case result of
                    Ok newGame ->
                        generateMove ( RandomStone, White ) newGame

                    Err game ->
                        ( game, Cmd.none )

        ComputerPlay move ->
            ( { game | pendingMove = Just move }, Cmd.none )

        Tick time ->
            case game.pendingMove of
                Just move ->
                    let
                        result =
                            Game.play move game
                    in
                        case result of
                            Ok newGame ->
                                ( { newGame | pendingMove = Nothing }, Cmd.none )

                            Err game ->
                                ( { game | gameRecord = addMessage game.gameRecord "AI got confused :/" }, Cmd.none )

                Nothing ->
                    ( game, Cmd.none )


subscriptions model =
    Time.every (500 * millisecond) Tick


type alias StartingStones =
    { black : List Point
    , white : List Point
    , liberties : List Point
    }


newGameWithStones : Int -> StartingStones -> Game
newGameWithStones size startingStones =
    let
        game =
            Game.new size

        boardWithBlack =
            List.foldl (Board.place Black) game.board startingStones.black

        boardWithBoth =
            List.foldl (Board.place White) boardWithBlack startingStones.white

        annotate point =
            ( point, LibertyCount )

        board =
            Board.annotateMany LibertyCount startingStones.liberties boardWithBoth
    in
        { game | board = board }


main =
    programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
