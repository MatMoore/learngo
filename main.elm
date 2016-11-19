module Main exposing (..)

import Html exposing (programWithFlags)
import Game.Api as Game
import Game.Types exposing (..)
import Types exposing (..)
import View exposing (view)
import AI exposing (generateMove, Strategy, getStrategy)
import Time exposing (millisecond)
import Board exposing (Player(..), Annotation(..), Point)
import Game.Record exposing (addMessage)


init : Flags -> ( Model, Cmd GameMessage )
init flags =
    ( newModelFromFlags flags
    , Cmd.none
    )


update : GameMessage -> Model -> ( Model, Cmd GameMessage )
update msg model =
    case msg of
        UserPlay point ->
            let
                result =
                    Game.play ( Black, Play point ) model.game
            in
                case result of
                    Ok newGame ->
                        ( { model | game = newGame }, generateMove model.strategy White newGame )

                    Err game ->
                        ( { model | game = game }, Cmd.none )

        ComputerPlay move ->
            ( { model | pendingMove = Just move }, Cmd.none )

        Tick time ->
            case model.pendingMove of
                Just move ->
                    let
                        result =
                            Game.play move model.game
                    in
                        case result of
                            Ok newGame ->
                                ( { model | game = newGame, pendingMove = Nothing }, Cmd.none )

                            Err game ->
                                {--( { game | gameRecord = addMessage game.gameRecord "AI got confused :/" }, Cmd.none ) --}
                                ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


subscriptions model =
    Time.every (500 * millisecond) Tick


newModelFromFlags : Flags -> Model
newModelFromFlags flags =
    let
        size =
            9

        game =
            Game.new size

        boardWithBlack =
            List.foldl (Board.place Black) game.board flags.black

        boardWithBoth =
            List.foldl (Board.place White) boardWithBlack flags.white

        annotate point =
            ( point, LibertyCount )

        board =
            Board.annotateMany LibertyCount flags.liberties boardWithBoth

        newGame =
            { game | board = board }

        strategy =
            getStrategy flags.strategy
    in
        { game = newGame, strategy = strategy, pendingMove = Nothing }


main =
    programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
