module Main exposing (..)

--import Html.App exposing (programWithFlags)

import Html.App exposing (program)
import Game exposing (..)
import View exposing (view)
import AI exposing (generateMove, Strategy(..))
import Time exposing (millisecond)


type alias Flags =
    { size : Int }



--init : Flags -> ( Model, Cmd Msg )
--init flags =
--    ( { size = flags.size, stones = Dict.empty }
--    , Cmd.none
--    )


init : ( Game, Cmd GameMessage )
init =
    ( newGame 9
    , Cmd.none
    )


update : GameMessage -> Game -> ( Game, Cmd GameMessage )
update msg game =
    case msg of
        UserPlay point ->
            let
                result =
                    playMove game ( Black, Play point )
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
                            playMove game move
                    in
                        case result of
                            Ok newGame ->
                                ( { newGame | pendingMove = Nothing }, Cmd.none )

                            Err game ->
                                ( addMessage game "AI got confused :/", Cmd.none )

                Nothing ->
                    ( game, Cmd.none )


subscriptions model =
    Time.every (500 * millisecond) Tick


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
