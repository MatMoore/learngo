module Main exposing (..)

--import Html.App exposing (programWithFlags)

import Html.App exposing (program)
import Game exposing (..)
import View exposing (view)


type alias Flags =
    { size : Int }



--init : Flags -> ( Model, Cmd Msg )
--init flags =
--    ( { size = flags.size, stones = Dict.empty }
--    , Cmd.none
--    )


init : ( Game, Cmd GameMessage )
init =
    ( playMove (newGame 9) ( Black, Play ( 2, 2 ) )
    , Cmd.none
    )


update : GameMessage -> Game -> ( Game, Cmd a )
update msg game =
    case msg of
        UserPlay point ->
            let
                newGame =
                    playMove game ( Black, Play point )
            in
                ( newGame, Cmd.none )


subscriptions model =
    Sub.none


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
