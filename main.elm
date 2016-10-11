module Main exposing (..)

--import Html.App exposing (programWithFlags)

import Html.App exposing (program)
import Game exposing (..)
import View exposing (view)
import AI exposing (generatePass)


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


update : GameMessage -> Game -> ( Game, Cmd a )
update msg game =
    case msg of
        UserPlay point ->
            let
                newGame =
                    playMove game ( Black, Play point )

                newGame2 =
                    generatePass White newGame
            in
                ( newGame2, Cmd.none )


subscriptions model =
    Sub.none


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
