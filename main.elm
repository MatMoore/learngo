module Main exposing (..)

--import Html.App exposing (programWithFlags)

import Html.App exposing (program)
import Game exposing (..)
import View exposing (view)


type Msg
    = NotImplemented


type alias Flags =
    { size : Int }



--init : Flags -> ( Model, Cmd Msg )
--init flags =
--    ( { size = flags.size, stones = Dict.empty }
--    , Cmd.none
--    )


init : ( Game, Cmd Msg )
init =
    ( addMove (newGame 9) (Play ( 2, 2 ) Black)
    , Cmd.none
    )


update msg model =
    ( model, Cmd.none )


subscriptions model =
    Sub.none


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
