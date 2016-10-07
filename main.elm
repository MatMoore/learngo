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


init : ( Game, Cmd GameMsg )
init =
    ( addMove (newGame 9) (Play ( 2, 2 ) Black)
    , Cmd.none
    )


update : GameMsg -> Game -> ( Game, Cmd a )
update msg game =
    case msg of
        PlayUserStone point ->
            let
                newGame =
                    addMove game (Play point Black)
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
