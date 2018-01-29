module Mastermind exposing (main)

import Mastermind.Model as Model exposing (..)
import Mastermind.Update as Update exposing (..)
import Mastermind.View as View exposing (..)

import Html exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none







