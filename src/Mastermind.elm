module Mastermind exposing (Model, Msg, update, view, subscriptions, init)


import Html exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { name : String
    }

type Msg = Message1 | Message2



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Message1 ->
            (model, Cmd.none)

        Message2 ->
            (model, Cmd.none)


view : Model -> Html Msg
view model =
    div []
        [ text "New Html Program"
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : (Model, Cmd Msg)
init = 
    (Model "jonathan", Cmd.none)

