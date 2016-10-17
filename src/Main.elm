module Main exposing (..)

import Html exposing (text, Html)
import Html.App as App


main =
    App.program { init = init "", update = update, view = view, subscriptions = \_ -> Sub.none }


type alias Model =
    { githubName : String
    }


type Msg
    = NoOp


init githubName =
    ( Model githubName, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    text "Hello World"
