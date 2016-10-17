module Main exposing (..)

import Html exposing (text, Html, nav, i, a, li, div, input, form, b, p, h5)
import Html.Attributes exposing (class, attribute, href, type', placeholder, id, name)
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
    div []
        [ profile model
        , searchform model
        ]


profile model =
    div [ class "m-x-auto" ]
        [ div [ class "starter-template text-xs-center" ]
            [ h5 []
                [ text "Search any user" ]
            , p [ class "lead" ]
                [ text "Please enter a "
                , b []
                    [ text "username" ]
                , text "."
                ]
            ]
        ]


searchform model =
    nav [ class "navbar navbar-fixed-top navbar-dark bg-inverse" ]
        [ div [ class "row pull-xs-right" ]
            [ div [ class "col-md-3 hidden-md-down" ]
                [ a [ class "navbar-brand", href "#" ]
                    [ text "Github Search Profile" ]
                ]
            , div [ class " col-md-6" ]
                [ form [ id "searchForm" ]
                    [ div [ class "form-group" ]
                        [ div [ class "input-group" ]
                            [ input [ class "form-control", name "username", placeholder "Enter A Github Username...", type' "text" ]
                                []
                            , div [ class "input-group-addon" ]
                                [ i [ attribute "aria-hidden" "true", class "fa fa-keyboard-o" ]
                                    []
                                ]
                            ]
                        ]
                    ]
                ]
            , div [ class "col-md-3" ]
                [ nav [ class "nav navbar-nav  pull-xs-right" ]
                    [ li [ class "nav-item" ]
                        [ a [ class "nav-link", href "https://twitter.com/_tipek" ]
                            [ i [ attribute "aria-hidden" "true", class "fa fa-twitter" ]
                                []
                            , text "Twitter"
                            ]
                        ]
                    , li [ class "nav-item" ]
                        [ a [ class "nav-link", href "https://github.com/tahaipek" ]
                            [ i [ attribute "aria-hidden" "true", class "fa fa-github" ]
                                []
                            , text "Github"
                            ]
                        ]
                    ]
                ]
            ]
        ]
