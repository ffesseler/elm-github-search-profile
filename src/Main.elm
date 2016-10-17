module Main exposing (..)

import Html exposing (text, Html, nav, i, a, li, div, input, form, b, p, h5)
import Html.Attributes exposing (class, attribute, href, type', placeholder, id, name)
import Html.Events exposing (onInput)
import Html.App as App
import Http
import Json.Decode as Decode exposing ((:=))
import Task
import String exposing (length)


main =
    App.program { init = init "" (Credentials "" ""), update = update, view = view, subscriptions = \_ -> Sub.none }


type alias Model =
    { githubName : String
    , credentials : Credentials
    , profile : Maybe GithubProfile
    , error : Maybe Http.Error
    }


type alias Credentials =
    { clientId : String
    , clientSecret : String
    }


type alias GithubProfile =
    { name : String
    , repos_url : String
    }


type alias Repository =
    { name : String
    }


type Msg
    = SearchUser String
    | FetchUserDone (GithubProfile)
    | FetchUserFail Http.Error


init githubName credentials =
    ( Model githubName credentials Nothing Nothing, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchUser githubName ->
            if length githubName > 0 then
                ( model, fetchGithubUser githubName model.credentials )
            else
                ( { model | error = Nothing, profile = Nothing }, Cmd.none )

        FetchUserDone profile ->
            ( { model | profile = Just profile, error = Nothing }, Cmd.none )

        FetchUserFail error ->
            Debug.log "fail" ( { model | error = Just error }, Cmd.none )


fetchGithubUser githubName credentials =
    Http.get profileDecoder (fetchGithubUserUrl githubName credentials)
        |> Task.perform FetchUserFail FetchUserDone


fetchGithubUserUrl : String -> Credentials -> String
fetchGithubUserUrl name credentials =
    "http://api.github.com/users/" ++ name ++ "?client_id=" ++ credentials.clientId ++ "&client_secret=" ++ credentials.clientSecret


profileDecoder : Decode.Decoder (GithubProfile)
profileDecoder =
    Decode.object2 GithubProfile
        ("name" := Decode.string)
        ("repos_url" := Decode.string)


view : Model -> Html Msg
view model =
    div []
        [ profile model
        , searchform model
        ]


profile model =
    case model.error of
        Just error ->
            div [ class "m-x-auto" ]
                [ div [ class "starter-template text-xs-center" ]
                    [ h5 []
                        [ text "User Profile not found." ]
                    , p [ class "lead" ]
                        [ text "Please enter a different "
                        , b []
                            [ text "username" ]
                        , text "."
                        ]
                    ]
                ]

        Nothing ->
            case model.profile of
                Nothing ->
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

                Just profile ->
                    div []
                        [ h5 [] [ text profile.name ]
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
                            [ input [ class "form-control", name "username", placeholder "Enter A Github Username...", type' "text", onInput SearchUser ]
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
