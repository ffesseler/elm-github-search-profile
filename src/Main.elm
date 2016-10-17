module Main exposing (..)

import Html exposing (text, Html, nav, i, a, li, div, input, form, b, p, h5, h6, span, ul, img, h4)
import Html.Attributes exposing (class, attribute, href, type', placeholder, id, name, target, src)
import Html.Events exposing (onInput)
import Html.App as App
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required, decode)
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
    , avatar_url : String
    , login : String
    , location : String
    , email : String
    , blog : String
    , created_at : String
    , html_url : String
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
    decode GithubProfile
        |> required "name" Decode.string
        |> required "repos_url" Decode.string
        |> required "avatar_url" Decode.string
        |> required "login" Decode.string
        |> required "created_at" Decode.string
        |> required "location" Decode.string
        |> required "email" Decode.string
        |> required "blog" Decode.string
        |> required "html_url" Decode.string


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
                    div [ class "row" ]
                        [ div [ class "col-md-5" ]
                            [ div []
                                [ div [ class "card" ]
                                    [ h4 [ class "card-header" ]
                                        [ text profile.name ]
                                    , div [ class "card-block" ]
                                        [ div [ class "row" ]
                                            [ div [ class "col-xl-4 m-b-1" ]
                                                [ img [ class "profile-img img-thumbnail m-b-1", src profile.avatar_url ]
                                                    []
                                                , a [ class "btn btn-sm btn-outline-primary btn-block m-t-1", href profile.html_url, target "_blank" ]
                                                    [ text "View Profile" ]
                                                ]
                                            , div [ class "col-xl-8" ]
                                                [ ul [ class "list-group" ]
                                                    [ li [ class "list-group-item" ]
                                                        [ b []
                                                            [ text "Username: " ]
                                                        , text profile.login
                                                        ]
                                                    , li [ class "list-group-item" ]
                                                        [ b []
                                                            [ text "Location: " ]
                                                        , text profile.location
                                                        ]
                                                    , li [ class "list-group-item" ]
                                                        [ b []
                                                            [ text "E-Mail: " ]
                                                        , text profile.email
                                                        ]
                                                    , li [ class "list-group-item" ]
                                                        [ b []
                                                            [ text "Blog Link: " ]
                                                        , text profile.blog
                                                        ]
                                                    , li [ class "list-group-item" ]
                                                        [ b []
                                                            [ text "Member Since: " ]
                                                        , text profile.created_at
                                                        ]
                                                    ]
                                                ]
                                            ]
                                        ]
                                    ]
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
