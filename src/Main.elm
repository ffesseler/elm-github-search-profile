module Main exposing (..)

import Html exposing (text, Html, nav, i, a, li, div, input, form, b, p, h5, h6, span, ul, img, h4)
import Html.Attributes exposing (class, attribute, href, type', placeholder, id, name, target, src)
import Html.Events exposing (onInput)
import Html.App as App
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required, decode, nullable)
import Task
import String exposing (length)


main =
    App.program { init = init "" (Credentials "" ""), update = update, view = view, subscriptions = \_ -> Sub.none }



-- MODEL


type alias Model =
    { githubName : String
    , credentials : Credentials
    , profile : Maybe GithubProfile
    , repositories : List Repository
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
    , created_at : String
    , location : String
    , email : Maybe String
    , blog : Maybe String
    , html_url : String
    , public_repos : Int
    , public_gists : Int
    , following : Int
    , followers : Int
    }


type alias Repository =
    { name : String
    , watchers : Int
    , forks : Int
    , description : Maybe String
    , html_url : String
    }


type Msg
    = SearchUser String
    | FetchUserDone (GithubProfile)
    | FetchUserFail Http.Error
    | FetchRepositoriesDone (List Repository)
    | FetchRepositoriesFail Http.Error



-- INIT


init githubName credentials =
    ( Model githubName credentials Nothing [] Nothing, Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchUser githubName ->
            if length githubName > 0 then
                ( model, fetchGithubUser githubName model.credentials )
            else
                ( { model | error = Nothing, profile = Nothing }, Cmd.none )

        FetchUserDone profile ->
            ( { model | profile = Just profile, error = Nothing }, fetchGithubUserRepositories profile.login model.credentials )

        FetchRepositoriesDone repositories ->
            ( { model | repositories = repositories }, Cmd.none )

        FetchUserFail error ->
            Debug.log "fail" ( { model | error = Just error }, Cmd.none )

        FetchRepositoriesFail error ->
            Debug.log "fail" ( { model | error = Just error }, Cmd.none )


fetchGithubUserRepositories githubName credentials =
    Http.get repositoriesDecoder (fetchGithubUserRepositoriesUrl githubName credentials)
        |> Task.perform FetchRepositoriesFail FetchRepositoriesDone


fetchGithubUser githubName credentials =
    Http.get profileDecoder (fetchGithubUserUrl githubName credentials)
        |> Task.perform FetchUserFail FetchUserDone


fetchGithubUserUrl : String -> Credentials -> String
fetchGithubUserUrl name credentials =
    githubApiBaseUrl name ++ githubApiSecretParams credentials


fetchGithubUserRepositoriesUrl : String -> Credentials -> String
fetchGithubUserRepositoriesUrl name credentials =
    githubApiBaseUrl name ++ "/repos" ++ githubApiSecretParams credentials


githubApiBaseUrl name =
    "http://api.github.com/users/" ++ name


githubApiSecretParams credentials =
    "?client_id=" ++ credentials.clientId ++ "&client_secret=" ++ credentials.clientSecret


profileDecoder : Decode.Decoder (GithubProfile)
profileDecoder =
    decode GithubProfile
        |> required "name" Decode.string
        |> required "repos_url" Decode.string
        |> required "avatar_url" Decode.string
        |> required "login" Decode.string
        |> required "created_at" Decode.string
        |> required "location" Decode.string
        |> required "email" (nullable Decode.string)
        |> required "blog" (nullable Decode.string)
        |> required "html_url" Decode.string
        |> required "public_repos" Decode.int
        |> required "public_gists" Decode.int
        |> required "followers" Decode.int
        |> required "following" Decode.int


repositoriesDecoder : Decode.Decoder (List Repository)
repositoriesDecoder =
    Decode.list repositoryDecoder


repositoryDecoder : Decode.Decoder (Repository)
repositoryDecoder =
    decode Repository
        |> required "name" Decode.string
        |> required "watchers" Decode.int
        |> required "forks" Decode.int
        |> required "description" (nullable Decode.string)
        |> required "html_url" Decode.string



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ profile model
        , searchform model
        ]


profile model =
    case model.error of
        Just error ->
            userProfileNotFound

        Nothing ->
            case model.profile of
                Nothing ->
                    noInputYet

                Just profile ->
                    displayProfile profile model


userProfileNotFound =
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


noInputYet =
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


displayProfile profile model =
    let
        email =
            (\value ->
                case value of
                    Just str ->
                        str

                    Nothing ->
                        ""
            )
                profile.email

        blog =
            (\value ->
                case value of
                    Just str ->
                        str

                    Nothing ->
                        ""
            )
                profile.blog
    in
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
                                            , text email
                                            ]
                                        , li [ class "list-group-item" ]
                                            [ b []
                                                [ text "Blog Link: " ]
                                            , text blog
                                            ]
                                        , li [ class "list-group-item" ]
                                            [ b []
                                                [ text "Member Since: " ]
                                            , text profile.created_at
                                            ]
                                        ]
                                    ]
                                ]
                            , div [ class "status m-t-1" ]
                                [ span [ class "p-a-05 bg-info text-xs-center" ]
                                    [ text (toString profile.public_repos ++ " Public Repos ") ]
                                , span [ class "p-a-05 bg-primary m-t-1 text-xs-center" ]
                                    [ text (toString profile.public_gists ++ " Public Gists ") ]
                                , span [ class "p-a-05 bg-danger m-t-1 text-xs-center" ]
                                    [ text (toString profile.followers ++ " Followers ") ]
                                , span [ class "p-a-05 bg-inverse m-t-1 text-xs-center" ]
                                    [ text (toString profile.following ++ " Followings ") ]
                                ]
                            ]
                        ]
                    ]
                ]
            , div [ class "col-md-7" ]
                [ div []
                    [ div [ class "card" ]
                        [ h5 [ class "card-header" ]
                            [ text "Repos" ]
                        , div [ class "card-block" ]
                            [ repositoriesViewList model
                            ]
                        ]
                    ]
                ]
            ]


repositoriesViewList model =
    model.repositories
        |> List.map repositoryView
        |> div [ class "list-group" ]


repositoryView repository =
    let
        description =
            (\value ->
                case value of
                    Nothing ->
                        ""

                    Just a ->
                        a
            )
                repository.description
    in
        a [ class "list-group-item list-group-item-action", href repository.html_url, target "_blank" ]
            [ span [ class "tag tag-info pull-xs-right" ]
                [ text (toString repository.watchers ++ " Watchers") ]
            , span [ class "tag tag-success pull-xs-right m-r-05" ]
                [ text (toString repository.forks ++ " Forks") ]
            , h6 [ class "list-group-item-heading" ]
                [ text repository.name ]
            , p [ class "list-group-item-text" ]
                [ text description ]
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
