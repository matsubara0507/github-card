module Main exposing (main)

import Browser
import Dict
import GitHub
import GitHub.Color as GitHub
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Event
import Http
import Json.Decode as Json
import Octicons


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( Model "" (Result.Err NoInput), Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { text : String
    , info : Result Error GitHubInfo
    }


type GitHubInfo
    = GitHubUser GitHub.User
    | GitHubRepo GitHub.Repo


type Error
    = NoInput
    | InvalidInput
    | HttpErr Http.Error
    | JsonErr Json.Error


type Msg
    = FetchGitHub String
    | FetchedGitHub (Result Http.Error GitHubInfo)
    | InputText String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchGitHub text ->
            case String.split "/" text of
                [ owner ] ->
                    ( model
                    , GitHub.getUser owner (FetchedGitHub << Result.map GitHubUser)
                    )

                [ owner, repo ] ->
                    ( model
                    , GitHub.getRepo owner repo (FetchedGitHub << Result.map GitHubRepo)
                    )

                _ ->
                    ( { model | info = Err InvalidInput }, Cmd.none )

        FetchedGitHub r ->
            ( { model | info = Result.mapError HttpErr r }, Cmd.none )

        InputText txt ->
            ( { model | text = txt }, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div [ Attr.class "container" ]
        [ Html.div [ Attr.class "clearfix" ]
            [ Html.div
                [ Attr.class "btn btn-sm btn-with-count"
                , Event.onClick (FetchGitHub model.text)
                ]
                [ Html.text "Build" ]
            , Html.input
                [ Attr.class "social-count"
                , Event.onInput InputText
                ]
                []
            ]
        , Html.div [] [ viewInfo model ]
        ]


viewInfo : Model -> Html msg
viewInfo model =
    let
        alert message =
            Html.div [ Attr.class "flash flash-error" ] [ Html.text message ]
    in
    case model.info of
        Ok info ->
            Debug.log "fetch" info
                |> always (Html.div [ Attr.class "p-5 one-half" ] [ buildCard info ])

        Err NoInput ->
            Html.div [] []

        Err InvalidInput ->
            alert "Err: invalid input"

        Err (HttpErr err) ->
            Debug.log "error" err
                |> always (alert "Err: can't fetch by GitHub")

        Err (JsonErr err) ->
            Debug.log "error" err
                |> always (alert "Err: can't decode JSON")


buildCard : GitHubInfo -> Html msg
buildCard info =
    case info of
        GitHubUser user ->
            buildUserCard user

        GitHubRepo repo ->
            buildRepoCard repo


buildUserCard : GitHub.User -> Html msg
buildUserCard user =
    Html.div [ Attr.class "Box box-shadow" ]
        [ Html.div [ Attr.class "Box-row" ]
            [ Html.div []
                [ Html.img [ Attr.class "avatar float-left pr-2", Attr.src <| user.avatar ++ "&s=48" ] []
                , Html.div []
                    [ Html.h3 [] [ Html.text user.name ]
                    , Html.span [] [ Html.text ("@" ++ user.login) ]
                    ]
                ]
            ]
        , Html.div [ Attr.class "Box-row d-flex text-uppercase" ]
            [ Html.div [ Attr.class "pr-2 border-right" ]
                [ Html.span [] [ Html.text "Repos" ]
                , Html.h4 [] [ Html.text <| String.fromInt user.repoCnt ]
                ]
            , Html.div [ Attr.class "pr-2 pl-2 border-right" ]
                [ Html.span [] [ Html.text "Followers" ]
                , Html.h4 [] [ Html.text <| String.fromInt user.followers ]
                ]
            , Html.div [ Attr.class "pl-2" ]
                [ Html.span [] [ Html.text "Following" ]
                , Html.h4 [] [ Html.text <| String.fromInt user.following ]
                ]
            ]
        , Html.div [ Attr.class "Box-row" ]
            [ Html.a [ Attr.class "avatar float-left pr-2", Attr.href user.url ]
                [ Octicons.markGithub <| Octicons.size 24 <| Octicons.defaultOptions ]
            , Html.div [] [ Html.text user.bio ]
            ]
        ]


buildRepoCard : GitHub.Repo -> Html msg
buildRepoCard repo =
    let
        border =
            Dict.get repo.language GitHub.colors
                |> Maybe.withDefault Nothing
                |> Maybe.map (String.append "5px solid ")
                |> Maybe.map (Attr.style "border-left")
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
    in
    Html.div ([ Attr.class "Box box-shadow" ] ++ border)
        [ Html.div [ Attr.class "Box-row" ]
            [ Html.div []
                [ Html.img [ Attr.class "avatar float-left pr-2", Attr.src <| repo.avatar ++ "&s=48" ] []
                , Html.div []
                    [ Html.h3 [] [ Html.text repo.name ]
                    , Html.span [] [ Html.text ("Create by @" ++ repo.owner) ]
                    , Html.div [] [ Html.text repo.description ]
                    ]
                ]
            ]
        , Html.div [ Attr.class "Box-row d-flex text-uppercase" ]
            [ Html.a [ Attr.class "avatar float-left pr-2", Attr.href repo.url ]
                [ Octicons.markGithub <| Octicons.size 24 <| Octicons.defaultOptions ]
            , Html.div [ Attr.class "pr-2 pl-2" ]
                [ Html.strong [ Attr.class "pr-1" ] [ Html.text <| String.fromInt repo.starCnt ]
                , Html.span [] [ Html.text "Stars" ]
                ]
            , Html.div [ Attr.class "pl-2" ]
                [ Html.strong [ Attr.class "pr-1" ] [ Html.text <| String.fromInt repo.forkCnt ]
                , Html.span [] [ Html.text "Forks" ]
                ]
            ]
        ]
