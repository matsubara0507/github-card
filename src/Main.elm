module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Dict
import GitHub
import GitHub.Color as GitHub
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Event
import Http
import Json.Decode as Json
import Octicons
import Task
import Url exposing (Url)
import Url.Parser as Url exposing ((</>), (<?>))
import Url.Parser.Query as UrlQuery


main : Program () Model Msg
main =
    Browser.application
        { init =
            \_ url ->
                initModel url << Model "" (Err NoInput) False { width = 0, height = 0 } ""
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


type alias Model =
    { text : String
    , info : Result Error GitHubInfo
    , embed : Bool
    , card : { width : Float, height : Float }
    , url : String
    , key : Nav.Key
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
    | GetCardElement (Result Dom.Error Dom.Element)
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url


initModel : Url -> Model -> ( Model, Cmd Msg )
initModel url model =
    let
        target =
            { url | path = "" }
                |> Url.parse (Url.top </> Url.top <?> UrlQuery.string "target")
                |> Maybe.withDefault Nothing

        baseUrl =
            Url.toString { url | query = Nothing, fragment = Nothing }
    in
    case target of
        Nothing ->
            ( { model | url = baseUrl }, Cmd.none )

        Just text ->
            update (FetchGitHub text)
                { model | text = text, embed = True }


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
            ( { model | info = Result.mapError HttpErr r }, getCardElement )

        InputText txt ->
            ( { model | text = txt }, Cmd.none )

        GetCardElement (Ok dom) ->
            let
                card =
                    { width = dom.element.width, height = dom.element.height }
            in
            ( { model | card = card }, Cmd.none )

        GetCardElement (Err _) ->
            ( model, Cmd.none )

        LinkClicked (Browser.Internal url) ->
            ( model, Nav.pushUrl model.key (Url.toString url) )

        LinkClicked (Browser.External href) ->
            ( model, Nav.load href )

        UrlChanged url ->
            initModel url model


view : Model -> Browser.Document Msg
view model =
    let
        title =
            "GitHub Card Builder"
    in
    { title = title
    , body =
        if model.embed then
            [ viewInfo model ]

        else
            [ Html.div [ Attr.class "p-3" ] [ viewBody title model ] ]
    }


viewBody : String -> Model -> Html Msg
viewBody title model =
    Html.div [ Attr.class "container" ]
        [ Html.h2 [ Attr.class "pb-3" ] [ Html.text title ]
        , Html.div [ Attr.class "clearfix" ]
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
        , Html.div [ Attr.class "p-5 one-half" ] [ viewInfo model ]
        , Html.div []
            [ Html.h3 [] [ Html.text "Embed:" ]
            , case model.info of
                Ok _ ->
                    Html.text (iframe model)

                Err _ ->
                    Html.text ""
            ]
        ]


iframe : Model -> String
iframe model =
    String.join " "
        [ "<iframe"
        , "width=\"" ++ String.fromFloat model.card.width ++ "\""
        , "height=\"" ++ String.fromFloat model.card.height ++ "\""
        , "scrolling=\"no\""
        , "frameborder=\"0\""
        , "src=\"" ++ model.url ++ "?target=" ++ model.text ++ "\""
        , "></iframe>"
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
                |> always (buildCard info)

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
    Html.div [ Attr.id "github-card", Attr.class "Box box-shadow" ]
        [ Html.div [ Attr.class "Box-row" ]
            [ Html.div []
                [ Html.img [ Attr.class "avatar float-left pr-2", Attr.src <| user.avatar ++ "&s=48" ] []
                , Html.div []
                    [ Html.h3 [] [ Html.text <| Maybe.withDefault user.login user.name ]
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
    Html.div ([ Attr.id "github-card", Attr.class "Box box-shadow" ] ++ border)
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


getCardElement : Cmd Msg
getCardElement =
    Task.attempt GetCardElement (Dom.getElement "github-card")
