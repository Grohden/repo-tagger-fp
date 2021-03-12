module Main exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Page.Login as Login
import Page.Starred as Starred
import Route as Route exposing (Route, parseUrl)
import Skeleton
import Url exposing (Url)



-- MAIN


main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



-- MODEL


type alias Model =
    { route : Route
    , page : Page
    , navKey : Nav.Key
    }


type Page
    = NotFoundPage
    | LoginPage Login.Model
    | StarredPage Starred.Model



-- INIT


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    let
        model =
            { route = parseUrl url
            , page = NotFoundPage
            , navKey = navKey
            }
    in
    initCurrentPage ( model, Cmd.none )


initCurrentPage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
initCurrentPage ( model, existingCmds ) =
    let
        ( currentPage, mappedPageCmds ) =
            case model.route of
                Route.NotFound ->
                    ( NotFoundPage, Cmd.none )

                Route.Starred ->
                    let
                        ( pageModel, pageCmds ) =
                            Starred.init
                    in
                    ( StarredPage pageModel, Cmd.map StarredPageMsg pageCmds )

                Route.Login ->
                    let
                        ( pageModel, pageCmds ) =
                            Login.init
                    in
                    ( LoginPage pageModel, Cmd.map LoginPageMsg pageCmds )
    in
    ( { model | page = currentPage }
    , Cmd.batch [ existingCmds, mappedPageCmds ]
    )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.page of
        NotFoundPage ->
            Skeleton.view never
                { title = "Not Found"
                , attrs = []
                , kids = notFound
                }

        StarredPage pageModel ->
            Skeleton.view StarredPageMsg (Starred.view pageModel)

        LoginPage pageModel ->
            Skeleton.view LoginPageMsg (Login.view pageModel)



-- NOT FOUND


notFound : List (Html msg)
notFound =
    [ div [ style "font-size" "12em" ] [ text "404" ]
    , div [ style "font-size" "3em" ] [ text "I cannot find this page!" ]
    ]



-- UPDATE


type Msg
    = StarredPageMsg Starred.Msg
    | LoginPageMsg Login.Msg
    | UrlChanged Url.Url
    | LinkClicked UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( UrlChanged url, _ ) ->
            let
                newRoute =
                    parseUrl url
            in
            ( { model | route = newRoute }, Cmd.none )
                |> initCurrentPage

        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navKey (Url.toString url) )

                Browser.External url ->
                    ( model, Nav.load url )

        ( StarredPageMsg subMsg, StarredPage pageModel ) ->
            let
                ( updatePageModel, updateCmd ) =
                    Starred.update subMsg pageModel
            in
            ( { model | page = StarredPage updatePageModel }
            , Cmd.map StarredPageMsg updateCmd
            )

        ( LoginPageMsg subMsg, LoginPage pageModel ) ->
            let
                ( updatePageModel, updateCmd ) =
                    Login.update subMsg pageModel
            in
            ( { model | page = LoginPage updatePageModel }
            , Cmd.map LoginPageMsg updateCmd
            )

        ( _, _ ) ->
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
