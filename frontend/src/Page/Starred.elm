module Page.Starred exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Backend.Api exposing (SimpleRepository, loadTaggerItems)
import Html exposing (Html, a, div, h3, img, li, p, span, text, ul)
import Html.Attributes exposing (class, href, src, style)
import Http
import Maybe
import Skeleton
import Util.Assets as Assets
import Util.Css as Css
import Util.Widgets.Container as Container
import Util.Widgets.Text as Text



-- MODEL


type Model
    = Failure Http.Error
    | LoadingStarred
    | Success (List SimpleRepository)



-- INIT


init : ( Model, Cmd Msg )
init =
    ( LoadingStarred, loadTaggerItems GotRepositoryList )



-- UPDATE


type Msg
    = GotRepositoryList (Result Http.Error (List SimpleRepository))


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        GotRepositoryList result ->
            case result of
                Ok list ->
                    ( Success list, Cmd.none )

                Err err ->
                    ( Failure err, Cmd.none )



-- VIEW


view : Model -> Skeleton.Details Msg
view model =
    { title = "Starred list"
    , attrs = []
    , kids =
        [ case model of
            Failure error ->
                centeredFullContainer [] [ errorView error ]

            LoadingStarred ->
                centeredLoader "Loading.."

            Success list ->
                div [ class "d-flex flex-items-baseline flex-justify-around" ]
                    [ listGroup (List.map toRepositoryListView list) ]
        ]
    }


centeredLoader : String -> Html msg
centeredLoader textValue =
    div [ class "d-flex justify-content-center" ]
        [ div
            [ class "spinner-border text-primary m-5" ]
            [ span [ class "sr-only" ] [ text textValue ] ]
        ]


centeredFullContainer : List Css.Class -> List (Html msg) -> Html msg
centeredFullContainer additional kids =
    div [ class ([ Css.alignItemsCenter, Css.flexJustifyCenter, Css.displayFlex, Css.h100 ] ++ additional |> Css.decode) ]
        kids


listGroup : List (Html msg) -> Html msg
listGroup msg =
    ul [ class "list-group" ] msg


toRepositoryListView : SimpleRepository -> Html Msg
toRepositoryListView repo =
    li [ class "col-12 d-flex width-full py-4 border-bottom public fork" ]
        [ div [ class "col-10 col-lg-9 d-inline-block" ]
            [ div [ class "d-inline-block mb-1" ]
                [ h3 [ class "wb-break-all" ] [ buildRepoLink repo ] ]
            , case repo.description of
                Just description ->
                    div []
                        [ p [ class "col-9 d-inline-block text-gray mb-2 pr-4" ]
                            [ text description ]
                        ]

                Nothing ->
                    div [] [ text "Repo list empty, TODO" ]
            ]
        ]


buildRepoLink repo =
    let
        link =
            [ "https://github.com", repo.ownerName, repo.name ] |> String.join "/"
    in
    a [ href link ] [ text repo.name ]



-- ERROR


errorView : Http.Error -> Html msg
errorView error =
    case error of
        Http.BadBody msg ->
            text msg

        Http.BadUrl msg ->
            text msg

        Http.Timeout ->
            errorContainer Assets.clockTimer "Uh oh! Got 'Timeout', maybe you could refresh the page?"

        Http.NetworkError ->
            errorContainer Assets.serverDown "Uh oh! Got 'NetworkError', are you sure you have internet connection?"

        Http.BadStatus status ->
            let
                ( asset, message ) =
                    getBadStatusMessage status
            in
            errorContainer asset message



-- TODO: change assets


getBadStatusMessage status =
    case status of
        401 ->
            ( Assets.serverDown, "Uh oh! Got (401 Unauthorized), are you really supposed to be here?" )

        _ ->
            ( Assets.serverDown, "Uh oh! Got BadStatus(" ++ String.fromInt status ++ "), I'm not sure whats going on.." )


errorContainer asset message =
    Container.column
        |> Container.builtDiv
            [ img
                [ style "height" "300px"
                , src (Assets.assetPath asset)
                ]
                []
            , Container.row
                |> Container.withHeight "212px"
                |> Container.builtDiv [ Text.h1 message ]
            ]
