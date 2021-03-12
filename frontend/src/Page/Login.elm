module Page.Login exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Backend.Api exposing (SimpleRepository, loadTaggerItems)
import Html exposing (Html, div, h3, img, li, p, span, text, ul)
import Html.Attributes exposing (class, src, style)
import Http
import Maybe
import Skeleton
import Util.Assets as Assets
import Util.Css as Css
import Util.Widgets.Container as Container exposing (MainAxisAlignment(..))
import Util.Widgets.Text as Text



-- MODEL


type Model
    = Initial



-- INIT


init : ( Model, Cmd Msg )
init =
    ( Initial, Cmd.none )



-- UPDATE


type Msg
    = RedirectToOAuth


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    ( model, Cmd.none )



-- VIEW


view : Model -> Skeleton.Details Msg
view model =
    { title = "Login"
    , attrs = []
    , kids =
        [ case model of
            Initial ->
                Container.column
                    |> Container.withSize "100%" "100%"
                    |> Container.withCrossAlignment Center
                    |> Container.withMainAlignment Center
                    |> Container.builtDiv [ loginButton ]
        ]
    }


loginButton =
    Container.defaults
        |> Container.withClasses "btn"
        |> Container.builtButton [ Text.h3 "Login" ]


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
                [ h3 [ class "wb-break-all" ] [ text repo.name ] ]
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
