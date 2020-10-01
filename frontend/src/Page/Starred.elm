module Page.Starred exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Html exposing (Html, div, h3, li, p, span, text, ul)
import Html.Attributes exposing (class)
import Http
import Json.Decode exposing (Decoder, field, int, list, map3, maybe, string)
import Maybe
import Risky
import Skeleton



-- MODEL


type Model
    = Failure Http.Error
    | LoadingStarred
    | Success (List SimpleRepository)



-- INIT


init : ( Model, Cmd Msg )
init =
    ( LoadingStarred, loadTaggerItems )



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
                text ("Could not load data from request:" ++ extractErrorMessage error)

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
                    div [] []
            ]
        ]



-- ERROR


extractErrorMessage : Http.Error -> String
extractErrorMessage error =
    case error of
        Http.BadBody msg ->
            msg

        Http.BadUrl msg ->
            msg

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "NetworkError"

        Http.BadStatus status ->
            "BadStatus: " ++ String.fromInt status



-- HTTP


type alias SimpleRepository =
    { name : String
    , stargazersCount : Int
    , description : Maybe String
    }


loadTaggerItems : Cmd Msg
loadTaggerItems =
    Risky.get
        { url = "http://localhost:8080/api/repository/starred"
        , expect = Http.expectJson GotRepositoryList repositoryListDecoder
        }


repositoryListDecoder : Decoder (List SimpleRepository)
repositoryListDecoder =
    list simpleRepositoryDecoder


simpleRepositoryDecoder : Decoder SimpleRepository
simpleRepositoryDecoder =
    map3 SimpleRepository
        (field "name" string)
        (field "stargazersCount" int)
        (maybe (field "description" string))
