module Backend.Api exposing (..)

import Http
import Json.Decode exposing (Decoder, field, int, list, map4, maybe, string)
import Util.Risky as Risky


baseUrl value =
    "http://localhost:8080" ++ value



--- Responses


type alias SimpleRepository =
    { name : String
    , stargazersCount : Int
    , description : Maybe String
    , ownerName : String
    }



-- List Decoders


repositoryListDecoder : Decoder (List SimpleRepository)
repositoryListDecoder =
    list simpleRepositoryDecoder



-- Object Decoders


simpleRepositoryDecoder : Decoder SimpleRepository
simpleRepositoryDecoder =
    map4 SimpleRepository
        (field "name" string)
        (field "stargazersCount" int)
        (maybe (field "description" string))
        (field "ownerName" string)



-- Requests


loadTaggerItems : (Result Http.Error (List SimpleRepository) -> msg) -> Cmd msg
loadTaggerItems expect =
    Risky.get
        { url = baseUrl "/api/repository/starred"
        , expect = Http.expectJson expect repositoryListDecoder
        }
