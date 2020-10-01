module Risky exposing (get)

import Http



{- Required for development, could be removed for prod. -}


get :
    { url : String
    , expect : Http.Expect msg
    }
    -> Cmd msg
get r =
    Http.riskyRequest
        { method = "GET"
        , headers = []
        , url = r.url
        , body = Http.emptyBody
        , expect = r.expect
        , timeout = Nothing
        , tracker = Nothing
        }
