module Route exposing (Route(..), parseUrl)

import Url exposing (Url)
import Url.Parser as Parser exposing (..)


type Route
    = Login
    | Starred
    | NotFound


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Login Parser.top
        , Parser.map Login (s "login")
        , Parser.map Starred (s "starred")
        ]


path : Route -> String
path route =
    case route of
        Login ->
            "#"

        Starred ->
            "#starred"

        NotFound ->
            "#not-found"


parseUrl : Url -> Route
parseUrl url =
    -- The RealWorld spec treats the fragment like a path.
    -- This makes it *literally* the path, so we can proceed
    -- with parsing as if it had been a normal path all along.
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser
        |> Maybe.withDefault NotFound
