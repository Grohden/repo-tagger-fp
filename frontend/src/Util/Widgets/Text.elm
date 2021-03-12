module Util.Widgets.Text exposing (..)

import Html exposing (Html, span, text)
import Html.Attributes exposing (class)


h1 msg =
    span [ class "h1" ] [ text msg ]


h3 msg =
    span [ class "h3" ] [ text msg ]
