module Util.Widgets.Container exposing (..)

import Html exposing (Attribute, button, div)
import Html.Attributes exposing (class, style)


type MainAxisAlignment
    = Center
    | Start
    | End


type Disposition
    = Row
    | Column


type alias ContainerConfiguration =
    { disposition : Disposition
    , mainAxisAlignment : MainAxisAlignment
    , crossAxisAlignment : MainAxisAlignment
    , width : Maybe String
    , height : Maybe String
    , border : Maybe (List String)
    , borderRadius : Maybe String
    , classes : Maybe String
    }


decodeAxisAlignment main cross disposition align =
    let
        flexDisposition =
            case disposition of
                Row ->
                    main

                Column ->
                    cross

        defaultAlign =
            case align of
                Center ->
                    [ "flex", flexDisposition, "center" ]

                Start ->
                    [ "flex", flexDisposition, "start" ]

                End ->
                    [ "flex", flexDisposition, "end" ]
    in
    String.join "-" defaultAlign


decodeCrossAxisAlignment =
    decodeAxisAlignment "items" "justify"


decodeMainAxisAlignment =
    decodeAxisAlignment "justify" "items"


decodeDisposition disposition =
    case disposition of
        Row ->
            "flex-row"

        Column ->
            "flex-column"



-- RESOLVER


resolveAttrs : ContainerConfiguration -> List (Attribute msg)
resolveAttrs config =
    let
        resolvedClasses =
            [ "d-flex"
            , decodeDisposition config.disposition
            , decodeMainAxisAlignment config.disposition config.mainAxisAlignment
            , decodeCrossAxisAlignment config.disposition config.crossAxisAlignment
            ]
                |> String.join " "
                |> Just

        allClasses =
            [ resolvedClasses, config.classes ]
                |> List.filterMap identity
                |> String.join " "

        styles =
            List.filterMap identity
                [ Maybe.map (\width -> style "width" width) config.width
                , Maybe.map (\height -> style "height" height) config.height
                , Maybe.map (\border -> style "border" (String.join " " border)) config.border
                , Maybe.map (\radius -> style "border-radius" radius) config.borderRadius
                ]
    in
    List.concat [ [ class allClasses ], styles ]



-- BUILDERS


builtDiv : List (Html.Html msg) -> ContainerConfiguration -> Html.Html msg
builtDiv children config =
    div
        (resolveAttrs config)
        children


builtButton : List (Html.Html msg) -> ContainerConfiguration -> Html.Html msg
builtButton children config =
    button
        (resolveAttrs config)
        children



-- STANDARD WIDGETS


defaults : ContainerConfiguration
defaults =
    { disposition = Row
    , mainAxisAlignment = Center
    , crossAxisAlignment = Center
    , width = Nothing
    , height = Nothing
    , border = Nothing
    , borderRadius = Nothing
    , classes = Nothing
    }


row : ContainerConfiguration
row =
    { defaults | disposition = Row }


column : ContainerConfiguration
column =
    { defaults | disposition = Column }



-- SIZES


withSize width height configs =
    { configs | width = Just width, height = Just height }


withWidth width configs =
    { configs | width = Just width }


withHeight height configs =
    { configs | height = Just height }



-- AESTHETICS


withSolidBorders : String -> String -> ContainerConfiguration -> ContainerConfiguration
withSolidBorders width color configs =
    { configs | border = Just [ width, color, "solid" ] }


withCircularBorderRadius : String -> ContainerConfiguration -> ContainerConfiguration
withCircularBorderRadius radius configs =
    { configs | borderRadius = Just radius }


withClasses : String -> ContainerConfiguration -> ContainerConfiguration
withClasses classes configs =
    { configs | classes = Just classes }



-- ALIGNMENT


withMainAlignment : MainAxisAlignment -> ContainerConfiguration -> ContainerConfiguration
withMainAlignment alignment configs =
    { configs | mainAxisAlignment = alignment }


withCrossAlignment : MainAxisAlignment -> ContainerConfiguration -> ContainerConfiguration
withCrossAlignment alignment configs =
    { configs | crossAxisAlignment = alignment }


withDisposition : Disposition -> ContainerConfiguration -> ContainerConfiguration
withDisposition disposition configs =
    { configs | disposition = disposition }
