module Util.Css exposing (..)


type Class
    = Class String


toClass : Class -> String
toClass class =
    case class of
        Class value ->
            value


displayFlex : Class
displayFlex =
    Class "d-flex"


flex : Class
flex =
    Class "flex"


h100 : Class
h100 =
    Class "h-100"


w100 : Class
w100 =
    Class "w-100"


flexJustifyCenter : Class
flexJustifyCenter =
    Class "flex-justify-center"


flexRow : Class
flexRow =
    Class "flex-row"


flexColumn : Class
flexColumn =
    Class "flex-column"


alignItemsCenter : Class
alignItemsCenter =
    Class "flex-items-center"


decode : List Class -> String
decode values =
    values
        |> List.map toClass
        |> String.join " "
