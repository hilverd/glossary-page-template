module Extras.HtmlAttribute exposing (fromBool, inert, showIf, showMaybe, showUnless)

import Html
import Html.Attributes


empty : Html.Attribute msg
empty =
    Html.Attributes.classList []


showIf : Bool -> Html.Attribute msg -> Html.Attribute msg
showIf condition attribute =
    if condition then
        attribute

    else
        empty


showMaybe : (a -> Html.Attribute msg) -> Maybe a -> Html.Attribute msg
showMaybe f maybe =
    maybe
        |> Maybe.map f
        |> Maybe.withDefault empty


showUnless : Bool -> Html.Attribute msg -> Html.Attribute msg
showUnless =
    not >> showIf


fromBool : String -> Bool -> Html.Attribute msg
fromBool name bool =
    Html.Attributes.attribute name <|
        if bool then
            "true"

        else
            "false"


inert : Html.Attribute msg
inert =
    Html.Attributes.attribute "inert" "inert"
