module Extras.HtmlAttribute exposing (empty, showIf)

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
