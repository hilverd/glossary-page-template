module Extras.HtmlAttribute exposing (empty, showIf, showMaybe)

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
