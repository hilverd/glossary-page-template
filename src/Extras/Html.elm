module Extras.Html exposing (nothing, showIf, showMaybe)

import Html exposing (Html)


nothing : Html msg
nothing =
    Html.text ""


showIf : Bool -> Html msg -> Html msg
showIf condition html =
    if condition then
        html

    else
        nothing


showMaybe : (a -> Html msg) -> Maybe a -> Html msg
showMaybe f maybe =
    maybe
        |> Maybe.map f
        |> Maybe.withDefault nothing
