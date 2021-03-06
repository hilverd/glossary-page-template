module Extras.Html exposing (inlineCode, nothing, showIf, showMaybe)

import Html exposing (Html, code, span, text)
import Html.Attributes exposing (class)


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


inlineCode : String -> Html msg
inlineCode string =
    span []
        [ code
            [ class "select-none" ]
            [ text "`" ]
        , code
            []
            [ text string ]
        , code
            [ class "select-none" ]
            [ text "`" ]
        ]
