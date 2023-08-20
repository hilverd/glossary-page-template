module Extras.SvgAttribute exposing (showIf, showMaybe)

import Svg
import Svg.Attributes


empty : Svg.Attribute msg
empty =
    Svg.Attributes.class ""


showIf : Bool -> Svg.Attribute msg -> Svg.Attribute msg
showIf condition attribute =
    if condition then
        attribute

    else
        empty


showMaybe : (a -> Svg.Attribute msg) -> Maybe a -> Svg.Attribute msg
showMaybe f maybe =
    maybe
        |> Maybe.map f
        |> Maybe.withDefault empty
