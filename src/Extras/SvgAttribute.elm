module Extras.SvgAttribute exposing (showIf)

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
