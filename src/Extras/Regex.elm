module Extras.Regex exposing (escapeStringForUseInRegex)

import Regex


escapeStringForUseInRegex : String -> String
escapeStringForUseInRegex string =
    let
        regex =
            "[-\\/\\\\^$*+?.()|[\\]{}]"
                |> Regex.fromString
                |> Maybe.withDefault Regex.never
    in
    Regex.replace regex (.match >> (++) "\\") string
