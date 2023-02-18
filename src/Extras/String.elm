module Extras.String exposing (escapeForMarkdown)

import Regex


escapeForMarkdown : String -> String
escapeForMarkdown string =
    let
        charactersToEscape =
            -- It might be necessary to escape this set too: [().!-]
            -- but these are left out for now.
            "[\\`*_{}\\[\\]#+]"
                |> Regex.fromString
                |> Maybe.withDefault Regex.never
    in
    Regex.replace charactersToEscape (.match >> (++) "\\") string
