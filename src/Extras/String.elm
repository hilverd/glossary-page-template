module Extras.String exposing (escapeForMarkdown, firstAlphabeticCharacter)

{-| Extra functionality for strings.


# Strings

@docs escapeForMarkdown, firstAlphabeticCharacter

-}

import Regex


{-| Escape a string so that it can be used in a Markdown fragment.
-}
escapeForMarkdown : String -> String
escapeForMarkdown string =
    let
        charactersToEscape : Regex.Regex
        charactersToEscape =
            -- It might be necessary to escape this set too: [().!-]
            -- but these are left out for now.
            "[\\`*_{}\\[\\]#+]"
                |> Regex.fromString
                |> Maybe.withDefault Regex.never
    in
    Regex.replace charactersToEscape (.match >> (++) "\\") string


{-| Returns the first alphabetic character occurring in a given string.

    firstAlphabeticCharacter "Word" --> Just "W"

    firstAlphabeticCharacter "1 Word" --> Just "W"

    firstAlphabeticCharacter "123" --> Nothing

-}
firstAlphabeticCharacter : String -> Maybe String
firstAlphabeticCharacter string =
    let
        regex : Regex.Regex
        regex =
            "[A-Za-z]"
                |> Regex.fromString
                |> Maybe.withDefault Regex.never
    in
    string |> Regex.findAtMost 1 regex |> List.head |> Maybe.map .match
