module Extras.String exposing (escapeForMarkdown, firstAlphaNumericCharacter, preserveOnlyAlphaNumChars)

{-| Extra functionality for strings.


# Strings

@docs escapeForMarkdown, firstAlphaNumericCharacter, preserveOnlyAlphaNumChars

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

    firstAlphaNumericCharacter "Word" --> Just "W"

    firstAlphaNumericCharacter "1 Word" --> Just "1"

    firstAlphaNumericCharacter ".5" --> Just "5"

    firstAlphaNumericCharacter "Ω" --> Nothing

-}
firstAlphaNumericCharacter : String -> Maybe String
firstAlphaNumericCharacter =
    let
        regex : Regex.Regex
        regex =
            "[A-Za-zÀ-ÖØ-öø-įĴ-őŔ-žǍ-ǰǴ-ǵǸ-țȞ-ȟȤ-ȳɃɆ-ɏḀ-ẞƀ-ƓƗ-ƚƝ-ơƤ-ƥƫ-ưƲ-ƶẠ-ỿ0-9]"
                |> Regex.fromString
                |> Maybe.withDefault Regex.never
    in
    Regex.findAtMost 1 regex >> List.head >> Maybe.map .match


{-| Strip out any characters that aren't alphanumeric.
-}
preserveOnlyAlphaNumChars : String -> String
preserveOnlyAlphaNumChars =
    let
        regex : Regex.Regex
        regex =
            "[^A-Za-zÀ-ÖØ-öø-įĴ-őŔ-žǍ-ǰǴ-ǵǸ-țȞ-ȟȤ-ȳɃɆ-ɏḀ-ẞƀ-ƓƗ-ƚƝ-ơƤ-ƥƫ-ưƲ-ƶẠ-ỿ0-9]"
                |> Regex.fromString
                |> Maybe.withDefault Regex.never
    in
    Regex.replace regex (always "")
