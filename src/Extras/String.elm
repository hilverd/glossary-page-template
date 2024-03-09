module Extras.String exposing
    ( escapeForMarkdown, preserveOnlyAlphaNumChars, emDash, enDash
    , firstAlphanumericCharacter
    )

{-| Extra functionality for strings.


# Strings

@docs escapeForMarkdown, firstAlphaNumericCharacter, preserveOnlyAlphaNumChars, emDash, enDash

-}

import Regex


charactersToEscapeForMarkdownRegex : Regex.Regex
charactersToEscapeForMarkdownRegex =
    -- It might be necessary to escape this set too: [().!-]
    -- but these are left out for now.
    "[\\`*_{}\\[\\]#+]"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


{-| Escape a string so that it can be used in a Markdown fragment.
-}
escapeForMarkdown : String -> String
escapeForMarkdown string =
    Regex.replace
        charactersToEscapeForMarkdownRegex
        (.match >> (++) "\\")
        string


firstAlphanumericCharacterRegex : Regex.Regex
firstAlphanumericCharacterRegex =
    "[A-Za-zÀ-ÖØ-öø-įĴ-őŔ-žǍ-ǰǴ-ǵǸ-țȞ-ȟȤ-ȳɃɆ-ɏḀ-ẞƀ-ƓƗ-ƚƝ-ơƤ-ƥƫ-ưƲ-ƶẠ-ỿ0-9]"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


{-| Returns the first alphabetic character occurring in a given string.

    firstAlphanumericCharacter "Word" --> Just "W"

    firstAlphanumericCharacter "1 Word" --> Just "1"

    firstAlphanumericCharacter ".5" --> Just "5"

    firstAlphanumericCharacter "Ω" --> Nothing

-}
firstAlphanumericCharacter : String -> Maybe String
firstAlphanumericCharacter =
    Regex.findAtMost 1 firstAlphanumericCharacterRegex
        >> List.head
        >> Maybe.map .match


nonAlphanumericCharacterRegex : Regex.Regex
nonAlphanumericCharacterRegex =
    "[^A-Za-zÀ-ÖØ-öø-įĴ-őŔ-žǍ-ǰǴ-ǵǸ-țȞ-ȟȤ-ȳɃɆ-ɏḀ-ẞƀ-ƓƗ-ƚƝ-ơƤ-ƥƫ-ưƲ-ƶẠ-ỿ0-9]"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


{-| Strip out any characters that aren't alphanumeric.
-}
preserveOnlyAlphaNumChars : String -> String
preserveOnlyAlphaNumChars =
    Regex.replace nonAlphanumericCharacterRegex (always "")


{-| A long dash (—) used in punctuation.
-}
emDash : String
emDash =
    "—"


{-| A short dash (–) used in punctuation.
-}
enDash : String
enDash =
    "–"
