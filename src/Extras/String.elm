module Extras.String exposing (escapeForMarkdown, firstAlphanumericCharacter, preserveOnlyAlphanumChars, emDash, enDash, isRelativeUrl)

{-| Extra functionality for strings.


# Strings

@docs escapeForMarkdown, firstAlphanumericCharacter, preserveOnlyAlphanumChars, emDash, enDash, isRelativeUrl

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
preserveOnlyAlphanumChars : String -> String
preserveOnlyAlphanumChars =
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


absoluteUrlRegex : Regex.Regex
absoluteUrlRegex =
    "^https?:\\/\\/|^\\/\\/"
        |> Regex.fromStringWith { caseInsensitive = True, multiline = False }
        |> Maybe.withDefault Regex.never


{-| Make a reasonable attempt to determine if the given string is a relative URL.

    isRelativeUrl "/foo/bar" --> True

    isRelativeUrl "foo/bar" --> True

    isRelativeUrl "https://example.com" --> False

    isRelativeUrl "http://example.com" --> False

-}
isRelativeUrl : String -> Bool
isRelativeUrl =
    not << Regex.contains absoluteUrlRegex
