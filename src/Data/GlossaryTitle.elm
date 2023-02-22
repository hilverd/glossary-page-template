module Data.GlossaryTitle exposing (GlossaryTitle, fromString, toString, toFilename)

{-| The title of a glossary.


# Glossary Titles

@docs GlossaryTitle, fromString, toString, toFilename

-}

import Regex


{-| An opaque type for a glossary title which is just a wrapper around a string.
-}
type GlossaryTitle
    = GlossaryTitle String


{-| Construct a glossary title from a string.
-}
fromString : String -> GlossaryTitle
fromString =
    GlossaryTitle


{-| Get the string representation of a glossary title.
-}
toString : GlossaryTitle -> String
toString glossaryTitle =
    case glossaryTitle of
        GlossaryTitle title ->
            title


{-| Convert a glossary title to a filename with the given extension.
Spaces are replaced by underscores.
Any other characters which are not generally safe for filenames are omitted.

    "Weather Terms: A Complete Guide"
    |> fromString
    |> toFilename ".html"
    --> "Weather_Terms_A_Complete_Guide.html"

-}
toFilename : String -> GlossaryTitle -> String
toFilename extension glossaryTitle =
    let
        notSoNiceCharacters : Regex.Regex
        notSoNiceCharacters =
            "[^A-Za-zÀ-ÖØ-öø-įĴ-őŔ-žǍ-ǰǴ-ǵǸ-țȞ-ȟȤ-ȳɃɆ-ɏḀ-ẞƀ-ƓƗ-ƚƝ-ơƤ-ƥƫ-ưƲ-ƶẠ-ỿ0-9_-]"
                |> Regex.fromString
                |> Maybe.withDefault Regex.never
    in
    glossaryTitle
        |> toString
        |> Regex.replace notSoNiceCharacters
            (\m ->
                if m.match == " " then
                    "_"

                else
                    ""
            )
        |> (\result ->
                if String.startsWith "-" result then
                    String.dropLeft 1 result

                else
                    result
           )
        |> (\result -> result ++ extension)
