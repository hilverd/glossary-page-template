module Data.GlossaryTitle exposing (GlossaryTitle, fromString, toFilename, toString)

import Regex


type GlossaryTitle
    = GlossaryTitle String


fromString : String -> GlossaryTitle
fromString =
    GlossaryTitle


toString : GlossaryTitle -> String
toString glossaryTitle =
    case glossaryTitle of
        GlossaryTitle title ->
            title


toFilename : String -> GlossaryTitle -> String
toFilename extension glossaryTitle =
    let
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
        |> (\result -> result ++ "." ++ extension)
