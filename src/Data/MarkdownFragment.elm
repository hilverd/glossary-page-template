module Data.MarkdownFragment exposing (MarkdownFragment, fromString, raw)

{-| A Markdown fragment is a string that has been parsed as Markdown.


# Markdown Fragments

@docs MarkdownFragment, fromString, raw

-}

import Markdown.Block
import Markdown.Parser


{-| The result of parsing a string as Markdown.
-}
type MarkdownFragment
    = MarkdownFragment
        { raw : String
        , parsed : Result String (List Markdown.Block.Block)
        }


{-| Build a Markdown fragment by parsing a string.
-}
fromString : String -> MarkdownFragment
fromString raw0 =
    MarkdownFragment
        { raw = raw0
        , parsed =
            raw0
                |> Markdown.Parser.parse
                |> Result.mapError (List.map Markdown.Parser.deadEndToString >> String.join "\n")
        }


{-| Retrieve the raw string that was used to construct this Markdown fragment.

    "*Hello*" |> fromString |> raw --> "*Hello*"

-}
raw : MarkdownFragment -> String
raw markdownFragment =
    case markdownFragment of
        MarkdownFragment fragment ->
            fragment.raw
