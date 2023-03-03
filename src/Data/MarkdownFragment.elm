module Data.MarkdownFragment exposing (MarkdownFragment, fromString, raw, parsed, transform, inlineFoldl, concatenateInlineText)

{-| A Markdown fragment is a string that has been parsed as Markdown.


# Markdown Fragments

@docs MarkdownFragment, fromString, raw, parsed, transform, inlineFoldl, concatenateInlineText

-}

import Components.DropdownMenu exposing (init)
import Markdown.Block as Block exposing (Block, Inline)
import Markdown.Parser


{-| The result of parsing a string as Markdown.
-}
type MarkdownFragment
    = MarkdownFragment
        { raw : String
        , parsed : Result String (List Block)
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


{-| Retrieve the raw string that was used to construct this Markdown fragment.

    import Markdown.Block as Block exposing (Block)

    expected : Result String (List Block)
    expected =
        Ok
            [ Block.Paragraph
                [ Block.Emphasis
                    [ Block.Text "Hello" ]
                ]
            ]

    "*Hello*" |> fromString |> parsed --> expected

-}
parsed : MarkdownFragment -> Result String (List Block)
parsed markdownFragment =
    case markdownFragment of
        MarkdownFragment fragment ->
            fragment.parsed


{-| Transform a Markdown fragment by recursively applying the given function.
-}
transform : (Block -> Block) -> MarkdownFragment -> MarkdownFragment
transform f markdownFragment =
    case markdownFragment of
        MarkdownFragment fragment ->
            MarkdownFragment
                { raw = fragment.raw
                , parsed = Result.map (List.map <| Block.walk f) fragment.parsed
                }


{-| Fold over all inlines within a Markdown fragment to yield a value.
-}
inlineFoldl : (Inline -> acc -> acc) -> acc -> MarkdownFragment -> Result String acc
inlineFoldl f initial markdownFragment =
    case markdownFragment of
        MarkdownFragment fragment ->
            Result.map (Block.inlineFoldl f initial) fragment.parsed


{-| Concatenate the inlines within a Markdown fragment to produce a string.

    "*Hello* _there_"
    |> fromString
    |> concatenateInlineText
    --> Ok "Hello there"

-}
concatenateInlineText : MarkdownFragment -> Result String String
concatenateInlineText =
    inlineFoldl
        (\inline result ->
            case inline of
                Block.CodeSpan str ->
                    str :: result

                Block.Text str ->
                    str :: result

                _ ->
                    result
        )
        []
        >> Result.map (List.reverse >> String.concat)
