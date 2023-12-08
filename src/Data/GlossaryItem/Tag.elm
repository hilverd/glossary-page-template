module Data.GlossaryItem.Tag exposing (Tag, fromMarkdown, decode, raw, inlineText, markdown, compareAlphabetically, view, fromQuery, toQueryParameter)

{-| A tag that can be used in glossary items.

Tags can be used to group items together that belong in the same "context" or share some important property. They should probably be used sparingly as too many tags could be an indication of a glossary trying to focus on too many different topics at once.


# Tags

@docs Tag, fromMarkdown, decode, raw, inlineText, markdown, compareAlphabetically, view, fromQuery, toQueryParameter

-}

import Data.MarkdownFragment as MarkdownFragment exposing (MarkdownFragment)
import Extras.String
import Html exposing (Attribute, Html, text)
import Html.Attributes exposing (class)
import Internationalisation as I18n
import Json.Decode as Decode exposing (Decoder)
import Markdown.Block exposing (Block)
import Markdown.Renderer as Renderer
import MarkdownRenderers
import String.Normalize
import Url.Builder
import Url.Parser.Query


{-| A tag.
-}
type Tag
    = MarkdownTag
        { body : MarkdownFragment
        , inlineText : String
        }


{-| Construct a tag from a Markdown string.

    fromMarkdown "The _ideal_ case" |> raw --> "The _ideal_ case"

-}
fromMarkdown : String -> Tag
fromMarkdown body =
    let
        fragment : MarkdownFragment
        fragment =
            MarkdownFragment.fromString body

        inlineTextConcatenated : String
        inlineTextConcatenated =
            fragment
                |> MarkdownFragment.concatenateInlineText
                |> Result.withDefault body
    in
    MarkdownTag
        { body = fragment
        , inlineText = inlineTextConcatenated
        }


{-| Decode a tag from its JSON representation.
-}
decode : Decoder Tag
decode =
    Decode.map
        fromMarkdown
        Decode.string


{-| Retrieve the raw body of a tag.
-}
raw : Tag -> String
raw tag =
    case tag of
        MarkdownTag t ->
            MarkdownFragment.raw t.body


{-| Retrieve the concatenated inline text of a tag.

    fromMarkdown "*Hello* _there_"
    |> inlineText
    --> "Hello there"

-}
inlineText : Tag -> String
inlineText tag =
    case tag of
        MarkdownTag t ->
            t.inlineText


{-| Convert a tag to a string suitable for a Markdown document.
-}
markdown : Tag -> String
markdown tag =
    case tag of
        MarkdownTag t ->
            MarkdownFragment.raw t.body


{-| Compares two tags for ordering them alphabetically.
-}
compareAlphabetically : Tag -> Tag -> Order
compareAlphabetically tag1 tag2 =
    compare
        (tag1
            |> inlineText
            |> String.Normalize.removeDiacritics
            |> Extras.String.preserveOnlyAlphaNumChars
            |> String.toUpper
        )
        (tag2
            |> inlineText
            |> String.Normalize.removeDiacritics
            |> Extras.String.preserveOnlyAlphaNumChars
            |> String.toUpper
        )


{-| View a tag as HTML.

    import Html exposing (Html)

    expected : Html msg
    expected =
        Html.span []
            [ Html.span []
                [ Html.text "The "
                , Html.em [] [ Html.text "ideal" ]
                , Html.text " case"
                ]
            ]

    fromMarkdown "The _ideal_ case" |> view False []
    --> expected

-}
view : Bool -> List (Attribute msg) -> Tag -> Html msg
view enableMathSupport additionalAttributes tag =
    case tag of
        MarkdownTag t ->
            let
                parsed : Result String (List Block)
                parsed =
                    MarkdownFragment.parsed t.body
            in
            case parsed of
                Ok blocks ->
                    case Renderer.render (MarkdownRenderers.inlineHtmlMsgRenderer enableMathSupport) blocks of
                        Ok rendered ->
                            Html.span
                                (class "prose dark:prose-invert print:prose-neutral dark:prose-pre:text-gray-200 prose-code:before:hidden prose-code:after:hidden leading-normal" :: additionalAttributes)
                                rendered

                        Err renderingError ->
                            text <| I18n.failedToRenderMarkdown ++ ": " ++ renderingError

                Err parsingError ->
                    text <| I18n.failedToParseMarkdown ++ ": " ++ parsingError


{-| Obtain the tag being filtered by according to query parameters in the URL.
-}
fromQuery : Url.Parser.Query.Parser (Maybe Tag)
fromQuery =
    Url.Parser.Query.custom "filter-by-tag"
        (\strings ->
            case strings of
                [ string ] ->
                    string
                        |> fromMarkdown
                        |> Just

                _ ->
                    Nothing
        )


{-| Represent a tag being filtered by as a query parameter.
-}
toQueryParameter : Tag -> Url.Builder.QueryParameter
toQueryParameter =
    Url.Builder.string "filter-by-tag" << raw
