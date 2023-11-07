module Data.TagDescription exposing (TagDescription, fromMarkdown, raw, inlineText, markdown, view)

{-| The description for a tag.


# Tag Descriptions

@docs TagDescription, fromMarkdown, raw, inlineText, markdown, view

-}

import Data.MarkdownFragment as MarkdownFragment exposing (MarkdownFragment)
import Html exposing (Attribute, Html, text)
import Html.Attributes exposing (class)
import Markdown.Block exposing (Block)
import Markdown.Renderer as Renderer
import MarkdownRenderers


{-| A tag description.
-}
type TagDescription
    = MarkdownTagDescription
        { body : MarkdownFragment
        , inlineText : String
        }


{-| Construct a tag description from a Markdown string.

    fromMarkdown "The _ideal_ case" |> raw --> "The _ideal_ case"

-}
fromMarkdown : String -> TagDescription
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
    MarkdownTagDescription
        { body = fragment
        , inlineText = inlineTextConcatenated
        }


{-| Retrieve the raw body of a tag description.
-}
raw : TagDescription -> String
raw tagDescription =
    case tagDescription of
        MarkdownTagDescription t ->
            MarkdownFragment.raw t.body


{-| Retrieve the concatenated inline text of a tag description.

    fromMarkdown "*Hello* _there_"
    |> inlineText
    --> "Hello there"

-}
inlineText : TagDescription -> String
inlineText tagDescription =
    case tagDescription of
        MarkdownTagDescription t ->
            t.inlineText


{-| Convert a tag description to a string suitable for a Markdown document.
-}
markdown : TagDescription -> String
markdown tagDescription =
    case tagDescription of
        MarkdownTagDescription t ->
            MarkdownFragment.raw t.body


{-| View a tag description as HTML.

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
view : Bool -> List (Attribute msg) -> TagDescription -> Html msg
view enableMathSupport additionalAttributes tagDescription =
    case tagDescription of
        MarkdownTagDescription t ->
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
                            text <| "Failed to render Markdown: " ++ renderingError

                Err parsingError ->
                    text <| "Failed to parse Markdown: " ++ parsingError
