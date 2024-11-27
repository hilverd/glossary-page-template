module Data.TagDescription exposing (TagDescription, fromMarkdown, raw, codec, markdown, view)

{-| The description for a tag.


# Tag Descriptions

@docs TagDescription, fromMarkdown, raw, codec, markdown, view

-}

import Codec exposing (Codec)
import Data.MarkdownFragment as MarkdownFragment exposing (MarkdownFragment)
import Html exposing (Attribute, Html, text)
import Html.Attributes exposing (class)
import Internationalisation as I18n
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
raw (MarkdownTagDescription { body }) =
    MarkdownFragment.raw body


{-| An encoder/decoder for tag descriptions.
-}
codec : Codec TagDescription
codec =
    Codec.map fromMarkdown raw Codec.string


{-| Convert a tag description to a string suitable for a Markdown document.
-}
markdown : TagDescription -> String
markdown (MarkdownTagDescription { body }) =
    MarkdownFragment.raw body


{-| View a tag description as HTML.

    import Html exposing (Html)

    expected : Html msg
    expected =
        Html.div []
            [ Html.p []
                [ Html.text "The "
                , Html.em [] [ Html.text "ideal" ]
                , Html.text " case"
                ]
            ]

    "The _ideal_ case"
    |> fromMarkdown
    |> view {enableMathSupport = False, makeLinksTabbable = True} []
    --> expected

-}
view : { enableMathSupport : Bool } -> List (Attribute msg) -> TagDescription -> Html msg
view { enableMathSupport } additionalAttributes (MarkdownTagDescription { body }) =
    let
        parsed : Result String (List Block)
        parsed =
            MarkdownFragment.parsed body
    in
    case parsed of
        Ok blocks ->
            case
                Renderer.render
                    (MarkdownRenderers.htmlMsgRenderer
                        { enableMathSupport = enableMathSupport }
                    )
                    blocks
            of
                Ok rendered ->
                    Html.div
                        (class "prose dark:prose-invert print:prose-neutral dark:prose-pre:text-gray-200 prose-code:before:hidden prose-code:after:hidden leading-normal" :: additionalAttributes)
                        rendered

                Err renderingError ->
                    text <| I18n.failedToRenderMarkdown ++ ": " ++ renderingError

        Err parsingError ->
            text <| I18n.failedToParseMarkdown ++ ": " ++ parsingError
