module Data.GlossaryItem.Definition exposing (Definition, fromMarkdown, raw, inlineText, markdown, view, viewInline, htmlTreeForAnki)

{-| A definition for a glossary item.


# Definitions

@docs Definition, fromMarkdown, raw, inlineText, markdown, view, viewInline, htmlTreeForAnki

-}

import Data.MarkdownFragment as MarkdownFragment exposing (MarkdownFragment)
import Extras.HtmlTree exposing (HtmlTree)
import Html exposing (Attribute, Html, text)
import Html.Attributes exposing (class, style)
import Internationalisation as I18n
import Markdown.Block as Block exposing (Block)
import Markdown.Html
import Markdown.Renderer as Renderer exposing (Renderer)
import MarkdownRenderers


{-| A definition for a glossary item.
-}
type Definition
    = MarkdownDefinition
        { body : MarkdownFragment
        , inlineText : String
        }


{-| Construct a definition from a Markdown string.

    definition : Definition
    definition = fromMarkdown "The _ideal_ case"

    raw definition --> "The _ideal_ case"

-}
fromMarkdown : String -> Definition
fromMarkdown body =
    let
        fragment : MarkdownFragment
        fragment =
            body
                |> MarkdownFragment.fromString
                |> sanitiseMarkdownFragment

        inlineTextConcatenated : String
        inlineTextConcatenated =
            fragment
                |> MarkdownFragment.concatenateInlineText
                |> Result.withDefault body
    in
    MarkdownDefinition
        { body = fragment
        , inlineText = inlineTextConcatenated
        }


{-| Retrieve the raw body of a definition.
-}
raw : Definition -> String
raw (MarkdownDefinition { body }) =
    MarkdownFragment.raw body


{-| Retrieve the concatenated inline text of a definition.

    fromMarkdown "*Hello* _there_"
    |> inlineText
    --> "Hello there"

-}
inlineText : Definition -> String
inlineText (MarkdownDefinition d) =
    d.inlineText


{-| Convert a definition to a string suitable for a Markdown document.
-}
markdown : Definition -> String
markdown (MarkdownDefinition { body }) =
    MarkdownFragment.raw body


{-| View a definition as HTML.

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
    |> view {enableMathSupport = False}
    --> expected

-}
view : { enableMathSupport : Bool } -> Definition -> Html msg
view { enableMathSupport } (MarkdownDefinition { body }) =
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
                        { enableMathSupport = enableMathSupport
                        }
                    )
                    blocks
            of
                Ok rendered ->
                    Html.div
                        [ class "prose print:prose-pre:overflow-x-hidden max-w-3xl prose-pre:bg-inherit prose-pre:text-gray-700 prose-pre:border print:prose-neutral dark:prose-invert dark:prose-pre:text-gray-200 prose-code:before:hidden prose-code:after:hidden leading-normal"
                        , style "overflow-wrap" "anywhere"
                        ]
                        rendered

                Err renderingError ->
                    text <| I18n.failedToRenderMarkdown ++ ": " ++ renderingError

        Err parsingError ->
            text <| I18n.failedToParseMarkdown ++ ": " ++ parsingError


{-| View a definition as inline HTML.
-}
viewInline : Bool -> List (Attribute msg) -> Definition -> Html msg
viewInline enableMathSupport additionalAttributes (MarkdownDefinition { body }) =
    let
        parsed : Result String (List Block)
        parsed =
            MarkdownFragment.parsed body
    in
    case parsed of
        Ok blocks ->
            case Renderer.render (MarkdownRenderers.inlineHtmlMsgRenderer enableMathSupport) blocks of
                Ok rendered ->
                    Html.span
                        (class "prose opacity-75 print:prose-neutral dark:prose-invert dark:prose-pre:text-gray-200 prose-code:before:hidden prose-code:after:hidden leading-normal space-x-1" :: additionalAttributes)
                        rendered

                Err renderingError ->
                    text <| I18n.failedToRenderMarkdown ++ ": " ++ renderingError

        Err parsingError ->
            text <| I18n.failedToParseMarkdown ++ ": " ++ parsingError


htmlTreeRendererForAnki : Bool -> Renderer HtmlTree
htmlTreeRendererForAnki enableMathSupport =
    let
        renderer0 : Renderer HtmlTree
        renderer0 =
            MarkdownRenderers.htmlTreeRendererForAnki enableMathSupport
    in
    { renderer0
        | html =
            Markdown.Html.oneOf
                [ MarkdownRenderers.anchorTagHtmlTreeRenderer
                , MarkdownRenderers.imgTagHtmlTreeRenderer
                ]
    }


{-| Convert a definition to an HtmlTree for Anki.
-}
htmlTreeForAnki : Bool -> Definition -> HtmlTree
htmlTreeForAnki enableMathSupport (MarkdownDefinition { body }) =
    case MarkdownFragment.parsed body of
        Ok blocks ->
            case Renderer.render (htmlTreeRendererForAnki enableMathSupport) blocks of
                Ok rendered ->
                    Extras.HtmlTree.Node "div" False [] rendered

                Err renderingError ->
                    Extras.HtmlTree.Leaf <| I18n.failedToRenderMarkdown ++ ": " ++ renderingError

        Err parsingError ->
            Extras.HtmlTree.Leaf <| I18n.failedToParseMarkdown ++ ": " ++ parsingError


sanitiseMarkdownFragment : MarkdownFragment -> MarkdownFragment
sanitiseMarkdownFragment fragment =
    MarkdownFragment.transform
        (\block ->
            case block of
                Block.Heading level children ->
                    Block.Heading
                        (case level of
                            Block.H1 ->
                                Block.H4

                            Block.H2 ->
                                Block.H4

                            Block.H3 ->
                                Block.H4

                            _ ->
                                level
                        )
                        children

                _ ->
                    block
        )
        fragment
