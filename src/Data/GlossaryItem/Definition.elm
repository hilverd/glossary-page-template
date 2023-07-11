module Data.GlossaryItem.Definition exposing (Definition, fromPlaintext, fromMarkdown, raw, markdown, view, viewInline, htmlTreeForAnki)

{-| A definition for a glossary item.
This can be in either plain text or Markdown.


# Definition

@docs Definition, fromPlaintext, fromMarkdown, raw, markdown, view, viewInline, htmlTreeForAnki

-}

import Data.MarkdownFragment as MarkdownFragment exposing (MarkdownFragment)
import Extras.HtmlTree exposing (HtmlTree)
import Extras.String
import Html exposing (Attribute, Html, text)
import Html.Attributes exposing (class)
import Markdown.Block as Block exposing (Block)
import Markdown.Html
import Markdown.Renderer as Renderer exposing (Renderer)
import MarkdownRenderers


{-| A definition for a glossary item.
-}
type Definition
    = PlaintextDefinition String
    | MarkdownDefinition MarkdownFragment


{-| Construct a definition from a plain text string.

    fromPlaintext "Foo" |> raw --> "Foo"

-}
fromPlaintext : String -> Definition
fromPlaintext =
    PlaintextDefinition


{-| Construct a definition from a Markdown string.

    definition : Definition
    definition = fromMarkdown "The _ideal_ case"

    raw definition --> "The _ideal_ case"

-}
fromMarkdown : String -> Definition
fromMarkdown =
    MarkdownFragment.fromString >> sanitiseMarkdownFragment >> MarkdownDefinition


{-| Retrieve the raw body of a definition.
-}
raw : Definition -> String
raw definition =
    case definition of
        PlaintextDefinition body ->
            body

        MarkdownDefinition fragment ->
            MarkdownFragment.raw fragment


{-| Convert a definition to a string suitable for a Markdown document.
-}
markdown : Definition -> String
markdown definition =
    case definition of
        PlaintextDefinition body ->
            Extras.String.escapeForMarkdown body

        MarkdownDefinition fragment ->
            MarkdownFragment.raw fragment


{-| View a definition as HTML.

    import Html exposing (Html)

    "Foo"
    |> fromPlaintext
    |> view {enableMathSupport = False, makeLinksTabbable = True}
    --> Html.text "Foo"

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
    |> view {enableMathSupport = False, makeLinksTabbable = True}
    --> expected

-}
view : { enableMathSupport : Bool, makeLinksTabbable : Bool } -> Definition -> Html msg
view { enableMathSupport, makeLinksTabbable } definition =
    case definition of
        PlaintextDefinition body ->
            text body

        MarkdownDefinition fragment ->
            let
                parsed : Result String (List Block)
                parsed =
                    MarkdownFragment.parsed fragment
            in
            case parsed of
                Ok blocks ->
                    case
                        Renderer.render
                            (MarkdownRenderers.htmlMsgRenderer
                                { enableMathSupport = enableMathSupport
                                , makeLinksTabbable = makeLinksTabbable
                                }
                            )
                            blocks
                    of
                        Ok rendered ->
                            Html.div
                                [ class "prose print:prose-pre:overflow-x-hidden max-w-3xl prose-pre:bg-inherit prose-pre:text-gray-700 prose-pre:border print:prose-neutral dark:prose-invert dark:prose-pre:text-gray-200 prose-code:before:hidden prose-code:after:hidden leading-normal" ]
                                rendered

                        Err renderingError ->
                            text <| "Failed to render Markdown: " ++ renderingError

                Err parsingError ->
                    text <| "Failed to parse Markdown: " ++ parsingError


{-| View a definition as inline HTML.
-}
viewInline : Bool -> List (Attribute msg) -> Definition -> Html msg
viewInline enableMathSupport additionalAttributes definition =
    case definition of
        PlaintextDefinition body ->
            Html.span additionalAttributes [ text body ]

        MarkdownDefinition fragment ->
            let
                parsed : Result String (List Block)
                parsed =
                    MarkdownFragment.parsed fragment
            in
            case parsed of
                Ok blocks ->
                    case Renderer.render (MarkdownRenderers.inlineHtmlMsgRenderer enableMathSupport) blocks of
                        Ok rendered ->
                            Html.span
                                (class "prose opacity-75 print:prose-neutral dark:prose-invert dark:prose-pre:text-gray-200 prose-code:before:hidden prose-code:after:hidden leading-normal space-x-1" :: additionalAttributes)
                                rendered

                        Err renderingError ->
                            text <| "Failed to render Markdown: " ++ renderingError

                Err parsingError ->
                    text <| "Failed to parse Markdown: " ++ parsingError


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
htmlTreeForAnki enableMathSupport definition =
    case definition of
        PlaintextDefinition body ->
            Extras.HtmlTree.Leaf body

        MarkdownDefinition fragment ->
            case MarkdownFragment.parsed fragment of
                Ok blocks ->
                    case Renderer.render (htmlTreeRendererForAnki enableMathSupport) blocks of
                        Ok rendered ->
                            Extras.HtmlTree.Node "div" False [] rendered

                        Err renderingError ->
                            Extras.HtmlTree.Leaf <| "Failed to render Markdown: " ++ renderingError

                Err parsingError ->
                    Extras.HtmlTree.Leaf <| "Failed to parse Markdown: " ++ parsingError


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
