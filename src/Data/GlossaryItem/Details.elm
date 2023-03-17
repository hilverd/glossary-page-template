module Data.GlossaryItem.Details exposing (Details, fromPlaintext, fromMarkdown, raw, markdown, view, htmlTree)

{-| A definition for a glossary item.
This can be in either plain text or Markdown.


# Details

@docs Details, fromPlaintext, fromMarkdown, raw, markdown, view, htmlTree

-}

import Data.MarkdownFragment as MarkdownFragment exposing (MarkdownFragment)
import Extras.HtmlTree exposing (HtmlTree)
import Extras.String
import Html exposing (Html, text)
import Html.Attributes exposing (class)
import Markdown.Block as Block exposing (Block)
import Markdown.Html
import Markdown.Renderer as Renderer exposing (Renderer)
import MarkdownRenderers


{-| Details (i.e. a definition) for a glossary item.
-}
type Details
    = PlaintextDetails String
    | MarkdownDetails MarkdownFragment


{-| Construct details from a plain text string.

    fromPlaintext "Foo" |> raw --> "Foo"

-}
fromPlaintext : String -> Details
fromPlaintext =
    PlaintextDetails


{-| Construct details from a Markdown string.

    details : Details
    details = fromMarkdown "The _ideal_ case"

    raw details --> "The _ideal_ case"

-}
fromMarkdown : String -> Details
fromMarkdown =
    MarkdownFragment.fromString >> sanitiseMarkdownFragment >> MarkdownDetails


{-| Retrieve the raw body of details.
-}
raw : Details -> String
raw details =
    case details of
        PlaintextDetails body ->
            body

        MarkdownDetails fragment ->
            MarkdownFragment.raw fragment


{-| Convert details to a string suitable for a Markdown document.
-}
markdown : Details -> String
markdown details =
    case details of
        PlaintextDetails body ->
            Extras.String.escapeForMarkdown body

        MarkdownDetails fragment ->
            MarkdownFragment.raw fragment


{-| View details as HTML.

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
view : { enableMathSupport : Bool, makeLinksTabbable : Bool } -> Details -> Html msg
view { enableMathSupport, makeLinksTabbable } details =
    case details of
        PlaintextDetails body ->
            text body

        MarkdownDetails fragment ->
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


htmlTreeRenderer : Renderer HtmlTree
htmlTreeRenderer =
    let
        renderer0 : Renderer HtmlTree
        renderer0 =
            MarkdownRenderers.htmlTreeRenderer
    in
    { renderer0
        | html =
            Markdown.Html.oneOf
                [ MarkdownRenderers.anchorTagHtmlTreeRenderer
                , MarkdownRenderers.imgTagHtmlTreeRenderer
                ]
    }


{-| Convert details to an HtmlTree.
-}
htmlTree : Details -> HtmlTree
htmlTree details =
    case details of
        PlaintextDetails body ->
            Extras.HtmlTree.Leaf body

        MarkdownDetails fragment ->
            case MarkdownFragment.parsed fragment of
                Ok blocks ->
                    case Renderer.render htmlTreeRenderer blocks of
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
