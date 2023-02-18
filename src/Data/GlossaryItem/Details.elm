module Data.GlossaryItem.Details exposing (Details, fromPlaintext, fromMarkdown, raw, markdown, view)

{-| A definition for a glossary item.
This can be in either plain text or Markdown.


# Details

@docs Details, fromPlaintext, fromMarkdown, raw, markdown, view

-}

import Data.MarkdownFragment as MarkdownFragment exposing (MarkdownFragment)
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
    = PlaintextDetails { body : String }
    | MarkdownDetails { fragment : MarkdownFragment }


{-| Construct details from a plain text string.

    fromPlaintext "Foo" |> raw --> "Foo"

-}
fromPlaintext : String -> Details
fromPlaintext body =
    PlaintextDetails { body = body }


{-| Construct details from a Markdown string.

    details : Details
    details = fromMarkdown "The _ideal_ case"

    raw details --> "The _ideal_ case"

-}
fromMarkdown : String -> Details
fromMarkdown raw0 =
    MarkdownDetails
        { fragment =
            raw0
                |> MarkdownFragment.fromString
                |> sanitiseMarkdownFragment
        }


{-| Retrieve the raw body of details.
-}
raw : Details -> String
raw details =
    case details of
        PlaintextDetails d ->
            d.body

        MarkdownDetails d ->
            MarkdownFragment.raw d.fragment


{-| Convert details to a string suitable for a Markdown document.
-}
markdown : Details -> String
markdown details =
    case details of
        PlaintextDetails d ->
            Extras.String.escapeForMarkdown d.body

        MarkdownDetails d ->
            MarkdownFragment.raw d.fragment


renderer : Renderer (Html msg)
renderer =
    let
        renderer0 =
            Renderer.defaultHtmlRenderer
    in
    { renderer0
        | html =
            Markdown.Html.oneOf
                [ MarkdownRenderers.anchorTagRenderer
                , MarkdownRenderers.imgTagRenderer
                ]
    }


{-| View details as HTML.

    import Html exposing (Html)

    "Foo" |> fromPlaintext |> view --> Html.text "Foo"

    expected : Html msg
    expected =
        Html.div []
            [ Html.p []
                [ Html.text "The "
                , Html.em [] [ Html.text "ideal" ]
                , Html.text " case"
                ]
            ]

    "The _ideal_ case" |> fromMarkdown |> view
    --> expected

-}
view : Details -> Html msg
view details =
    case details of
        PlaintextDetails d ->
            text d.body

        MarkdownDetails d ->
            let
                parsed : Result String (List Block)
                parsed =
                    MarkdownFragment.parsed d.fragment
            in
            case parsed of
                Ok blocks ->
                    case Renderer.render renderer blocks of
                        Ok rendered ->
                            Html.div
                                [ class "prose prose-pre:bg-inherit prose-pre:text-gray-700 prose-pre:border print:prose-neutral dark:prose-invert dark:prose-pre:text-gray-200 prose-code:before:hidden prose-code:after:hidden leading-normal" ]
                                rendered

                        Err renderingError ->
                            text <| "Failed to render Markdown: " ++ renderingError

                Err parsingError ->
                    text <| "Failed to parse Markdown: " ++ parsingError


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
