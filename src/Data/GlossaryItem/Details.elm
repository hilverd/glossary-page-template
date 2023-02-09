module Data.GlossaryItem.Details exposing (Details, fromPlaintext, fromMarkdown, raw, view)

{-| A definition for a glossary item.
This can be in either plain text or Markdown.


# Details

@docs Details, fromPlaintext, fromMarkdown, raw, view

-}

import Data.MarkdownFragment as MarkdownFragment exposing (MarkdownFragment)
import Html exposing (Html, text)
import Html.Attributes as Attr exposing (class)
import Markdown.Block as Block exposing (Block)
import Markdown.Html
import Markdown.Renderer as Renderer exposing (Renderer)


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
    MarkdownDetails { fragment = MarkdownFragment.fromString raw0 }


{-| Retrieve the raw body of details.
-}
raw : Details -> String
raw details =
    case details of
        PlaintextDetails d ->
            d.body

        MarkdownDetails d ->
            MarkdownFragment.raw d.fragment


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
                    case Renderer.render Renderer.defaultHtmlRenderer blocks of
                        Ok rendered ->
                            Html.div
                                [ class "prose print:prose-neutral dark:prose-invert leading-normal" ]
                                rendered

                        Err renderingError ->
                            text <| "Failed to render Markdown: " ++ renderingError

                Err parsingError ->
                    text <| "Failed to parse Markdown: " ++ parsingError
