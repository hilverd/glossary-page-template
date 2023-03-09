module Data.AboutParagraph exposing (AboutParagraph, fromPlaintext, fromMarkdown, raw, markdown, view)

{-| The "about" paragraph(s) shown at the top of a glossary.
This can be in either plain text or Markdown.


# "About" Paragraphs

@docs AboutParagraph, fromPlaintext, fromMarkdown, raw, markdown, view

-}

import Data.MarkdownFragment as MarkdownFragment exposing (MarkdownFragment)
import Extras.String
import Html exposing (Html, text)
import Html.Attributes exposing (class)
import Markdown.Block as Block exposing (Block)
import Markdown.Renderer as Renderer
import MarkdownRenderers


{-| An opaque type for an "about" paragraph which is just a wrapper around a string.
-}
type AboutParagraph
    = PlaintextAboutParagraph String
    | MarkdownAboutParagraph MarkdownFragment


{-| Construct an "about" paragraph from a string.

    fromPlaintext "Foo" |> raw --> "Foo"

-}
fromPlaintext : String -> AboutParagraph
fromPlaintext =
    PlaintextAboutParagraph


{-| Construct an "about" paragraph from a Markdown string.

    aboutParagraph : AboutParagraph
    aboutParagraph = fromMarkdown "The _ideal_ case"

    raw aboutParagraph --> "The _ideal_ case"

-}
fromMarkdown : String -> AboutParagraph
fromMarkdown =
    MarkdownAboutParagraph << sanitiseMarkdownFragment << MarkdownFragment.fromString


{-| Retrieve the raw body of an "about" paragraph.
-}
raw : AboutParagraph -> String
raw aboutParagraph =
    case aboutParagraph of
        PlaintextAboutParagraph body ->
            body

        MarkdownAboutParagraph fragment ->
            MarkdownFragment.raw fragment


{-| Convert an "about" paragraph to a string suitable for a Markdown document.
-}
markdown : AboutParagraph -> String
markdown aboutParagraph =
    case aboutParagraph of
        PlaintextAboutParagraph body ->
            Extras.String.escapeForMarkdown body

        MarkdownAboutParagraph fragment ->
            MarkdownFragment.raw fragment


{-| View an "about" paragraph as HTML.

    import Html exposing (Html)

    "Foo" |> fromPlaintext |> view False --> Html.text "Foo"

    expected : Html msg
    expected =
        Html.div []
            [ Html.p []
                [ Html.text "The "
                , Html.em [] [ Html.text "ideal" ]
                , Html.text " case"
                ]
            ]

    "The _ideal_ case" |> fromMarkdown |> view False
    --> expected

-}
view : Bool -> AboutParagraph -> Html msg
view enableMathSupport aboutParagraph =
    case aboutParagraph of
        PlaintextAboutParagraph body ->
            text body

        MarkdownAboutParagraph fragment ->
            let
                parsed : Result String (List Block)
                parsed =
                    MarkdownFragment.parsed fragment
            in
            case parsed of
                Ok blocks ->
                    case Renderer.render (MarkdownRenderers.htmlMsgRenderer enableMathSupport) blocks of
                        Ok rendered ->
                            Html.div
                                [ class "prose print:prose-pre:overflow-x-hidden max-w-3xl prose-pre:bg-inherit prose-pre:text-gray-700 prose-pre:border print:prose-neutral dark:prose-invert dark:prose-pre:text-gray-200 prose-code:before:hidden prose-code:after:hidden leading-normal" ]
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
                                Block.H3

                            Block.H2 ->
                                Block.H3

                            _ ->
                                level
                        )
                        children

                _ ->
                    block
        )
        fragment
