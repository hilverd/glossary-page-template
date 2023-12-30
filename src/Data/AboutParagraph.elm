module Data.AboutParagraph exposing (AboutParagraph, fromMarkdown, raw, markdown, view, codec)

{-| The "about" paragraph(s) shown at the top of a glossary.


# "About" Paragraphs

@docs AboutParagraph, fromMarkdown, raw, markdown, view, codec

-}

import Codec exposing (Codec)
import Data.MarkdownFragment as MarkdownFragment exposing (MarkdownFragment)
import Html exposing (Html, text)
import Html.Attributes exposing (class)
import Internationalisation as I18n
import Markdown.Block as Block exposing (Block)
import Markdown.Renderer as Renderer
import MarkdownRenderers


{-| An opaque type for an "about" paragraph which is just a wrapper around a string.
-}
type AboutParagraph
    = MarkdownAboutParagraph MarkdownFragment


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
        MarkdownAboutParagraph fragment ->
            MarkdownFragment.raw fragment


{-| Convert an "about" paragraph to a string suitable for a Markdown document.
-}
markdown : AboutParagraph -> String
markdown aboutParagraph =
    case aboutParagraph of
        MarkdownAboutParagraph fragment ->
            MarkdownFragment.raw fragment


{-| View an "about" paragraph as HTML.

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
    |> view {enableMathSupport = False, makeLinksTabbable = True}
    --> expected

-}
view : { enableMathSupport : Bool, makeLinksTabbable : Bool } -> AboutParagraph -> Html msg
view { enableMathSupport, makeLinksTabbable } aboutParagraph =
    case aboutParagraph of
        MarkdownAboutParagraph fragment ->
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
                            text <| I18n.failedToRenderMarkdown ++ ": " ++ renderingError

                Err parsingError ->
                    text <| I18n.failedToParseMarkdown ++ ": " ++ parsingError


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


{-| An encoder/decoder for an "about" paragraph.
-}
codec : Codec AboutParagraph
codec =
    Codec.map fromMarkdown raw Codec.string
