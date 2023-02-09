module Data.AboutParagraph exposing (AboutParagraph, fromPlaintext, fromMarkdown, raw, view)

{-| The "about" paragraph(s) shown at the top of a glossary.
This can be in either plain text or Markdown.


# "About" Paragraphs

@docs AboutParagraph, fromPlaintext, fromMarkdown, raw, view

-}

import Data.MarkdownFragment as MarkdownFragment exposing (MarkdownFragment)
import Html exposing (Html, text)
import Html.Attributes as Attr exposing (class)
import Markdown.Block as Block exposing (Block)
import Markdown.Html
import Markdown.Renderer as Renderer exposing (Renderer)


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
    MarkdownAboutParagraph << MarkdownFragment.fromString


{-| Retrieve the raw body of an "about" paragraph.
-}
raw : AboutParagraph -> String
raw aboutParagraph =
    case aboutParagraph of
        PlaintextAboutParagraph body ->
            body

        MarkdownAboutParagraph fragment ->
            MarkdownFragment.raw fragment


{-| View an "about" paragraph as HTML.

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
view : AboutParagraph -> Html msg
view aboutParagraph =
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
                    case Renderer.render htmlRenderer blocks of
                        Ok rendered ->
                            Html.div
                                [ class "prose print:prose-neutral dark:prose-invert leading-normal" ]
                                rendered

                        Err renderingError ->
                            text <| "Failed to render Markdown: " ++ renderingError

                Err parsingError ->
                    text <| "Failed to parse Markdown: " ++ parsingError


htmlRenderer : Renderer (Html msg)
htmlRenderer =
    { heading =
        \{ level, children } ->
            case level of
                Block.H1 ->
                    Html.h1 [] children

                Block.H2 ->
                    Html.h2 [] children

                Block.H3 ->
                    Html.h3 [] children

                Block.H4 ->
                    Html.h4 [] children

                Block.H5 ->
                    Html.h5 [] children

                Block.H6 ->
                    Html.h6 [] children
    , paragraph = Html.p []
    , hardLineBreak = Html.br [] []
    , blockQuote = Html.blockquote []
    , strong =
        \children -> Html.strong [] children
    , emphasis =
        \children -> Html.em [] children
    , codeSpan =
        \content -> Html.code [] [ Html.text content ]
    , link =
        \link content ->
            case link.title of
                Just title ->
                    Html.a
                        [ Attr.href link.destination
                        , Attr.title title
                        ]
                        content

                Nothing ->
                    Html.a [ Attr.href link.destination ] content
    , image =
        \imageInfo ->
            case imageInfo.title of
                Just title ->
                    Html.img
                        [ Attr.src imageInfo.src
                        , Attr.alt imageInfo.alt
                        , Attr.title title
                        ]
                        []

                Nothing ->
                    Html.img
                        [ Attr.src imageInfo.src
                        , Attr.alt imageInfo.alt
                        ]
                        []
    , text =
        Html.text
    , unorderedList =
        \items ->
            Html.ul []
                (items
                    |> List.map
                        (\item ->
                            case item of
                                Block.ListItem task children ->
                                    let
                                        checkbox =
                                            case task of
                                                Block.NoTask ->
                                                    Html.text ""

                                                Block.IncompleteTask ->
                                                    Html.input
                                                        [ Attr.disabled True
                                                        , Attr.checked False
                                                        , Attr.type_ "checkbox"
                                                        ]
                                                        []

                                                Block.CompletedTask ->
                                                    Html.input
                                                        [ Attr.disabled True
                                                        , Attr.checked True
                                                        , Attr.type_ "checkbox"
                                                        ]
                                                        []
                                    in
                                    Html.li [] (checkbox :: children)
                        )
                )
    , orderedList =
        \startingIndex items ->
            Html.ol
                (case startingIndex of
                    1 ->
                        [ Attr.start startingIndex ]

                    _ ->
                        []
                )
                (items
                    |> List.map
                        (\itemBlocks ->
                            Html.li []
                                itemBlocks
                        )
                )
    , html = Markdown.Html.oneOf []
    , codeBlock =
        \block ->
            Html.pre []
                [ Html.code []
                    [ Html.text block.body
                    ]
                ]
    , thematicBreak = Html.hr [] []
    , table = Html.table []
    , tableHeader = Html.thead []
    , tableBody = Html.tbody []
    , tableRow = Html.tr []
    , tableHeaderCell =
        \maybeAlignment ->
            let
                attrs =
                    maybeAlignment
                        |> Maybe.map
                            (\alignment ->
                                case alignment of
                                    Block.AlignLeft ->
                                        "left"

                                    Block.AlignCenter ->
                                        "center"

                                    Block.AlignRight ->
                                        "right"
                            )
                        |> Maybe.map Attr.align
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
            in
            Html.th attrs
    , tableCell = \_ children -> Html.td [] children
    , strikethrough = Html.span [ Attr.style "text-decoration-line" "line-through" ]
    }
