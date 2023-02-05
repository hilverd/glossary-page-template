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
