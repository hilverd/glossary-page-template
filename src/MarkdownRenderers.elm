module MarkdownRenderers exposing
    ( anchorTagHtmlTreeRenderer, anchorTagRenderer, htmlMsgRenderer, htmlTreeRenderer, imgTagHtmlTreeRenderer, imgTagRenderer, inlineHtmlMsgRenderer
    , inlineHtmlTreeRenderer
    )

{-| Renderers for Markdown data.


# Markdown Renderers

@docs anchorTagHtmlTreeRenderer, anchorTagRenderer, htmlMsgRenderer, htmlTreeRenderer, imgTagHtmlTreeRenderer, imgTagRenderer, inlineHtmlMsgRenderer

-}

import Extras.HtmlAttribute
import Extras.HtmlTree as HtmlTree exposing (HtmlTree)
import Html exposing (Html)
import Html.Attributes as Attr exposing (class)
import Markdown.Block as Block
import Markdown.Html
import Markdown.Renderer as Renderer exposing (Renderer)


{-| Render an anchor tag as HTML.
-}
anchorTagRenderer : Markdown.Html.Renderer (List (Html msg) -> Html msg)
anchorTagRenderer =
    anchorTagRendererForViewFunction viewAnchorTag


{-| Render an anchor tag as an HtmlTree.
-}
anchorTagHtmlTreeRenderer : Markdown.Html.Renderer (List HtmlTree -> HtmlTree)
anchorTagHtmlTreeRenderer =
    anchorTagRendererForViewFunction anchorTagToHtmlTree


anchorTagRendererForViewFunction :
    (Maybe String -> Maybe String -> Maybe String -> List view -> view)
    -> Markdown.Html.Renderer (List view -> view)
anchorTagRendererForViewFunction viewFunction =
    Markdown.Html.tag "a" viewFunction
        |> Markdown.Html.withOptionalAttribute "href"
        |> Markdown.Html.withOptionalAttribute "target"
        |> Markdown.Html.withOptionalAttribute "style"


viewAnchorTag :
    Maybe String
    -> Maybe String
    -> Maybe String
    -> List (Html msg)
    -> Html msg
viewAnchorTag href target style renderedChildren =
    Html.a
        [ Extras.HtmlAttribute.showMaybe Attr.href href
        , Extras.HtmlAttribute.showMaybe Attr.target target
        , Extras.HtmlAttribute.showMaybe (Attr.attribute "style") style
        ]
        renderedChildren


{-| Render Markdown to HTML.
-}
htmlMsgRenderer : Renderer (Html msg)
htmlMsgRenderer =
    let
        renderer0 : Renderer (Html msg)
        renderer0 =
            Renderer.defaultHtmlRenderer
    in
    { renderer0
        | paragraph = Html.p [ class "max-w-prose" ]
        , blockQuote = Html.blockquote [ class "max-w-prose" ]
        , html =
            Markdown.Html.oneOf
                [ anchorTagRenderer
                , imgTagRenderer
                ]
    }


{-| A renderer that only handles inline elements (e.g. strong, emphasis) properly.
Block elements are either ignored or have their children wrapped in a <span>.
-}
inlineHtmlMsgRenderer : Renderer (Html msg)
inlineHtmlMsgRenderer =
    { heading = \{ children } -> Html.span [] children
    , paragraph = Html.span []
    , hardLineBreak = Html.text ""
    , blockQuote = Html.span []
    , strong = Html.strong []
    , emphasis = Html.em []
    , strikethrough = Html.del []
    , codeSpan = \content -> Html.code [] [ Html.text content ]
    , link = always <| Html.span []
    , image = always <| Html.text ""
    , text = Html.text
    , unorderedList = always <| Html.text ""
    , orderedList = \_ _ -> Html.text ""
    , html = Markdown.Html.oneOf []
    , codeBlock = always <| Html.text ""
    , thematicBreak = Html.span [] []
    , table = Html.span []
    , tableHeader = Html.span []
    , tableBody = Html.span []
    , tableRow = Html.span []
    , tableHeaderCell = always <| Html.span []
    , tableCell = always <| Html.span []
    }


anchorTagToHtmlTree :
    Maybe String
    -> Maybe String
    -> Maybe String
    -> List HtmlTree
    -> HtmlTree
anchorTagToHtmlTree href target style renderedChildren =
    HtmlTree.Node "a"
        True
        ([ Maybe.map (HtmlTree.Attribute "href") href
         , Maybe.map (HtmlTree.Attribute "target") target
         , Maybe.map (HtmlTree.Attribute "style") style
         ]
            |> List.filterMap identity
        )
        renderedChildren


{-| Render an image tag as HTML.
-}
imgTagRenderer : Markdown.Html.Renderer (List (Html msg) -> Html msg)
imgTagRenderer =
    imgTagRendererForViewFunction viewImgTag


{-| Render an image tags as an HtmlTree.
-}
imgTagHtmlTreeRenderer : Markdown.Html.Renderer (List HtmlTree -> HtmlTree)
imgTagHtmlTreeRenderer =
    imgTagRendererForViewFunction imgTagToHtmlTree


imgTagRendererForViewFunction :
    (Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> List view -> view)
    -> Markdown.Html.Renderer (List view -> view)
imgTagRendererForViewFunction viewFunction =
    Markdown.Html.tag "img" viewFunction
        |> Markdown.Html.withOptionalAttribute "src"
        |> Markdown.Html.withOptionalAttribute "width"
        |> Markdown.Html.withOptionalAttribute "height"
        |> Markdown.Html.withOptionalAttribute "alt"
        |> Markdown.Html.withOptionalAttribute "title"
        |> Markdown.Html.withOptionalAttribute "style"


viewImgTag :
    Maybe String
    -> Maybe String
    -> Maybe String
    -> Maybe String
    -> Maybe String
    -> Maybe String
    -> List (Html msg)
    -> Html msg
viewImgTag src width height alt title style _ =
    Html.img
        [ Extras.HtmlAttribute.showMaybe Attr.src src
        , Extras.HtmlAttribute.showMaybe Attr.width (Maybe.andThen String.toInt width)
        , Extras.HtmlAttribute.showMaybe Attr.height (Maybe.andThen String.toInt height)
        , Extras.HtmlAttribute.showMaybe Attr.alt alt
        , Extras.HtmlAttribute.showMaybe Attr.title title
        , Extras.HtmlAttribute.showMaybe (Attr.attribute "style") style
        ]
        []


imgTagToHtmlTree :
    Maybe String
    -> Maybe String
    -> Maybe String
    -> Maybe String
    -> Maybe String
    -> Maybe String
    -> List HtmlTree
    -> HtmlTree
imgTagToHtmlTree src width height alt title style _ =
    HtmlTree.Node "img"
        True
        ([ Maybe.map (HtmlTree.Attribute "src") src
         , width |> Maybe.map (HtmlTree.Attribute "width")
         , height |> Maybe.map (HtmlTree.Attribute "height")
         , Maybe.map (HtmlTree.Attribute "alt") alt
         , Maybe.map (HtmlTree.Attribute "title") title
         , Maybe.map (HtmlTree.Attribute "style") style
         ]
            |> List.filterMap identity
        )
        []


{-| Render Markdown as an HtmlTree.
-}
htmlTreeRenderer : Renderer HtmlTree
htmlTreeRenderer =
    { heading =
        \{ level, children } ->
            case level of
                Block.H1 ->
                    HtmlTree.Node "h1" False [] children

                Block.H2 ->
                    HtmlTree.Node "h2" False [] children

                Block.H3 ->
                    HtmlTree.Node "h3" False [] children

                Block.H4 ->
                    HtmlTree.Node "h4" False [] children

                Block.H5 ->
                    HtmlTree.Node "h5" False [] children

                Block.H6 ->
                    HtmlTree.Node "h6" False [] children
    , paragraph = HtmlTree.Node "p" False []
    , hardLineBreak = HtmlTree.Node "br" False [] []
    , blockQuote = HtmlTree.Node "blockquote" False []
    , strong =
        \children -> HtmlTree.Node "strong" False [] children
    , emphasis =
        \children -> HtmlTree.Node "em" False [] children
    , codeSpan =
        \content -> HtmlTree.Node "code" False [] [ HtmlTree.Leaf content ]
    , link =
        \link content ->
            case link.title of
                Just title ->
                    HtmlTree.Node "a"
                        False
                        [ HtmlTree.Attribute "href" link.destination
                        , HtmlTree.Attribute "title" title
                        ]
                        content

                Nothing ->
                    HtmlTree.Node "a" False [ HtmlTree.Attribute "href" link.destination ] content
    , image =
        \imageInfo ->
            case imageInfo.title of
                Just title ->
                    HtmlTree.Node "img"
                        False
                        [ HtmlTree.Attribute "src" imageInfo.src
                        , HtmlTree.Attribute "alt" imageInfo.alt
                        , HtmlTree.Attribute "title" title
                        ]
                        []

                Nothing ->
                    HtmlTree.Node "img"
                        False
                        [ HtmlTree.Attribute "src" imageInfo.src
                        , HtmlTree.Attribute "alt" imageInfo.alt
                        ]
                        []
    , text =
        HtmlTree.Leaf
    , unorderedList =
        \items ->
            HtmlTree.Node "ul"
                False
                []
                (items
                    |> List.map
                        (\item ->
                            case item of
                                Block.ListItem task children ->
                                    let
                                        checkbox : HtmlTree
                                        checkbox =
                                            case task of
                                                Block.NoTask ->
                                                    HtmlTree.Leaf ""

                                                Block.IncompleteTask ->
                                                    HtmlTree.Node "input"
                                                        False
                                                        [ HtmlTree.Attribute "disabled" "true"
                                                        , HtmlTree.Attribute "checked" "false"
                                                        , HtmlTree.Attribute "type_" "checkbox"
                                                        ]
                                                        []

                                                Block.CompletedTask ->
                                                    HtmlTree.Node "input"
                                                        False
                                                        [ HtmlTree.Attribute "disabled" "true"
                                                        , HtmlTree.Attribute "checked" "true"
                                                        , HtmlTree.Attribute "type_" "checkbox"
                                                        ]
                                                        []
                                    in
                                    HtmlTree.Node "li" False [] (checkbox :: children)
                        )
                )
    , orderedList =
        \startingIndex items ->
            HtmlTree.Node "ol"
                False
                (case startingIndex of
                    1 ->
                        [ HtmlTree.Attribute "start" "1" ]

                    _ ->
                        []
                )
                (items
                    |> List.map
                        (\itemBlocks ->
                            HtmlTree.Node "li"
                                False
                                []
                                itemBlocks
                        )
                )
    , html = Markdown.Html.oneOf []
    , codeBlock =
        \block ->
            HtmlTree.Node "pre"
                False
                []
                [ HtmlTree.Node "code"
                    False
                    []
                    [ HtmlTree.Leaf block.body
                    ]
                ]
    , thematicBreak = HtmlTree.Node "hr" False [] []
    , table = HtmlTree.Node "table" False []
    , tableHeader = HtmlTree.Node "thead" False []
    , tableBody = HtmlTree.Node "tbody" False []
    , tableRow = HtmlTree.Node "tr" False []
    , tableHeaderCell =
        \maybeAlignment ->
            let
                attrs : List HtmlTree.Attribute
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
                        |> Maybe.map (HtmlTree.Attribute "align")
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
            in
            HtmlTree.Node "th" False attrs
    , tableCell = \_ children -> HtmlTree.Node "td" False [] children
    , strikethrough = HtmlTree.Node "span" False [ HtmlTree.Attribute "style" "text-decoration-line: line-through" ]
    }


{-| A HtmlTree renderer that only handles inline elements (e.g. strong, emphasis) properly.
Block elements are either ignored or have their children wrapped in a <span>.
-}
inlineHtmlTreeRenderer : Renderer HtmlTree
inlineHtmlTreeRenderer =
    { heading = \{ children } -> HtmlTree.Node "span" False [] children
    , paragraph = HtmlTree.Node "span" False []
    , hardLineBreak = HtmlTree.Leaf ""
    , blockQuote = HtmlTree.Node "span" False []
    , strong = HtmlTree.Node "strong" False []
    , emphasis = HtmlTree.Node "em" False []
    , strikethrough = HtmlTree.Node "del" False []
    , codeSpan = \content -> HtmlTree.Node "code" False [] [ HtmlTree.Leaf content ]
    , link = always <| HtmlTree.Node "span" False []
    , image = always <| HtmlTree.Leaf ""
    , text = HtmlTree.Leaf
    , unorderedList = always <| HtmlTree.Leaf ""
    , orderedList = \_ _ -> HtmlTree.Leaf ""
    , html = Markdown.Html.oneOf []
    , codeBlock = always <| HtmlTree.Leaf ""
    , thematicBreak = HtmlTree.Node "span" False [] []
    , table = HtmlTree.Node "span" False []
    , tableHeader = HtmlTree.Node "span" False []
    , tableBody = HtmlTree.Node "span" False []
    , tableRow = HtmlTree.Node "span" False []
    , tableHeaderCell = always <| HtmlTree.Node "span" False []
    , tableCell = always <| HtmlTree.Node "span" False []
    }
