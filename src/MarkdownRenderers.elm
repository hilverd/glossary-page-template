module MarkdownRenderers exposing (anchorTagHtmlTreeRenderer, htmlMsgRenderer, htmlTreeRendererForAnki, imgTagHtmlTreeRenderer, inlineHtmlMsgRenderer, inlineHtmlTreeRendererForAnki, inlineTextRenderer)

{-| Renderers for Markdown data.


# Markdown Renderers

@docs anchorTagHtmlTreeRenderer, htmlMsgRenderer, htmlTreeRendererForAnki, imgTagHtmlTreeRenderer, inlineHtmlMsgRenderer, inlineHtmlTreeRendererForAnki, inlineTextRenderer

-}

import Accessibility.Key
import Extras.HtmlAttribute
import Extras.HtmlTree as HtmlTree exposing (HtmlTree)
import Extras.String
import Html exposing (Html)
import Html.Attributes as Attr exposing (class)
import Markdown.Block as Block
import Markdown.Html
import Markdown.Renderer as Renderer exposing (Renderer)


{-| Render an anchor tag as HTML.
-}
anchorTagRenderer : Bool -> Markdown.Html.Renderer (List (Html msg) -> Html msg)
anchorTagRenderer =
    anchorTagRendererForViewFunction << viewAnchorTag


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
    Bool
    -> Maybe String
    -> Maybe String
    -> Maybe String
    -> List (Html msg)
    -> Html msg
viewAnchorTag tabbable href target style renderedChildren =
    let
        isAbsoluteUrl : Bool
        isAbsoluteUrl =
            href
                |> Maybe.map Extras.String.isRelativeUrl
                |> (==) (Just False)
    in
    Html.a
        [ Accessibility.Key.tabbable tabbable
        , Extras.HtmlAttribute.showMaybe Attr.href href
        , Extras.HtmlAttribute.showMaybe Attr.target target
        , Extras.HtmlAttribute.showIf (isAbsoluteUrl && target == Nothing) <| Attr.target "_blank"
        , Extras.HtmlAttribute.showIf (isAbsoluteUrl && target == Nothing) <| Attr.rel "noopener noreferrer"
        , Extras.HtmlAttribute.showMaybe (Attr.attribute "style") style
        ]
        renderedChildren


{-| Render Markdown to HTML.
-}
htmlMsgRenderer : { enableMathSupport : Bool, makeLinksTabbable : Bool } -> Renderer (Html msg)
htmlMsgRenderer { enableMathSupport, makeLinksTabbable } =
    let
        renderer0 : Renderer (Html msg)
        renderer0 =
            Renderer.defaultHtmlRenderer
    in
    { renderer0
        | paragraph = Html.p [ class "max-w-prose print:max-w-full" ]
        , blockQuote = Html.blockquote [ class "max-w-prose print:max-w-full" ]
        , html =
            Markdown.Html.oneOf
                [ anchorTagRenderer makeLinksTabbable
                , imgTagRenderer
                ]
        , codeSpan =
            \content ->
                if enableMathSupport && String.startsWith "$" content && String.endsWith "$" content then
                    Html.node "katex-inline"
                        [ content
                            |> String.slice 1 -1
                            |> Attr.attribute "data-expr"
                        ]
                        []

                else
                    Html.code [] [ Html.text content ]
        , link =
            \link content ->
                let
                    isAbsoluteUrl =
                        not <| Extras.String.isRelativeUrl link.destination
                in
                case link.title of
                    Just title ->
                        Html.a
                            [ Attr.href link.destination
                            , Attr.title title
                            , Accessibility.Key.tabbable makeLinksTabbable
                            , Extras.HtmlAttribute.showIf isAbsoluteUrl <| Attr.target "_blank"
                            , Extras.HtmlAttribute.showIf isAbsoluteUrl <| Attr.rel "noopener noreferrer"
                            ]
                            content

                    Nothing ->
                        Html.a
                            [ Attr.href link.destination
                            , Accessibility.Key.tabbable makeLinksTabbable
                            , Extras.HtmlAttribute.showIf isAbsoluteUrl <| Attr.target "_blank"
                            , Extras.HtmlAttribute.showIf isAbsoluteUrl <| Attr.rel "noopener noreferrer"
                            ]
                            content
        , codeBlock =
            if enableMathSupport then
                \{ body, language } ->
                    if language == Just "math" then
                        Html.node "katex-display"
                            [ Attr.attribute "data-expr" body ]
                            []

                    else
                        Renderer.defaultHtmlRenderer.codeBlock { body = body, language = language }

            else
                Renderer.defaultHtmlRenderer.codeBlock
    }


{-| A renderer that only handles inline elements (e.g. strong, emphasis) properly.
Block elements are either ignored or have their children wrapped in a <span>.
-}
inlineHtmlMsgRenderer : Bool -> Renderer (Html msg)
inlineHtmlMsgRenderer enableMathSupport =
    { heading = \{ children } -> Html.span [] children
    , paragraph = Html.span []
    , hardLineBreak = Html.text ""
    , blockQuote = Html.span []
    , strong = Html.strong []
    , emphasis = Html.em []
    , strikethrough = Html.del []
    , codeSpan =
        \content ->
            if enableMathSupport && String.startsWith "$" content && String.endsWith "$" content then
                Html.node "katex-inline"
                    [ content
                        |> String.slice 1 -1
                        |> Attr.attribute "data-expr"
                    ]
                    []

            else
                Html.code [] [ Html.text content ]
    , link = always <| Html.span []
    , image = always <| Html.text ""
    , text = Html.text
    , unorderedList = always <| Html.text ""
    , orderedList = \_ _ -> Html.text ""
    , html =
        Markdown.Html.oneOf
            [ Markdown.Html.tag "a" (always <| Html.text "")
            , Markdown.Html.tag "img" (always <| Html.text "")
            ]
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


{-| Render Markdown as an HtmlTree for Anki.
-}
htmlTreeRendererForAnki : Bool -> Renderer HtmlTree
htmlTreeRendererForAnki enableMathSupport =
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
        \content ->
            if enableMathSupport && String.startsWith "$" content && String.endsWith "$" content then
                HtmlTree.Node "anki-mathjax" False [] [ HtmlTree.Leaf <| String.slice 1 -1 content ]

            else
                HtmlTree.Node "code" False [] [ HtmlTree.Leaf content ]
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
            if enableMathSupport then
                HtmlTree.Node "anki-mathjax"
                    False
                    [ HtmlTree.boolAttribute "block" True ]
                    [ HtmlTree.Leaf block.body
                    ]

            else
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


{-| A HtmlTree renderer for Anki that only handles inline elements (e.g. strong, emphasis) properly.
Block elements are either ignored or have their children wrapped in a <span>.
-}
inlineHtmlTreeRendererForAnki : Bool -> Renderer HtmlTree
inlineHtmlTreeRendererForAnki enableMathSupport =
    { heading = \{ children } -> HtmlTree.Node "span" False [] children
    , paragraph = HtmlTree.Node "span" False []
    , hardLineBreak = HtmlTree.Leaf ""
    , blockQuote = HtmlTree.Node "span" False []
    , strong = HtmlTree.Node "strong" False []
    , emphasis = HtmlTree.Node "em" False []
    , strikethrough = HtmlTree.Node "del" False []
    , codeSpan =
        \content ->
            if enableMathSupport && String.startsWith "$" content && String.endsWith "$" content then
                HtmlTree.Node "anki-mathjax" False [] [ HtmlTree.Leaf <| String.slice 1 -1 content ]

            else
                HtmlTree.Node "code" False [] [ HtmlTree.Leaf content ]
    , link = always <| HtmlTree.Node "span" False []
    , image = always <| HtmlTree.Leaf ""
    , text = HtmlTree.Leaf
    , unorderedList = always <| HtmlTree.Leaf ""
    , orderedList = \_ _ -> HtmlTree.Leaf ""
    , html =
        Markdown.Html.oneOf
            [ Markdown.Html.tag "a" (always <| HtmlTree.Leaf "")
            , Markdown.Html.tag "img" (always <| HtmlTree.Leaf "")
            ]
    , codeBlock = always <| HtmlTree.Leaf ""
    , thematicBreak = HtmlTree.Node "span" False [] []
    , table = HtmlTree.Node "span" False []
    , tableHeader = HtmlTree.Node "span" False []
    , tableBody = HtmlTree.Node "span" False []
    , tableRow = HtmlTree.Node "span" False []
    , tableHeaderCell = always <| HtmlTree.Node "span" False []
    , tableCell = always <| HtmlTree.Node "span" False []
    }



-- {-| A text renderer, producing Markdown. Adapted from <https://github.com/matheus23/elm-markdown-transforms>.
-- This is a work in progress.
-- -}
-- textRenderer : Bool -> Renderer String
-- textRenderer enableMathSupport =
--     let
--         escape string =
--             String.replace string ("\\" ++ string)
--     in
--     { heading =
--         \{ level, children } ->
--             (case level of
--                 Block.H1 ->
--                     "# "
--                 Block.H2 ->
--                     "## "
--                 Block.H3 ->
--                     "### "
--                 Block.H4 ->
--                     "#### "
--                 Block.H5 ->
--                     "##### "
--                 Block.H6 ->
--                     "###### "
--             )
--                 ++ String.concat children
--     , paragraph = String.concat
--     , hardLineBreak = "\n\n"
--     , blockQuote =
--         \children ->
--             children
--                 |> String.concat
--                 |> String.split "\n"
--                 |> List.map (\line -> "> " ++ line)
--                 |> String.join "\n"
--     , strong =
--         \children ->
--             "**" ++ String.replace "**" "\\**" (String.concat children) ++ "**"
--     , emphasis =
--         \children ->
--             "_" ++ escape "_" (String.concat children) ++ "_"
--     , strikethrough =
--         \children ->
--             "~" ++ escape "~" (String.concat children) ++ "~"
--     , codeSpan =
--         \content ->
--             if enableMathSupport && String.startsWith "$" content && String.endsWith "$" content then
--                 "$" ++ String.slice 1 -1 content ++ "$"
--             else
--                 "`" ++ content ++ "`"
--     , link =
--         \{ destination } children ->
--             -- TODO: handle titles, see https://github.github.com/gfm/#example-494
--             "[" ++ escape "]" (escape ")" (String.concat children)) ++ "](" ++ destination ++ ")"
--     , image =
--         \{ alt, src, title } ->
--             "!["
--                 ++ escape "]" (escape ")" alt)
--                 ++ "]("
--                 ++ src
--                 ++ (title
--                         |> Maybe.map (\t -> " \"" ++ escape "\"" t ++ "\"")
--                         |> Maybe.withDefault ""
--                    )
--                 ++ ")"
--     , text = identity
--     , unorderedList =
--         \items ->
--             items
--                 |> List.map
--                     (\(Block.ListItem task children) ->
--                         case task of
--                             Block.NoTask ->
--                                 "- " ++ String.concat children
--                             Block.IncompleteTask ->
--                                 "- [ ] " ++ String.concat children
--                             Block.CompletedTask ->
--                                 "- [X] " ++ String.concat children
--                     )
--                 |> String.join "\n"
--     , orderedList =
--         \startingIndex items ->
--             items
--                 |> List.indexedMap
--                     (\index children ->
--                         String.fromInt (index + startingIndex)
--                             ++ ". "
--                             ++ String.concat children
--                     )
--                 |> String.join "\n"
--     , html = Markdown.Html.oneOf []
--     , codeBlock =
--         \{ body, language } ->
--             case language of
--                 Just langName ->
--                     "```"
--                         ++ langName
--                         ++ "\n"
--                         ++ body
--                         ++ "```"
--                 Nothing ->
--                     let
--                         bodyLines =
--                             body
--                                 |> String.split "\n"
--                     in
--                     if bodyLines |> List.any (not << String.startsWith " ") then
--                         bodyLines
--                             |> List.map ((++) "    ")
--                             |> String.join "\n"
--                     else
--                         "```\n" ++ body ++ "```"
--     , thematicBreak = "---\n"
--     , table = always "" -- TODO
--     , tableHeader = always "" -- TODO
--     , tableBody = always "" -- TODO
--     , tableRow = always "" -- TODO
--     , tableHeaderCell = \_ _ -> "" -- TODO
--     , tableCell = \_ _ -> "" -- TODO
--     }


{-| A text (i.e. Markdown) renderer that only handles inline elements (e.g. strong, emphasis) properly.
Block elements are either ignored or have their children wrapped in a <span>.
Adapted from <https://github.com/matheus23/elm-markdown-transforms>.
-}
inlineTextRenderer : Bool -> Renderer String
inlineTextRenderer enableMathSupport =
    let
        escape string =
            String.replace string ("\\" ++ string)
    in
    { heading = \_ -> ""
    , paragraph = String.concat
    , hardLineBreak = ""
    , blockQuote = always ""
    , strong =
        \children ->
            "**" ++ String.replace "**" "\\**" (String.concat children) ++ "**"
    , emphasis =
        \children ->
            "_" ++ escape "_" (String.concat children) ++ "_"
    , strikethrough =
        \children ->
            "~" ++ escape "~" (String.concat children) ++ "~"
    , codeSpan =
        \content ->
            if enableMathSupport && String.startsWith "$" content && String.endsWith "$" content then
                "$" ++ String.slice 1 -1 content ++ "$"

            else
                "`" ++ content ++ "`"
    , link =
        \{ destination } children ->
            -- TODO: handle titles, see https://github.github.com/gfm/#example-494
            "[" ++ escape "]" (escape ")" (String.concat children)) ++ "](" ++ destination ++ ")"
    , image = always ""
    , text = identity
    , unorderedList = always ""
    , orderedList = \_ _ -> ""
    , html = Markdown.Html.oneOf []
    , codeBlock = always ""
    , thematicBreak = ""
    , table = always ""
    , tableHeader = always ""
    , tableBody = always ""
    , tableRow = always ""
    , tableHeaderCell = \_ _ -> ""
    , tableCell = \_ _ -> ""
    }
