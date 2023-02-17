module MarkdownRenderers exposing (anchorTagRenderer, imgTagRenderer)

import Extras.HtmlAttribute
import Html exposing (Html)
import Html.Attributes as Attr
import Markdown.Html


anchorTagRenderer : Markdown.Html.Renderer (List (Html msg) -> Html msg)
anchorTagRenderer =
    Markdown.Html.tag "img" viewAnchorTag
        |> Markdown.Html.withOptionalAttribute "href"
        |> Markdown.Html.withOptionalAttribute "target"
        |> Markdown.Html.withOptionalAttribute "style"


imgTagRenderer : Markdown.Html.Renderer (List (Html msg) -> Html msg)
imgTagRenderer =
    Markdown.Html.tag "img" viewImgTag
        |> Markdown.Html.withOptionalAttribute "src"
        |> Markdown.Html.withOptionalAttribute "width"
        |> Markdown.Html.withOptionalAttribute "height"
        |> Markdown.Html.withOptionalAttribute "alt"
        |> Markdown.Html.withOptionalAttribute "title"
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
