module Data.TitleHeaderHtml exposing (TitleHeaderHtml, fromString, toVirtualDom)

import Html exposing (Html)
import Html.Parser
import Html.Parser.Util


type TitleHeaderHtml
    = TitleHeaderHtml (List Html.Parser.Node)


fromString : String -> TitleHeaderHtml
fromString =
    Html.Parser.run
        >> Result.withDefault [ Html.Parser.Text "Unable to parse" ]
        >> TitleHeaderHtml


toVirtualDom : TitleHeaderHtml -> List (Html msg)
toVirtualDom titleHeaderHtml =
    case titleHeaderHtml of
        TitleHeaderHtml html ->
            Html.Parser.Util.toVirtualDom html
