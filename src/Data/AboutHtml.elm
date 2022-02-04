module Data.AboutHtml exposing (AboutHtml, fromString, toVirtualDom)

import Html exposing (Html)
import Html.Parser
import Html.Parser.Util


type AboutHtml
    = AboutHtml (List Html.Parser.Node)


fromString : String -> AboutHtml
fromString =
    Html.Parser.run
        >> Result.withDefault [ Html.Parser.Text "Unable to parse" ]
        >> AboutHtml


toVirtualDom : AboutHtml -> List (Html msg)
toVirtualDom aboutHtml =
    case aboutHtml of
        AboutHtml html ->
            Html.Parser.Util.toVirtualDom html
