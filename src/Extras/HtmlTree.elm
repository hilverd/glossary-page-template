module Extras.HtmlTree exposing (Attribute, HtmlTree(..), toHtml)

import Http exposing (Response(..))


type alias Attribute =
    { name : String
    , value : String
    }


type HtmlTree
    = Leaf String
    | Node String (List Attribute) (List HtmlTree)


toHtml : HtmlTree -> String
toHtml =
    toIndentedHtml 3 0


toIndentedHtml : Int -> Int -> HtmlTree -> String
toIndentedHtml initialLevel level tree =
    let
        prefix =
            String.repeat (4 * (initialLevel + level)) " "
    in
    case tree of
        Leaf text ->
            prefix ++ text

        Node name attributes children ->
            let
                attributesString =
                    attributes
                        |> List.map (\attribute -> attribute.name ++ "=" ++ "\"" ++ attribute.value ++ "\"")
                        |> String.join " "
            in
            [ prefix
                ++ "<"
                ++ name
                ++ (if List.isEmpty attributes then
                        ""

                    else
                        " " ++ attributesString
                   )
                ++ ">"
            , children
                |> List.map (toIndentedHtml initialLevel (level + 1))
                |> String.join "\n"
            , prefix ++ "</" ++ name ++ ">"
            ]
                |> String.join "\n"
