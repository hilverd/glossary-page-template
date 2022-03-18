module Extras.HtmlTree exposing (Attribute, HtmlTree(..), escape, toHtml)

import Http exposing (Response(..))


type alias Attribute =
    { name : String
    , value : String
    }


type HtmlTree
    = Leaf String
    | Node String Bool (List Attribute) (List HtmlTree)


toHtml : HtmlTree -> String
toHtml =
    toIndentedHtml 1 0 True


escape : String -> String
escape string =
    string
        |> String.replace "&" "&amp;"
        |> String.replace "<" "&lt;"
        |> String.replace ">" "&gt;"
        |> String.replace "\"" "&quot;"
        |> String.replace "'" "&#39;"


toIndentedHtml : Int -> Int -> Bool -> HtmlTree -> String
toIndentedHtml initialLevel level format tree =
    let
        prefix =
            if format then
                String.repeat (4 * (initialLevel + level)) " "

            else
                ""
    in
    case tree of
        Leaf text ->
            prefix ++ escape text

        Node name formatChildren attributes children ->
            let
                newLineIfFormatChildren =
                    if format && formatChildren then
                        "\n"

                    else
                        ""

                attributesString =
                    attributes
                        |> List.map (\attribute -> attribute.name ++ "=" ++ "\"" ++ escape attribute.value ++ "\"")
                        |> String.join " "

                openingTag =
                    "<"
                        ++ name
                        ++ (if List.isEmpty attributes then
                                ""

                            else
                                " " ++ attributesString
                           )
                        ++ ">"

                closingTag =
                    "</" ++ name ++ ">"
            in
            (prefix ++ openingTag ++ newLineIfFormatChildren)
                ++ (children
                        |> List.map (toIndentedHtml initialLevel (level + 1) (format && formatChildren))
                        |> String.join newLineIfFormatChildren
                   )
                ++ (newLineIfFormatChildren
                        ++ (if formatChildren then
                                prefix

                            else
                                ""
                           )
                        ++ closingTag
                   )
