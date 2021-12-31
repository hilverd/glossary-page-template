module Extras.HtmlTree exposing (Attribute, HtmlTree(..), toHtml)

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
    toIndentedHtml 3 0 True


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
                attributesString =
                    attributes
                        |> List.map (\attribute -> attribute.name ++ "=" ++ "\"" ++ escape attribute.value ++ "\"")
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
            , if format && formatChildren then
                children
                    |> List.map (toIndentedHtml initialLevel (level + 1) True)
                    |> String.join "\n"

              else
                children
                    |> List.map (toIndentedHtml initialLevel (level + 1) False)
                    |> String.join ""
            , prefix ++ "</" ++ name ++ ">"
            ]
                |> String.join
                    (if format then
                        "\n"

                     else
                        ""
                    )
