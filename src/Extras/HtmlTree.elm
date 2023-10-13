module Extras.HtmlTree exposing
    ( Attribute, attributeToHtmlAttribute, boolAttribute, HtmlTree(..)
    , toHtml, toHtmlReplacementString, escape
    )

{-| An `HtmlTree` represents some HTML content that is to be written to a file.


# Type and Constructors

@docs Attribute, attributeToHtmlAttribute, boolAttribute, HtmlTree


# Converting to a String

@docs toHtml, toHtmlReplacementString, escape

-}

import Html
import Html.Attributes


{-| An `Attribute` is a `name` / `value` pair that represents a normal HTML attribute.
-}
type alias Attribute =
    { name : String
    , value : String
    }


{-| Convert an Attribute to an Html.Attribute.
-}
attributeToHtmlAttribute : Attribute -> Html.Attribute msg
attributeToHtmlAttribute attribute =
    Html.Attributes.attribute attribute.name attribute.value


{-| Create an attribute from a Boolean value.

    boolAttribute "enabled" True
    --> Attribute "enabled" "true"

-}
boolAttribute : String -> Bool -> Attribute
boolAttribute name bool =
    { name = name
    , value =
        if bool then
            "true"

        else
            "false"
    }


{-| An `HtmlTree` is either a `Leaf` node containing a text body without any children, or it is an inner `Node` with

  - a tag name,
  - a Boolean value indicating whether or not its children are to be formatted,
  - zero or more attributes, and
  - zero or more children.

-}
type HtmlTree
    = Leaf String
    | Node String Bool (List Attribute) (List HtmlTree)


{-| Format the given `HtmlTree` as a string.

    toHtml (Leaf "Foo")
    --> "    Foo"

    toHtml (Node "p" False [ Attribute "class" "ml-2" ] [ Leaf "Hello" ])
    --> """    <p class="ml-2">Hello</p>"""

    toHtml (Node "p" True [ Attribute "class" "ml-2" ] [ Leaf "Hello" ])
    --> """    <p class="ml-2">\n        Hello\n    </p>"""

-}
toHtml : HtmlTree -> String
toHtml =
    toIndentedHtml 1 0 True


{-| Format the given `HtmlTree` as a string, escaped to make it suitable as a replacement string in `String.prototype.replace()`.
-}
toHtmlReplacementString : HtmlTree -> String
toHtmlReplacementString =
    toHtml >> String.replace "$" "$$"


{-| Perform HTML escaping on the given string, transforming special characters to HTML entities.

    escape "Q&A" -->
    "Q&amp;A"

    escape "<p>Hello 'world'</p>"
    --> "&lt;p&gt;Hello &#39;world&#39;&lt;/p&gt;"

    escape "\""
    --> "&quot;"

-}
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
        prefix : String
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
                newLineIfFormatChildren : String
                newLineIfFormatChildren =
                    if format && formatChildren then
                        "\n"

                    else
                        ""

                openingTag : String
                openingTag =
                    "<"
                        ++ name
                        ++ (if List.isEmpty attributes then
                                ""

                            else
                                let
                                    attributesString : String
                                    attributesString =
                                        attributes
                                            |> List.map (\attribute -> attribute.name ++ "=" ++ "\"" ++ escape attribute.value ++ "\"")
                                            |> String.join " "
                                in
                                " " ++ attributesString
                           )
                        ++ ">"

                closingTag : String
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
