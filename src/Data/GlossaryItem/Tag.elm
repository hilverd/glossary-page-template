module Data.GlossaryItem.Tag exposing (Tag, emptyPlaintext, fromPlaintext, fromMarkdown, decode, raw, inlineText, markdown, id, view)

{-| A tag that can be used in glossary items.
This can be in either plain text or Markdown.
The `body` is the actual tag.

Tags can be used to group items together that belong in the same "context" or share some important property. They should probably be used sparingly as too many tags could be an indication of a glossary trying to focus on too many different topics at once.


# Tags

@docs Tag, emptyPlaintext, fromPlaintext, fromMarkdown, decode, raw, inlineText, markdown, id, view

-}

import Data.MarkdownFragment as MarkdownFragment exposing (MarkdownFragment)
import Extras.String
import Html exposing (Attribute, Html, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode exposing (Decoder)
import Markdown.Block exposing (Block)
import Markdown.Renderer as Renderer
import MarkdownRenderers


{-| A tag.
-}
type Tag
    = PlaintextTag
        { body : String
        }
    | MarkdownTag
        { body : MarkdownFragment
        , inlineText : String
        }


{-| Convenience function for constructing an empty plain text tag.
-}
emptyPlaintext : Tag
emptyPlaintext =
    fromPlaintext ""


{-| Construct a tag from a plain text string and a Boolean indicating whether the tag is an abbreviation.

    fromPlaintext "NA" |> raw --> "NA"

-}
fromPlaintext : String -> Tag
fromPlaintext body =
    PlaintextTag
        { body = body
        }


{-| Construct a tag from a Markdown string and a Boolean indicating whether the tag is an abbreviation.

    fromMarkdown "The _ideal_ case" |> raw --> "The _ideal_ case"

-}
fromMarkdown : String -> Tag
fromMarkdown body =
    let
        fragment : MarkdownFragment
        fragment =
            MarkdownFragment.fromString body

        inlineTextConcatenated : String
        inlineTextConcatenated =
            fragment
                |> MarkdownFragment.concatenateInlineText
                |> Result.withDefault body
    in
    MarkdownTag
        { body = fragment
        , inlineText = inlineTextConcatenated
        }


{-| Decode a tag from its JSON representation.
-}
decode : Bool -> Decoder Tag
decode enableMarkdownBasedSyntax =
    Decode.map
        (if enableMarkdownBasedSyntax then
            fromMarkdown

         else
            fromPlaintext
        )
        Decode.string


{-| Retrieve the raw body of a tag.
-}
raw : Tag -> String
raw tag =
    case tag of
        PlaintextTag t ->
            t.body

        MarkdownTag t ->
            MarkdownFragment.raw t.body


{-| Retrieve the concatenated inline text of a tag.

    fromMarkdown "*Hello* _there_"
    |> inlineText
    --> "Hello there"

-}
inlineText : Tag -> String
inlineText tag =
    case tag of
        PlaintextTag t ->
            t.body

        MarkdownTag t ->
            t.inlineText


{-| Convert a tag to a string suitable for a Markdown document.
-}
markdown : Tag -> String
markdown tag =
    case tag of
        PlaintextTag t ->
            Extras.String.escapeForMarkdown t.body

        MarkdownTag t ->
            MarkdownFragment.raw t.body


{-| Return a string for the tag that can be used as an HTML ID attribute.

    fromPlaintext "Foo bar" |> id
    --> "Foo_bar"

-}
id : Tag -> String
id =
    raw >> String.replace " " "_"


{-| View a tag as HTML.

    import Html exposing (Html)

    fromPlaintext "Foo" |> view False [] --> Html.span [] [ Html.text "Foo" ]

    expected : Html msg
    expected =
        Html.span []
            [ Html.span []
                [ Html.text "The "
                , Html.em [] [ Html.text "ideal" ]
                , Html.text " case"
                ]
            ]

    fromMarkdown "The _ideal_ case" |> view False []
    --> expected

-}
view : Bool -> List (Attribute msg) -> Tag -> Html msg
view enableMathSupport additionalAttributes tag =
    case tag of
        PlaintextTag t ->
            Html.span additionalAttributes [ text t.body ]

        MarkdownTag t ->
            let
                parsed : Result String (List Block)
                parsed =
                    MarkdownFragment.parsed t.body
            in
            case parsed of
                Ok blocks ->
                    case Renderer.render (MarkdownRenderers.inlineHtmlMsgRenderer enableMathSupport) blocks of
                        Ok rendered ->
                            Html.span
                                (class "prose dark:prose-invert print:prose-neutral dark:prose-pre:text-gray-200 prose-code:before:hidden prose-code:after:hidden leading-normal" :: additionalAttributes)
                                rendered

                        Err renderingError ->
                            text <| "Failed to render Markdown: " ++ renderingError

                Err parsingError ->
                    text <| "Failed to parse Markdown: " ++ parsingError
