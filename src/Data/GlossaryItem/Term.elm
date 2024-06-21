module Data.GlossaryItem.Term exposing (Term, fromMarkdown, codec, id, isAbbreviation, raw, inlineText, markdown, view, indexGroupString, compareAlphabetically, updateRaw, htmlTreeForAnki, toTermFromDom)

{-| A term in a glossary item.
A term's `id` is used to be able to refer to this term (as a related one) from other glossary items.
A term might be an abbreviation such as "Etc." or "TBD" (an acronym).
The `body` is the actual term.


# Terms

@docs Term, fromMarkdown, codec, id, isAbbreviation, raw, inlineText, markdown, view, indexGroupString, compareAlphabetically, updateRaw, htmlTreeForAnki, toTermFromDom

-}

import Codec exposing (Codec)
import Data.GlossaryItem.RawTerm as RawTerm exposing (RawTerm)
import Data.GlossaryItem.TermFromDom exposing (TermFromDom)
import Data.MarkdownFragment as MarkdownFragment exposing (MarkdownFragment)
import Extras.HtmlTree exposing (HtmlTree)
import Extras.String
import Html exposing (Attribute, Html, text)
import Html.Attributes exposing (class)
import Internationalisation as I18n
import Markdown.Renderer as Renderer
import MarkdownRenderers
import String.Normalize


{-| A term.
-}
type Term
    = MarkdownTerm
        { isAbbreviation : Bool
        , body : MarkdownFragment
        , indexGroupString : String
        , inlineText : String
        }


stringToIndexGroupString : String -> String
stringToIndexGroupString =
    String.Normalize.removeDiacritics
        >> String.toUpper
        >> Extras.String.firstAlphanumericCharacter
        >> Maybe.withDefault "…"
        >> (\result ->
                if List.member result [ "0", "1", "2", "3", "4", "5", "6", "7", "8", "9" ] then
                    "0–9"

                else
                    result
           )


{-| Construct a term from a Markdown string and a Boolean indicating whether the term is an abbreviation.

    import Data.GlossaryItem.RawTerm as RawTerm

    fromMarkdown "The _ideal_ case" False
    |> raw
    |> RawTerm.toString
    --> "The _ideal_ case"

-}
fromMarkdown : String -> Bool -> Term
fromMarkdown body isAbbreviation0 =
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
    MarkdownTerm
        { isAbbreviation = isAbbreviation0
        , body = fragment
        , indexGroupString = inlineTextConcatenated |> stringToIndexGroupString
        , inlineText = inlineTextConcatenated
        }


{-| Encode/decode a term from its JSON representation.
-}
codec : Codec Term
codec =
    Codec.object
        fromMarkdown
        |> Codec.field "body" (raw >> RawTerm.toString) Codec.string
        |> Codec.field "isAbbreviation" isAbbreviation Codec.bool
        |> Codec.buildObject


{-| Produce the ID of a term.

    fromMarkdown "Hi there" False
    |> id
    --> "Hi_there"

-}
id : Term -> String
id (MarkdownTerm { body }) =
    body |> MarkdownFragment.raw |> String.replace " " "_"


{-| Retrieve the "is an abbreviation" Boolean of a term.

    fromMarkdown "NA" True |> isAbbreviation --> True

-}
isAbbreviation : Term -> Bool
isAbbreviation (MarkdownTerm t) =
    t.isAbbreviation


{-| Retrieve the raw body of a term.
-}
raw : Term -> RawTerm
raw (MarkdownTerm { body }) =
    body |> MarkdownFragment.raw |> RawTerm.fromString


{-| Retrieve the concatenated inline text of a term.

    fromMarkdown "*Hello* _there_" False
    |> inlineText
    --> "Hello there"

-}
inlineText : Term -> String
inlineText (MarkdownTerm t) =
    t.inlineText


{-| Convert a term to a string suitable for a Markdown document.
-}
markdown : Term -> String
markdown (MarkdownTerm { body }) =
    MarkdownFragment.raw body


{-| View a term as HTML.

    import Html exposing (Html)

    expected : Html msg
    expected =
        Html.span []
            [ Html.span []
                [ Html.text "The "
                , Html.em [] [ Html.text "ideal" ]
                , Html.text " case"
                ]
            ]

    fromMarkdown "The _ideal_ case" False |> view False []
    --> expected

-}
view : Bool -> List (Attribute msg) -> Term -> Html msg
view enableMathSupport additionalAttributes (MarkdownTerm { body }) =
    case MarkdownFragment.parsed body of
        Ok blocks ->
            case Renderer.render (MarkdownRenderers.inlineHtmlMsgRenderer enableMathSupport) blocks of
                Ok rendered ->
                    Html.span
                        (class "prose dark:prose-invert print:prose-neutral dark:prose-pre:text-gray-200 prose-code:before:hidden prose-code:after:hidden leading-normal" :: additionalAttributes)
                        rendered

                Err renderingError ->
                    text <| I18n.failedToRenderMarkdown ++ ": " ++ renderingError

        Err parsingError ->
            text <| I18n.failedToParseMarkdown ++ ": " ++ parsingError


{-| The _index group character_ of a term is the character it will be listed under in the index.

    fromMarkdown "__future__" False
    |> indexGroupString
    --> "F"

    fromMarkdown "123" False
    |> indexGroupString
    --> "0–9"

    fromMarkdown "Ω" False
    |> indexGroupString
    --> "…"

-}
indexGroupString : Term -> String
indexGroupString (MarkdownTerm t) =
    t.indexGroupString


{-| Compares two terms for ordering them alphabetically.
-}
compareAlphabetically : Term -> Term -> Order
compareAlphabetically term1 term2 =
    case compare (indexGroupString term1) (indexGroupString term2) of
        LT ->
            LT

        GT ->
            GT

        EQ ->
            compare
                (term1
                    |> inlineText
                    |> String.Normalize.removeDiacritics
                    |> Extras.String.preserveOnlyAlphanumChars
                    |> String.toUpper
                )
                (term2
                    |> inlineText
                    |> String.Normalize.removeDiacritics
                    |> Extras.String.preserveOnlyAlphanumChars
                    |> String.toUpper
                )


{-| Update the raw body of a term.
-}
updateRaw : (String -> String) -> Term -> Term
updateRaw f (MarkdownTerm t) =
    fromMarkdown
        (t.body |> MarkdownFragment.raw |> f)
        t.isAbbreviation


{-| Convert a term to an HtmlTree for Anki.
-}
htmlTreeForAnki : Bool -> Term -> HtmlTree
htmlTreeForAnki enableMathSupport (MarkdownTerm { body }) =
    case MarkdownFragment.parsed body of
        Ok blocks ->
            case blocks |> Renderer.render (MarkdownRenderers.inlineHtmlTreeRendererForAnki enableMathSupport) of
                Ok rendered ->
                    Extras.HtmlTree.Node "span" False [] rendered

                Err renderingError ->
                    Extras.HtmlTree.Leaf <| I18n.failedToRenderMarkdown ++ ": " ++ renderingError

        Err parsingError ->
            Extras.HtmlTree.Leaf <| I18n.failedToParseMarkdown ++ ": " ++ parsingError


{-| Convert a term to a TermFromDom.
-}
toTermFromDom : Term -> TermFromDom
toTermFromDom (MarkdownTerm term) =
    { isAbbreviation = term.isAbbreviation
    , body = MarkdownFragment.raw term.body
    }
