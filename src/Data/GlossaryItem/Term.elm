module Data.GlossaryItem.Term exposing (Term, fromMarkdown, decode, id, isAbbreviation, raw, inlineText, markdown, view, indexGroupString, compareAlphabetically, updateRaw, htmlTreeForAnki)

{-| A term in a glossary item.
A term's `id` is used to be able to refer to this term (as a related one) from other glossary items.
A term might be an abbreviation such as "Etc." or "TBD" (an acronym).
The `body` is the actual term.


# Terms

@docs Term, fromMarkdown, decode, id, isAbbreviation, raw, inlineText, markdown, view, indexGroupString, compareAlphabetically, updateRaw, htmlTreeForAnki

-}

import Data.GlossaryItem.TermId as TermId exposing (TermId)
import Data.MarkdownFragment as MarkdownFragment exposing (MarkdownFragment)
import Extras.HtmlTree exposing (HtmlTree)
import Extras.String
import Html exposing (Attribute, Html, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode exposing (Decoder)
import Markdown.Block exposing (Block)
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
        >> Extras.String.firstAlphaNumericCharacter
        >> Maybe.withDefault "…"
        >> (\result ->
                if List.member result [ "0", "1", "2", "3", "4", "5", "6", "7", "8", "9" ] then
                    "0–9"

                else
                    result
           )


{-| Construct a term from a Markdown string and a Boolean indicating whether the term is an abbreviation.

    fromMarkdown "The _ideal_ case" False |> raw --> "The _ideal_ case"

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


{-| Decode a term from its JSON representation.
-}
decode : Decoder Term
decode =
    Decode.map2
        fromMarkdown
        (Decode.field "body" Decode.string)
        (Decode.field "isAbbreviation" Decode.bool)


{-| Produce the ID of a term.

    import Data.GlossaryItem.TermId as TermId

    fromMarkdown "Hi there" False
    |> id
    |> TermId.toString
    --> "Hi_there"

-}
id : Term -> TermId
id term =
    case term of
        MarkdownTerm t ->
            t.body |> MarkdownFragment.raw |> String.replace " " "_" |> TermId.fromString


{-| Retrieve the "is an abbreviation" Boolean of a term.

    fromMarkdown "NA" True |> isAbbreviation --> True

-}
isAbbreviation : Term -> Bool
isAbbreviation term =
    case term of
        MarkdownTerm t ->
            t.isAbbreviation


{-| Retrieve the raw body of a term.
-}
raw : Term -> String
raw term =
    case term of
        MarkdownTerm t ->
            MarkdownFragment.raw t.body


{-| Retrieve the concatenated inline text of a term.

    fromMarkdown "*Hello* _there_" False
    |> inlineText
    --> "Hello there"

-}
inlineText : Term -> String
inlineText term =
    case term of
        MarkdownTerm t ->
            t.inlineText


{-| Convert a term to a string suitable for a Markdown document.
-}
markdown : Term -> String
markdown term =
    case term of
        MarkdownTerm t ->
            MarkdownFragment.raw t.body


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
view enableMathSupport additionalAttributes term =
    case term of
        MarkdownTerm t ->
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
indexGroupString term =
    case term of
        MarkdownTerm t ->
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
                    |> Extras.String.preserveOnlyAlphaNumChars
                    |> String.toUpper
                )
                (term2
                    |> inlineText
                    |> String.Normalize.removeDiacritics
                    |> Extras.String.preserveOnlyAlphaNumChars
                    |> String.toUpper
                )


{-| Update the raw body of a term.
-}
updateRaw : (String -> String) -> Term -> Term
updateRaw f term =
    case term of
        MarkdownTerm t ->
            fromMarkdown
                (t.body |> MarkdownFragment.raw |> f)
                t.isAbbreviation


{-| Convert a term to an HtmlTree for Anki.
-}
htmlTreeForAnki : Bool -> Term -> HtmlTree
htmlTreeForAnki enableMathSupport term =
    case term of
        MarkdownTerm t ->
            case MarkdownFragment.parsed t.body of
                Ok blocks ->
                    case blocks |> Renderer.render (MarkdownRenderers.inlineHtmlTreeRendererForAnki enableMathSupport) of
                        Ok rendered ->
                            Extras.HtmlTree.Node "span" False [] rendered

                        Err renderingError ->
                            Extras.HtmlTree.Leaf <| "Failed to render Markdown: " ++ renderingError

                Err parsingError ->
                    Extras.HtmlTree.Leaf <| "Failed to parse Markdown: " ++ parsingError
