module Data.GlossaryItem.Term exposing (Term, emptyPlaintext, fromPlaintext, fromMarkdown, fromPlaintextWithId, fromMarkdownWithId, decode, id, isAbbreviation, raw, inlineText, markdown, view, indexGroupCharacter, compareAlphabetically, htmlTree)

{-| A term in a glossary item.
This can be in either plain text or Markdown.
A term's `id` is used to be able to refer to this term (as a related one) from other glossary items.
A term might be an abbreviation such as "Etc." or "TBD" (an acronym).
The `body` is the actual term.


# Terms

@docs Term, emptyPlaintext, fromPlaintext, fromMarkdown, fromPlaintextWithId, fromMarkdownWithId, decode, id, isAbbreviation, raw, inlineText, markdown, view, indexGroupCharacter, compareAlphabetically, htmlTree

-}

import Data.FeatureFlag exposing (enableFeaturesInProgress)
import Data.MarkdownFragment as MarkdownFragment exposing (MarkdownFragment)
import Extras.HtmlTree exposing (HtmlTree)
import Extras.String
import Html exposing (Html, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode exposing (Decoder)
import Markdown.Block as Block exposing (Block)
import Markdown.Html
import Markdown.Renderer as Renderer exposing (Renderer)
import MarkdownRenderers
import Regex
import String.Normalize
import Svg.Attributes exposing (from)


{-| A term.
-}
type Term
    = PlaintextTerm
        { id : String
        , isAbbreviation : Bool
        , body : String
        , indexGroupCharacter : String
        }
    | MarkdownTerm
        { id : String
        , isAbbreviation : Bool
        , body : MarkdownFragment
        , indexGroupCharacter : String
        , inlineText : String
        }


{-| Convenience function for constructing an empty plain text term.
-}
emptyPlaintext : Term
emptyPlaintext =
    fromPlaintext "" False


stringToIndexGroupCharacter : String -> String
stringToIndexGroupCharacter =
    String.Normalize.removeDiacritics
        >> String.toUpper
        >> Extras.String.firstAlphabeticCharacter
        >> Maybe.withDefault "…"


{-| Construct a term from a plain text string and a Boolean indicating whether the term is an abbreviation.

    fromPlaintext "NA" True |> raw --> "NA"

-}
fromPlaintext : String -> Bool -> Term
fromPlaintext body isAbbreviation0 =
    PlaintextTerm
        { id = String.replace " " "_" body
        , isAbbreviation = isAbbreviation0
        , body = body
        , indexGroupCharacter = stringToIndexGroupCharacter body
        }


{-| Construct a term from a Markdown string and a Boolean indicating whether the term is an abbreviation.

    fromMarkdown "The _ideal_ case" False |> raw --> "The _ideal_ case"

-}
fromMarkdown : String -> Bool -> Term
fromMarkdown body isAbbreviation0 =
    let
        fragment =
            MarkdownFragment.fromString body

        inlineTextConcatenated =
            fragment
                |> MarkdownFragment.concatenateInlineText
                |> Result.withDefault body
    in
    MarkdownTerm
        { id = String.replace " " "_" body
        , isAbbreviation = isAbbreviation0
        , body = fragment
        , indexGroupCharacter = inlineTextConcatenated |> stringToIndexGroupCharacter
        , inlineText = inlineTextConcatenated
        }


{-| Construct a term from a plain text string, an ID and a Boolean indicating whether the term is an abbreviation.

    fromPlaintextWithId "Hello" "id1" False
    |> id
    --> "id1"

-}
fromPlaintextWithId : String -> String -> Bool -> Term
fromPlaintextWithId body id0 isAbbreviation0 =
    PlaintextTerm
        { id = id0
        , isAbbreviation = isAbbreviation0
        , body = body
        , indexGroupCharacter = stringToIndexGroupCharacter body
        }


{-| Construct a term from a Markdown string, an ID and a Boolean indicating whether the term is an abbreviation.

    fromMarkdownWithId "The _ideal_ case" "id1" False
    |> id
    --> "id1"

-}
fromMarkdownWithId : String -> String -> Bool -> Term
fromMarkdownWithId body id0 isAbbreviation0 =
    let
        result0 =
            fromMarkdown body isAbbreviation0
    in
    case result0 of
        PlaintextTerm _ ->
            result0

        MarkdownTerm t ->
            MarkdownTerm { t | id = id0 }


{-| Decode a term from its JSON representation.
-}
decode : Bool -> Decoder Term
decode enableMarkdownBasedSyntax =
    (if enableFeaturesInProgress && enableMarkdownBasedSyntax then
        Decode.map3 fromMarkdownWithId

     else
        Decode.map3 fromPlaintextWithId
    )
        (Decode.field "body" Decode.string)
        (Decode.field "id" Decode.string)
        (Decode.field "isAbbreviation" Decode.bool)


{-| Retrieve the ID of a term.

    fromPlaintext "Hi there" False |> id --> "Hi_there"

-}
id : Term -> String
id term =
    case term of
        PlaintextTerm t ->
            t.id

        MarkdownTerm t ->
            t.id


{-| Retrieve the "is an abbreviation" Boolean of a term.

    fromPlaintext "NA" True |> isAbbreviation --> True

-}
isAbbreviation : Term -> Bool
isAbbreviation term =
    case term of
        PlaintextTerm t ->
            t.isAbbreviation

        MarkdownTerm t ->
            t.isAbbreviation


{-| Retrieve the raw body of a term.
-}
raw : Term -> String
raw term =
    case term of
        PlaintextTerm t ->
            t.body

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
        PlaintextTerm t ->
            t.body

        MarkdownTerm t ->
            t.inlineText


{-| Convert a term to a string suitable for a Markdown document.
-}
markdown : Term -> String
markdown term =
    case term of
        PlaintextTerm t ->
            Extras.String.escapeForMarkdown t.body

        MarkdownTerm t ->
            MarkdownFragment.raw t.body


{-| View a term as HTML.

    import Html exposing (Html)

    fromPlaintext "Foo" False |> view False --> Html.text "Foo"

    expected : Html msg
    expected =
        Html.span []
            [ Html.span []
                [ Html.text "The "
                , Html.em [] [ Html.text "ideal" ]
                , Html.text " case"
                ]
            ]

    fromMarkdown "The _ideal_ case" False |> view False
    --> expected

-}
view : Bool -> Term -> Html msg
view enableMathSupport term =
    case term of
        PlaintextTerm t ->
            text t.body

        MarkdownTerm t ->
            let
                parsed : Result String (List Block)
                parsed =
                    MarkdownFragment.parsed t.body
            in
            case parsed of
                Ok blocks ->
                    case Renderer.render (MarkdownRenderers.inlineHtmlMsgRenderer { enableMathSupport = enableMathSupport }) blocks of
                        Ok rendered ->
                            Html.span
                                [ class "prose print:prose-neutral dark:prose-invert dark:prose-pre:text-gray-200 prose-code:before:hidden prose-code:after:hidden leading-normal" ]
                                rendered

                        Err renderingError ->
                            text <| "Failed to render Markdown: " ++ renderingError

                Err parsingError ->
                    text <| "Failed to parse Markdown: " ++ parsingError


{-| The _index group character_ of a term is the character it will be listed under in the index.

    fromMarkdown "__future__" False
    |> indexGroupCharacter
    --> "F"

    fromMarkdown "123" False
    |> indexGroupCharacter
    --> "…"

-}
indexGroupCharacter : Term -> String
indexGroupCharacter term =
    case term of
        PlaintextTerm t ->
            t.indexGroupCharacter

        MarkdownTerm t ->
            t.indexGroupCharacter


preserveOnlyAlphaNumChars : String -> String
preserveOnlyAlphaNumChars =
    let
        regex : Regex.Regex
        regex =
            "[^A-Za-z0-9]"
                |> Regex.fromString
                |> Maybe.withDefault Regex.never
    in
    Regex.replace regex (always "")


{-| Compares two terms for listing them alphabetically.
-}
compareAlphabetically : Term -> Term -> Order
compareAlphabetically term1 term2 =
    case compare (indexGroupCharacter term1) (indexGroupCharacter term2) of
        LT ->
            LT

        GT ->
            GT

        EQ ->
            compare
                (term1
                    |> inlineText
                    |> String.Normalize.removeDiacritics
                    |> preserveOnlyAlphaNumChars
                    |> String.toUpper
                )
                (term2
                    |> inlineText
                    |> String.Normalize.removeDiacritics
                    |> preserveOnlyAlphaNumChars
                    |> String.toUpper
                )


{-| Convert a term to an HtmlTree.
-}
htmlTree : Term -> HtmlTree
htmlTree term =
    case term of
        PlaintextTerm t ->
            Extras.HtmlTree.Leaf t.body

        MarkdownTerm t ->
            case MarkdownFragment.parsed t.body of
                Ok blocks ->
                    case Renderer.render MarkdownRenderers.inlineHtmlTreeRenderer blocks of
                        Ok rendered ->
                            Extras.HtmlTree.Node "span" False [] rendered

                        Err renderingError ->
                            Extras.HtmlTree.Leaf <| "Failed to render Markdown: " ++ renderingError

                Err parsingError ->
                    Extras.HtmlTree.Leaf <| "Failed to parse Markdown: " ++ parsingError
