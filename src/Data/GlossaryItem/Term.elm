module Data.GlossaryItem.Term exposing (Term, emptyPlaintext, fromPlaintext, fromMarkdown, fromPlaintextWithId, fromMarkdownWithId, decode, id, isAbbreviation, raw, view, groupCharacter)

{-| A term in a glossary item.
This can be in either plain text or Markdown.
A term's `id` is used to be able to refer to this term (as a related one) from other glossary items.
A term might be an abbreviation such as "Etc." or "TBD" (an acronym).
The `body` is the actual term.


# Terms

@docs Term, emptyPlaintext, fromPlaintext, fromMarkdown, fromPlaintextWithId, fromMarkdownWithId, decode, id, isAbbreviation, raw, view, groupCharacter

-}

import Data.MarkdownFragment as MarkdownFragment exposing (MarkdownFragment)
import Extras.String
import Html exposing (Html, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode exposing (Decoder)
import Markdown.Block as Block exposing (Block)
import Markdown.Html
import Markdown.Renderer as Renderer exposing (Renderer)
import MarkdownRenderers
import String.Normalize
import Svg.Attributes exposing (from)


{-| A term.
-}
type Term
    = PlaintextTerm
        { id : String
        , isAbbreviation : Bool
        , body : String
        }
    | MarkdownTerm
        { id : String
        , isAbbreviation : Bool
        , body : MarkdownFragment
        }


{-| Convenience function for constructing an empty plain text term.
-}
emptyPlaintext : Term
emptyPlaintext =
    fromPlaintext "" False


{-| Construct a term from a plain text string and a Boolean indicating whether the term is an abbreviation.

    fromPlaintext "NA" True |> raw --> "NA"

-}
fromPlaintext : String -> Bool -> Term
fromPlaintext body isAbbreviation0 =
    PlaintextTerm
        { id = String.replace " " "_" body
        , isAbbreviation = isAbbreviation0
        , body = body
        }


{-| Construct a term from a Markdown string and a Boolean indicating whether the term is an abbreviation.

    fromMarkdown "The _ideal_ case" True |> raw --> "The _ideal_ case"

-}
fromMarkdown : String -> Bool -> Term
fromMarkdown body isAbbreviation0 =
    MarkdownTerm
        { id = String.replace " " "_" body
        , isAbbreviation = isAbbreviation0
        , body = MarkdownFragment.fromString body
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
        }


{-| Construct a term from a Markdown string, an ID and a Boolean indicating whether the term is an abbreviation.

    fromMarkdownWithId "The _ideal_ case" "id1" False
    |> id
    --> "id1"

-}
fromMarkdownWithId : String -> String -> Bool -> Term
fromMarkdownWithId body id0 isAbbreviation0 =
    MarkdownTerm
        { id = id0
        , isAbbreviation = isAbbreviation0
        , body = MarkdownFragment.fromString body
        }


{-| Decode a term from its JSON representation.
-}
decode : Bool -> Decoder Term
decode enableMarkdownBasedSyntax =
    (if enableMarkdownBasedSyntax then
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


{-| View a term as HTML.

    import Html exposing (Html)

    fromPlaintext "Foo" False |> view --> Html.text "Foo"

    expected : Html msg
    expected =
        Html.span []
            [ Html.span []
                [ Html.text "The "
                , Html.em [] [ Html.text "ideal" ]
                , Html.text " case"
                ]
            ]

    fromMarkdown "The _ideal_ case" False |> view
    --> expected

-}
view : Term -> Html msg
view term =
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
                    case Renderer.render MarkdownRenderers.inlineHtmlMsgRenderer blocks of
                        Ok rendered ->
                            Html.span
                                [ class "prose print:prose-neutral dark:prose-invert dark:prose-pre:text-gray-200 prose-code:before:hidden prose-code:after:hidden leading-normal" ]
                                rendered

                        Err renderingError ->
                            text <| "Failed to render Markdown: " ++ renderingError

                Err parsingError ->
                    text <| "Failed to parse Markdown: " ++ parsingError


{-| The _group character_ of a term is the character it will be listed under in the index.
-}
groupCharacter : Term -> String
groupCharacter =
    raw
        >> String.Normalize.removeDiacritics
        >> String.toUpper
        >> Extras.String.firstAlphabeticCharacter
        >> Maybe.withDefault "…"
