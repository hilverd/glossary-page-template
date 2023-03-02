module Data.GlossaryItem.Term exposing (Term, emptyPlaintext, fromPlaintext, fromMarkdown, fromPlaintextWithId, fromMarkdownWithId, decode, id, isAbbreviation, raw)

{-| A term in a glossary item.
This can be in either plain text or Markdown.
A term's `id` is used to be able to refer to this term (as a related one) from other glossary items.
A term might be an abbreviation such as "Etc." or "TBD" (an acronym).
The `body` is the actual term.


# Terms

@docs Term, emptyPlaintext, fromPlaintext, fromMarkdown, fromPlaintextWithId, fromMarkdownWithId, decode, id, isAbbreviation, raw

-}

import Data.MarkdownFragment as MarkdownFragment exposing (MarkdownFragment)
import Json.Decode as Decode exposing (Decoder)
import Markdown.Block as Block exposing (Block)
import Markdown.Html
import Markdown.Renderer as Renderer exposing (Renderer)
import MarkdownRenderers
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
        , body = body |> MarkdownFragment.fromString |> sanitiseMarkdownFragment
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
    PlaintextTerm
        { id = id0
        , isAbbreviation = isAbbreviation0
        , body = body
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


sanitiseMarkdownFragment : MarkdownFragment -> MarkdownFragment
sanitiseMarkdownFragment fragment =
    -- TODO
    MarkdownFragment.transform
        (\block ->
            case block of
                Block.Heading level children ->
                    Block.Heading
                        (case level of
                            Block.H1 ->
                                Block.H4

                            Block.H2 ->
                                Block.H4

                            Block.H3 ->
                                Block.H4

                            _ ->
                                level
                        )
                        children

                _ ->
                    block
        )
        fragment
