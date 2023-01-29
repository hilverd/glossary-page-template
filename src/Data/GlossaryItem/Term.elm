module Data.GlossaryItem.Term exposing (Term, emptyPlaintext, fromPlaintext, fromPlaintextWithId, decode, id, isAbbreviation, raw)

{-| A term in a glossary item.
This can be in either plain text or Markdown.
A term's `id` is used to be able to refer to this term (as a related one) from other glossary items.
A term might be an abbreviation such as "Etc." or "TBD" (an acronym).
The `body` is the actual term.


# Terms

@docs Term, emptyPlaintext, fromPlaintext, fromPlaintextWithId, decode, id, isAbbreviation, raw

-}

import Json.Decode as Decode exposing (Decoder)


{-| A term.
-}
type Term
    = PlaintextTerm
        { id : String
        , isAbbreviation : Bool
        , body : String
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


{-| Decode a term from its JSON representation.
-}
decode : Decoder Term
decode =
    Decode.map3 fromPlaintextWithId
        (Decode.field "body" Decode.string)
        (Decode.field "id" <| Decode.string)
        (Decode.field "isAbbreviation" Decode.bool)


{-| Retrieve the ID of a term.

    fromPlaintext "Hi there" False |> id --> "Hi_there"

-}
id : Term -> String
id term =
    case term of
        PlaintextTerm t ->
            t.id


{-| Retrieve the "is an abbreviation" Boolean of a term.

    fromPlaintext "NA" True |> isAbbreviation --> True

-}
isAbbreviation : Term -> Bool
isAbbreviation term =
    case term of
        PlaintextTerm t ->
            t.isAbbreviation


{-| Retrieve the raw body of a term.
-}
raw : Term -> String
raw term =
    case term of
        PlaintextTerm t ->
            t.body
