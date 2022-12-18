module Data.GlossaryItem.Term exposing (Term, fromPlaintext, id, isAbbreviation, raw)

{-| A term in a glossary item.
This can be in either plain text or Markdown.
A term's `id` is used to be able to refer to this term (as a related one) from other glossary items.
A term might be an abbreviation such as "Etc." or "TBD" (an acronym).
The `body` is the actual term.


# Terms

@docs Term, fromPlaintext, id, isAbbreviation, raw

-}


{-| A term.
-}
type Term
    = PlaintextTerm
        { id : String
        , isAbbreviation : Bool
        , body : String
        }


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


termBodyToId : String -> String
termBodyToId =
    String.replace " " "_"
