module Data.GlossaryItem.Term exposing (Term, fromPlaintext)

{-| A term in a glossary item.
-}


type Term
    = PlaintextTerm
        { id : String
        , isAbbreviation : Bool
        , body : String
        }


fromPlaintext : String -> Bool -> Term
fromPlaintext body isAbbreviation =
    PlaintextTerm
        { id = String.replace " " "_" body
        , isAbbreviation = isAbbreviation
        , body = body
        }


termBodyToId : String -> String
termBodyToId =
    String.replace " " "_"
