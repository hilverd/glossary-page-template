module Data.GlossaryItem.RawTerm exposing (RawTerm, fromString, toString)

{-| The raw (Markdown) body of a term.


# Raw Terms

@docs RawTerm, fromString, toString

-}


{-| A raw term.
-}
type RawTerm
    = RawTerm String


{-| Create a raw term from a string.
-}
fromString : String -> RawTerm
fromString =
    RawTerm


{-| Convert a raw term to a string.
-}
toString : RawTerm -> String
toString (RawTerm rawTerm) =
    rawTerm
