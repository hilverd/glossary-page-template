module Data.GlossaryItem.DisambiguatedTerm exposing (DisambiguatedTerm, fromTerm, toTerm, compareAlphabetically, codec)

{-| A disambiguated term, i.e. the preferred term for an item with its disambiguation tag (if any) appended.


# Disambiguated Terms

@docs DisambiguatedTerm, fromTerm, toTerm, compareAlphabetically, codec

-}

import Codec exposing (Codec)
import Data.GlossaryItem.Term as Term exposing (Term)


{-| A disambiguated term.
-}
type DisambiguatedTerm
    = DisambiguatedTerm Term


{-| Create a disambiguated term by wrapping a term.
-}
fromTerm : Term -> DisambiguatedTerm
fromTerm =
    DisambiguatedTerm


{-| Retrieve the underlying term (including any disambiguation tag) for a disambiguated term.
-}
toTerm : DisambiguatedTerm -> Term
toTerm (DisambiguatedTerm term) =
    term


{-| Compares two terms for ordering them alphabetically.
-}
compareAlphabetically : DisambiguatedTerm -> DisambiguatedTerm -> Order
compareAlphabetically (DisambiguatedTerm term1) (DisambiguatedTerm term2) =
    Term.compareAlphabetically term1 term2


{-| Encode/decode a term from its JSON representation.
-}
codec : Codec DisambiguatedTerm
codec =
    Codec.map fromTerm toTerm Term.codec
