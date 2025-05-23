module Data.GlossaryItemOutline exposing (GlossaryItemOutline)

{-| The raw "outline" of a glossary item.
This contains the term(s) being defined and associated tags.


# Glossary Item Outline

@docs GlossaryItemOutline

-}


{-| The outline of a glossary item.
-}
type alias GlossaryItemOutline =
    { disambiguatedPreferredTerm : String
    , preferredTerm : String
    , alternativeTerms : List String
    , allTags : List String
    }
