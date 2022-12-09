module Data.GlossaryItemIndex exposing (GlossaryItemIndex, fromInt, toInt)

{-| An index of a glossary item.


# Glossary Item Indices

@docs GlossaryItemIndex, fromInt, toInt

-}


{-| A glossary item index is really just a wrapper around an `Int`.
-}
type GlossaryItemIndex
    = GlossaryItemIndex Int


{-| Construct an index from an `Int`.
-}
fromInt : Int -> GlossaryItemIndex
fromInt =
    GlossaryItemIndex


{-| Get the `Int` value for the index.
-}
toInt : GlossaryItemIndex -> Int
toInt glossaryItemIndex =
    case glossaryItemIndex of
        GlossaryItemIndex index ->
            index
