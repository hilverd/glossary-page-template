module Data.GlossaryItemIndex exposing (GlossaryItemIndex, fromInt, toInt, minusOne, plusOne)

{-| An index of a glossary item.


# Glossary Item Indices

@docs GlossaryItemIndex, fromInt, toInt, minusOne, plusOne

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


{-| Subtract one from the index.
-}
minusOne : GlossaryItemIndex -> GlossaryItemIndex
minusOne glossaryItemIndex =
    case glossaryItemIndex of
        GlossaryItemIndex index ->
            GlossaryItemIndex <| index - 1


{-| Add one to the index.
-}
plusOne : GlossaryItemIndex -> GlossaryItemIndex
plusOne glossaryItemIndex =
    case glossaryItemIndex of
        GlossaryItemIndex index ->
            GlossaryItemIndex <| index + 1
