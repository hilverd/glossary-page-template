module Data.GlossaryItemIndex exposing (GlossaryItemIndex, fromInt, toInt)


type GlossaryItemIndex
    = GlossaryItemIndex Int


fromInt : Int -> GlossaryItemIndex
fromInt =
    GlossaryItemIndex


toInt : GlossaryItemIndex -> Int
toInt glossaryItemIndex =
    case glossaryItemIndex of
        GlossaryItemIndex index ->
            index
