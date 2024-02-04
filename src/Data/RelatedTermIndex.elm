module Data.RelatedTermIndex exposing (RelatedTermIndex, fromInt, toInt)


type RelatedTermIndex
    = RelatedTermIndex Int


fromInt : Int -> RelatedTermIndex
fromInt =
    RelatedTermIndex


toInt : RelatedTermIndex -> Int
toInt (RelatedTermIndex index) =
    index
