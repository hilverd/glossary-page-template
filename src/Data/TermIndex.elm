module Data.TermIndex exposing (TermIndex, fromInt, toInt)


type TermIndex
    = TermIndex Int


fromInt : Int -> TermIndex
fromInt =
    TermIndex


toInt : TermIndex -> Int
toInt (TermIndex index) =
    index
