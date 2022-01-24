module Data.TermIndex exposing (TermIndex, fromInt, toInt)


type TermIndex
    = TermIndex Int


fromInt : Int -> TermIndex
fromInt =
    TermIndex


toInt : TermIndex -> Int
toInt termIndex =
    case termIndex of
        TermIndex index ->
            index
