module Data.DetailsIndex exposing (DetailsIndex, fromInt, toInt)


type DetailsIndex
    = DetailsIndex Int


fromInt : Int -> DetailsIndex
fromInt =
    DetailsIndex


toInt : DetailsIndex -> Int
toInt detailsIndex =
    case detailsIndex of
        DetailsIndex index ->
            index
