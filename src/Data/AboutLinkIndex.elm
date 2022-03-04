module Data.AboutLinkIndex exposing (AboutLinkIndex, fromInt, toInt)


type AboutLinkIndex
    = AboutLinkIndex Int


fromInt : Int -> AboutLinkIndex
fromInt =
    AboutLinkIndex


toInt : AboutLinkIndex -> Int
toInt aboutLinkIndex =
    case aboutLinkIndex of
        AboutLinkIndex index ->
            index
