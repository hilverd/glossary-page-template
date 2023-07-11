module Data.DefinitionIndex exposing (DefinitionIndex, fromInt, toInt)


type DefinitionIndex
    = DefinitionIndex Int


fromInt : Int -> DefinitionIndex
fromInt =
    DefinitionIndex


toInt : DefinitionIndex -> Int
toInt definitionIndex =
    case definitionIndex of
        DefinitionIndex index ->
            index
