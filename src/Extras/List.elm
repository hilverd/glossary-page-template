module Extras.List exposing (relocate)

{-| This module provides additional functions for common operations on lists.


# Relocating Elements

@docs relocate

-}


{-| Move element currently at a given source index to just before the current element currently at the destination index.
If the index is out of bounds then the original list is returned.

    [ "red", "orange", "yellow", "green", "blue" ]
    |> Extras.List.relocate 3 1
    --> [ "red", "green", "orange", "yellow", "blue" ]

    [ "red", "orange", "yellow", "green", "blue" ]
    |> Extras.List.relocate 1 3
    --> [ "red", "yellow", "orange", "green", "blue" ]

    [ "red", "orange", "yellow", "green", "blue" ]
    |> Extras.List.relocate 0 0
    --> [ "red", "orange", "yellow", "green", "blue" ]

    [ "red", "orange", "yellow", "green", "blue" ]
    |> Extras.List.relocate 0 1
    --> [ "red", "orange", "yellow", "green", "blue" ]

    [ "red", "orange", "yellow", "green", "blue" ]
    |> Extras.List.relocate 0 5
    --> [ "orange", "yellow", "green", "blue", "red" ]

    [ "red", "orange", "yellow", "green", "blue" ]
    |> Extras.List.relocate 5 0
    --> [ "red", "orange", "yellow", "green", "blue" ]

-}
relocate : Int -> Int -> List a -> List a
relocate sourceIndex destinationIndex list =
    let
        indexed : List ( Int, a )
        indexed =
            List.indexedMap Tuple.pair list

        maybeElement : Maybe ( Int, a )
        maybeElement =
            indexed
                |> List.filter (\( index, _ ) -> index == sourceIndex)
                |> List.head
    in
    case maybeElement of
        Just ( _, element ) ->
            (List.take destinationIndex indexed
                ++ (( -1, element )
                        :: List.drop destinationIndex indexed
                   )
            )
                |> List.filterMap
                    (\( index, element_ ) ->
                        if index == sourceIndex then
                            Nothing

                        else
                            Just element_
                    )

        _ ->
            list
