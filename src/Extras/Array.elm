module Extras.Array exposing
    ( delete
    , update
    )

{-| This module provides additional functions for common operations on arrays.


# Removing Elements

@docs delete


# Updating Elements

@docs update

-}

import Array exposing (Array)


{-| Delete the element at the given index from the array.
If the index is out of bounds then the original array is returned.

    import Array

    [ "a", "b", "c" ]
    |> Array.fromList
    |> Extras.Array.delete 0
    --> Array.fromList [ "b", "c" ]

    [ "a", "b", "c" ]
    |> Array.fromList
    |> Extras.Array.delete 1
    --> Array.fromList [ "a", "c" ]

    [ "a", "b", "c" ]
    |> Array.fromList
    |> Extras.Array.delete 9
    --> Array.fromList [ "a", "b", "c" ]

-}
delete : Int -> Array a -> Array a
delete index array =
    Array.append
        (Array.slice 0 index array)
        (Array.slice (index + 1) (Array.length array) array)


{-| Update the element at the given index in the array by transforming it using a function.
If the index is out of bounds then the original array is returned.

    import Array

    [ 0, 1, 2 ]
    |> Array.fromList
    |> Extras.Array.update ((+) 1) 2
    --> Array.fromList [ 0, 1, 3 ]

    [ "a", "b" ]
    |> Array.fromList
    |> Extras.Array.update (always "z") 2
    --> Array.fromList [ "a", "b" ]

-}
update : (a -> a) -> Int -> Array a -> Array a
update f index array =
    Array.get index array
        |> Maybe.map (\element -> Array.set index (f element) array)
        |> Maybe.withDefault array
