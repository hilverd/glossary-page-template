module Extras.Array exposing (delete, update)

import Array exposing (Array)


delete : Int -> Array a -> Array a
delete index array =
    Array.append
        (Array.slice 0 index array)
        (Array.slice (index + 1) (Array.length array) array)


update : (a -> a) -> Int -> Array a -> Array a
update f index array =
    Array.get index array
        |> Maybe.map (\element -> Array.set index (f element) array)
        |> Maybe.withDefault array
