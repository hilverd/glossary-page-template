module DuplicateRejectingDict exposing
    ( DuplicateRejectingDict
    , empty, insert
    , get
    , toResult
    )

{-| A dictionary that rejects attempts to insert duplicates.
Calling `insert` with an existing key changes the dictionary to an "error state".
This state is kept no matter what operations are attempted afterwards.


# Duplicate-Rejecting Dictionaries

@docs DuplicateRejectingDict


# Build

@docs empty, insert


# Query

@docs get


# Handling Errors

@docs toResult

-}

import Dict exposing (Dict)


{-| A duplicate-rejecting dictionary.
-}
type DuplicateRejectingDict comparable v
    = DuplicateRejectingDict
        (Result
            { key : comparable, value1 : v, value2 : v }
            (Dict comparable v)
        )


{-| Create an empty dictionary.
-}
empty : DuplicateRejectingDict comparable v
empty =
    DuplicateRejectingDict <| Ok Dict.empty


{-| Insert a key-value pair into a dictionary. Enters an error state when there is a collision.

    empty
    |> insert 1 "one"
    |> get 1
    --> Just "one"

-}
insert : comparable -> v -> DuplicateRejectingDict comparable v -> DuplicateRejectingDict comparable v
insert key value duplicateRejectingDict =
    case duplicateRejectingDict of
        DuplicateRejectingDict (Ok dict) ->
            case Dict.get key dict of
                Just value1 ->
                    DuplicateRejectingDict <| Err { key = key, value1 = value1, value2 = value }

                Nothing ->
                    DuplicateRejectingDict <| Ok <| Dict.insert key value dict

        DuplicateRejectingDict (Err _) ->
            duplicateRejectingDict


{-| Get the value associated with a key.
If the key is not found or the dictionary is in the error state, return `Nothing`.

    empty
    |> get 1
    --> Nothing

    empty
    |> insert 1 "one"
    |> insert 1 "first"
    |> get 1
    --> Nothing

-}
get : comparable -> DuplicateRejectingDict comparable v -> Maybe v
get key duplicateRejectingDict =
    case duplicateRejectingDict of
        DuplicateRejectingDict (Ok dict) ->
            Dict.get key dict

        DuplicateRejectingDict (Err _) ->
            Nothing


{-| Convert to a `Result`.

    empty
    |> insert 1 "one"
    |> insert 2 "two"
    |> insert 2 "second"
    |> toResult
    --> Err {key = 2, value1 = "two", value2 = "second"}

-}
toResult : DuplicateRejectingDict comparable v -> Result { key : comparable, value1 : v, value2 : v } (Dict comparable v)
toResult duplicateRejectingDict =
    case duplicateRejectingDict of
        DuplicateRejectingDict result ->
            result
