module Data.TagIdDict exposing
    ( TagIdDict
    , empty, insert
    , get
    , fromList
    , map, foldl
    )

{-| A dictionary mapping tag IDs to values.


# Dictionaries

@docs TagIdDict


# Build

@docs empty, insert


# Query

@docs get


# Lists

@docs fromList


# Transform

@docs map, foldl

-}

import Data.TagId as TagId exposing (TagId)
import Dict exposing (Dict)


{-| A dictionary of tag IDs and values.
-}
type TagIdDict a
    = TagIdDict (Dict Int a)


{-| Create an empty dictionary.
-}
empty : TagIdDict a
empty =
    TagIdDict Dict.empty


{-| Insert a key-value pair into a dictionary. Replaces value when there is a collision.
-}
insert : TagId -> v -> TagIdDict v -> TagIdDict v
insert tagId value tagIdDict =
    case tagIdDict of
        TagIdDict dict ->
            dict
                |> Dict.insert (TagId.toInt tagId) value
                |> TagIdDict


{-| Get the value associated with a key.
If the key is not found, return `Nothing`.
This is useful when you are not sure if a key will be in the dictionary.
-}
get : TagId -> TagIdDict v -> Maybe v
get tagId tagIdDict =
    case tagIdDict of
        TagIdDict dict ->
            dict
                |> Dict.get (TagId.toInt tagId)


{-| Convert an association list into a dictionary.
-}
fromList : List ( TagId, v ) -> TagIdDict v
fromList list =
    list
        |> List.map
            (\( tagId, value ) ->
                ( TagId.toInt tagId, value )
            )
        |> Dict.fromList
        |> TagIdDict


{-| Apply a function to all values in a dictionary.
-}
map : (TagId -> a -> b) -> TagIdDict a -> TagIdDict b
map f tagIdDict =
    case tagIdDict of
        TagIdDict dict ->
            dict
                |> Dict.map (TagId.create >> f)
                |> TagIdDict


{-| Fold over the key-value pairs in a dictionary from lowest key to highest key.
-}
foldl : (TagId -> v -> b -> b) -> b -> TagIdDict v -> b
foldl func acc tagIdDict =
    case tagIdDict of
        TagIdDict dict ->
            Dict.foldl (TagId.create >> func) acc dict
