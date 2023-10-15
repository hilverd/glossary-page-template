module Data.TagIdDict exposing
    ( TagIdDict
    , empty, insert, update
    , get, nextTagId
    , keys, values, toList, fromList
    , map, foldl
    )

{-| A dictionary mapping tag IDs to values.


# Dictionaries

@docs TagIdDict


# Build

@docs empty, insert, update


# Query

@docs get, nextTagId


# Lists

@docs keys, values, toList, fromList


# Transform

@docs map, foldl

-}

import Data.TagId as TagId exposing (TagId)
import Dict exposing (Dict)
import Json.Decode exposing (dict)


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


{-| Update the value of a dictionary for a specific key with a given function.
-}
update : TagId -> (Maybe v -> Maybe v) -> TagIdDict v -> TagIdDict v
update tagId f tagIdDict =
    case tagIdDict of
        TagIdDict dict ->
            dict
                |> Dict.update (TagId.toInt tagId) f
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


{-| Compute the next tag ID to be used for inserting a new item.
-}
nextTagId : TagIdDict v -> TagId
nextTagId tagIdDict =
    case tagIdDict of
        TagIdDict dict ->
            dict
                |> Dict.keys
                |> List.maximum
                |> Maybe.map ((+) 1)
                |> Maybe.withDefault 0
                |> TagId.create


{-| Get all of the keys in a dictionary, sorted from lowest to highest.
-}
keys : TagIdDict v -> List TagId
keys tagIdDict =
    case tagIdDict of
        TagIdDict dict ->
            dict
                |> Dict.keys
                |> List.map TagId.create


{-| Get all of the values in a dictionary, in the order of their keys.
-}
values : TagIdDict v -> List v
values tagIdDict =
    case tagIdDict of
        TagIdDict dict ->
            Dict.values dict


{-| Convert a dictionary into an association list of key-value pairs, sorted by keys.
-}
toList : TagIdDict v -> List ( TagId, v )
toList tagIdDict =
    case tagIdDict of
        TagIdDict dict ->
            dict
                |> Dict.toList
                |> List.map
                    (\( tagIdInt, val ) ->
                        ( TagId.create tagIdInt, val )
                    )


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
