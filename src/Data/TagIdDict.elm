module Data.TagIdDict exposing
    ( TagIdDict
    , empty, insert, update
    , get
    , values, toList, fromList
    , foldl
    )

{-| A dictionary mapping tag IDs to values.


# Dictionaries

@docs TagIdDict


# Build

@docs empty, insert, update


# Query

@docs get


# Lists

@docs values, toList, fromList


# Transform

@docs foldl

-}

import Data.TagId as TagId exposing (TagId)
import Dict exposing (Dict)


{-| A dictionary of tag IDs and values.
-}
type TagIdDict a
    = TagIdDict (Dict String a)


{-| Create an empty dictionary.
-}
empty : TagIdDict a
empty =
    TagIdDict Dict.empty


{-| Insert a key-value pair into a dictionary. Replaces value when there is a collision.
-}
insert : TagId -> v -> TagIdDict v -> TagIdDict v
insert tagId value (TagIdDict dict) =
    dict
        |> Dict.insert (TagId.toString tagId) value
        |> TagIdDict


{-| Update the value of a dictionary for a specific key with a given function.
-}
update : TagId -> (Maybe v -> Maybe v) -> TagIdDict v -> TagIdDict v
update tagId f (TagIdDict dict) =
    dict
        |> Dict.update (TagId.toString tagId) f
        |> TagIdDict


{-| Get the value associated with a key.
If the key is not found, return `Nothing`.
This is useful when you are not sure if a key will be in the dictionary.
-}
get : TagId -> TagIdDict v -> Maybe v
get tagId (TagIdDict dict) =
    dict
        |> Dict.get (TagId.toString tagId)


{-| Get all of the values in a dictionary, in the order of their keys.
-}
values : TagIdDict v -> List v
values (TagIdDict dict) =
    Dict.values dict


{-| Convert a dictionary into an association list of key-value pairs, sorted by keys.
-}
toList : TagIdDict v -> List ( TagId, v )
toList (TagIdDict dict) =
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
                ( TagId.toString tagId, value )
            )
        |> Dict.fromList
        |> TagIdDict


{-| Fold over the key-value pairs in a dictionary from lowest key to highest key.
-}
foldl : (TagId -> v -> b -> b) -> b -> TagIdDict v -> b
foldl func acc (TagIdDict dict) =
    Dict.foldl (TagId.create >> func) acc dict
