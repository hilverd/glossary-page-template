module Data.GlossaryItemIdDict exposing
    ( GlossaryItemIdDict
    , empty, insert
    , get
    , keys, toList
    , map, foldl
    )

{-| A dictionary mapping glossary item IDs to values.


# Dictionaries

@docs GlossaryItemIdDict


# Build

@docs empty, insert


# Query

@docs get


# Lists

@docs keys, toList


# Transform

@docs map, foldl

-}

import Data.GlossaryItemId as GlossaryItemId exposing (GlossaryItemId)
import Dict exposing (Dict)


{-| A dictionary of glossary item IDs and values.
-}
type GlossaryItemIdDict a
    = GlossaryItemIdDict (Dict Int a)


{-| Create an empty dictionary.
-}
empty : GlossaryItemIdDict a
empty =
    GlossaryItemIdDict Dict.empty


{-| Insert a key-value pair into a dictionary. Replaces value when there is a collision.
-}
insert : GlossaryItemId -> v -> GlossaryItemIdDict v -> GlossaryItemIdDict v
insert glossaryItemId value (GlossaryItemIdDict dict) =
    dict
        |> Dict.insert (GlossaryItemId.toInt glossaryItemId) value
        |> GlossaryItemIdDict


{-| Get the value associated with a key.
If the key is not found, return `Nothing`.
This is useful when you are not sure if a key will be in the dictionary.
-}
get : GlossaryItemId -> GlossaryItemIdDict v -> Maybe v
get glossaryItemId (GlossaryItemIdDict dict) =
    dict
        |> Dict.get (GlossaryItemId.toInt glossaryItemId)


{-| Get all of the keys in a dictionary, sorted from lowest to highest.
-}
keys : GlossaryItemIdDict v -> List GlossaryItemId
keys (GlossaryItemIdDict dict) =
    dict
        |> Dict.keys
        |> List.map GlossaryItemId.create


{-| Convert a dictionary into an association list of key-value pairs, sorted by keys.
-}
toList : GlossaryItemIdDict v -> List ( GlossaryItemId, v )
toList (GlossaryItemIdDict dict) =
    dict
        |> Dict.toList
        |> List.map
            (\( glossaryItemIdInt, val ) ->
                ( GlossaryItemId.create glossaryItemIdInt, val )
            )


{-| Apply a function to all values in a dictionary.
-}
map : (GlossaryItemId -> a -> b) -> GlossaryItemIdDict a -> GlossaryItemIdDict b
map f (GlossaryItemIdDict dict) =
    dict
        |> Dict.map (GlossaryItemId.create >> f)
        |> GlossaryItemIdDict


{-| Fold over the key-value pairs in a dictionary from lowest key to highest key.
-}
foldl : (GlossaryItemId -> v -> b -> b) -> b -> GlossaryItemIdDict v -> b
foldl func acc (GlossaryItemIdDict dict) =
    Dict.foldl (GlossaryItemId.create >> func) acc dict
