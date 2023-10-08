module Data.GlossaryItemIdDict exposing
    ( GlossaryItemIdDict
    , empty, insert, update
    , get
    , keys, values, toList, fromList
    , map, foldl
    )

{-| A dictionary mapping glossary item IDs to values.


# Dictionaries

@docs GlossaryItemIdDict


# Build

@docs empty, insert, update


# Query

@docs get


# Lists

@docs keys, values, toList, fromList


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
insert glossaryItemId value glossaryItemIdDict =
    case glossaryItemIdDict of
        GlossaryItemIdDict dict ->
            dict
                |> Dict.insert (GlossaryItemId.toInt glossaryItemId) value
                |> GlossaryItemIdDict


{-| Update the value of a dictionary for a specific key with a given function.
-}
update : GlossaryItemId -> (Maybe v -> Maybe v) -> GlossaryItemIdDict v -> GlossaryItemIdDict v
update glossaryItemId f glossaryItemIdDict =
    case glossaryItemIdDict of
        GlossaryItemIdDict dict ->
            dict
                |> Dict.update (GlossaryItemId.toInt glossaryItemId) f
                |> GlossaryItemIdDict


{-| Get the value associated with a key.
If the key is not found, return `Nothing`.
This is useful when you are not sure if a key will be in the dictionary.
-}
get : GlossaryItemId -> GlossaryItemIdDict v -> Maybe v
get glossaryItemId glossaryItemIdDict =
    case glossaryItemIdDict of
        GlossaryItemIdDict dict ->
            dict
                |> Dict.get (GlossaryItemId.toInt glossaryItemId)


{-| Get all of the keys in a dictionary, sorted from lowest to highest.
-}
keys : GlossaryItemIdDict v -> List GlossaryItemId
keys glossaryItemIdDict =
    case glossaryItemIdDict of
        GlossaryItemIdDict dict ->
            dict
                |> Dict.keys
                |> List.map GlossaryItemId.create


{-| Get all of the values in a dictionary, in the order of their keys.
-}
values : GlossaryItemIdDict v -> List v
values glossaryItemIdDict =
    case glossaryItemIdDict of
        GlossaryItemIdDict dict ->
            dict
                |> Dict.values


{-| Convert a dictionary into an association list of key-value pairs, sorted by keys.
-}
toList : GlossaryItemIdDict v -> List ( GlossaryItemId, v )
toList glossaryItemIdDict =
    case glossaryItemIdDict of
        GlossaryItemIdDict dict ->
            dict
                |> Dict.toList
                |> List.map
                    (\( glossaryItemIdInt, val ) ->
                        ( GlossaryItemId.create glossaryItemIdInt, val )
                    )


{-| Convert an association list into a dictionary.
-}
fromList : List ( GlossaryItemId, v ) -> GlossaryItemIdDict v
fromList list =
    list
        |> List.map
            (\( glossaryItemId, value ) ->
                ( GlossaryItemId.toInt glossaryItemId, value )
            )
        |> Dict.fromList
        |> GlossaryItemIdDict


{-| Apply a function to all values in a dictionary.
-}
map : (GlossaryItemId -> a -> b) -> GlossaryItemIdDict a -> GlossaryItemIdDict b
map f glossaryItemIdDict =
    case glossaryItemIdDict of
        GlossaryItemIdDict dict ->
            dict
                |> Dict.map (GlossaryItemId.create >> f)
                |> GlossaryItemIdDict


{-| Fold over the key-value pairs in a dictionary from lowest key to highest key.
-}
foldl : (GlossaryItemId -> v -> b -> b) -> b -> GlossaryItemIdDict v -> b
foldl func acc glossaryItemIdDict =
    case glossaryItemIdDict of
        GlossaryItemIdDict dict ->
            Dict.foldl (GlossaryItemId.create >> func) acc dict
