module Data.GlossaryItemIdDict exposing
    ( GlossaryItemIdDict
    , empty, insert
    , get
    , fromList
    , map
    )

{-| A dictionary mapping glossary item IDs to values.


# Dictionaries

@docs GlossaryItemIdDict


# Build

@docs empty, insert


# Query

@docs get


# Lists

@docs fromList


# Transform

@docs map

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
