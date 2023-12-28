module Data.GlossaryChanges exposing
    ( GlossaryChanges
    , fromList
    , toList
    )

{-| A representation of a sequence of changes to be made to a glossary.


# Glossary Changes

@docs GlossaryChanges


# Build

@docs fromList


# Query

@docs toList

-}

import Data.GlossaryChange exposing (GlossaryChange)


{-| Represents a sequence of changes to be made to a glossary.
-}
type GlossaryChanges
    = GlossaryChanges (List GlossaryChange)


{-| Construct a sequence of changes from a list.
-}
fromList : List GlossaryChange -> GlossaryChanges
fromList =
    GlossaryChanges


{-| Return the sequence of changes as a list.
-}
toList : GlossaryChanges -> List GlossaryChange
toList glossaryChanges =
    case glossaryChanges of
        GlossaryChanges changes ->
            changes
