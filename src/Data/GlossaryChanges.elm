module Data.GlossaryChanges exposing
    ( GlossaryChanges
    , fromList, codec
    , toList
    )

{-| A representation of a sequence of changes to be made to a glossary.


# Glossary Changes

@docs GlossaryChanges


# Build

@docs fromList, codec


# Query

@docs toList

-}

import Codec exposing (Codec)
import Data.GlossaryChange as GlossaryChange exposing (GlossaryChange)


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


{-| An encoder/decoder for a sequence of changes.
-}
codec : Codec GlossaryChanges
codec =
    Codec.map fromList toList (Codec.list GlossaryChange.codec)
