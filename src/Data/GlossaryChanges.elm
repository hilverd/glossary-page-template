module Data.GlossaryChanges exposing
    ( GlossaryChanges
    , codec
    , changes, create
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
    = GlossaryChanges
        { changeList : List GlossaryChange
        }


{-| Construct a sequence of changes from a list.
-}
create : List GlossaryChange -> GlossaryChanges
create changes_ =
    GlossaryChanges { changeList = changes_ }


{-| Return the sequence of changes as a list.
-}
changes : GlossaryChanges -> List GlossaryChange
changes (GlossaryChanges glossaryChanges) =
    glossaryChanges.changeList


{-| An encoder/decoder for a sequence of changes.
-}
codec : Codec GlossaryChanges
codec =
    Codec.map create changes (Codec.list GlossaryChange.codec)
