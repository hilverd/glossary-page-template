module Data.GlossaryChanges exposing
    ( GlossaryChanges
    , create, codec
    , applyToVersionNumber, changes
    )

{-| A representation of a sequence of changes to be made to a glossary.


# Glossary Changes

@docs GlossaryChanges


# Build

@docs create, codec


# Query

@docs applyToVersionNumber, changes

-}

import Codec exposing (Codec)
import Data.GlossaryChange as GlossaryChange exposing (GlossaryChange)
import Data.GlossaryVersionNumber as GlossaryVersionNumber exposing (GlossaryVersionNumber)


{-| Represents a sequence of changes to be made to a glossary.
-}
type GlossaryChanges
    = GlossaryChanges
        { applyToVersionNumber : GlossaryVersionNumber
        , changeList : List GlossaryChange
        }


{-| Construct a sequence of changes from a list.
-}
create : GlossaryVersionNumber -> List GlossaryChange -> GlossaryChanges
create applyToVersionNumber_ changeList_ =
    GlossaryChanges
        { applyToVersionNumber = applyToVersionNumber_
        , changeList = changeList_
        }


{-| Return the version number for the glossary that the changes are to be applied to.
-}
applyToVersionNumber : GlossaryChanges -> GlossaryVersionNumber
applyToVersionNumber (GlossaryChanges glossaryChanges) =
    glossaryChanges.applyToVersionNumber


{-| Return the sequence of changes as a list.
-}
changes : GlossaryChanges -> List GlossaryChange
changes (GlossaryChanges glossaryChanges) =
    glossaryChanges.changeList


{-| An encoder/decoder for a sequence of changes.
-}
codec : Codec GlossaryChanges
codec =
    Codec.object create
        |> Codec.field "applyToVersionNumber" applyToVersionNumber GlossaryVersionNumber.codec
        |> Codec.field "changeList" changes (Codec.list GlossaryChange.codec)
        |> Codec.buildObject
