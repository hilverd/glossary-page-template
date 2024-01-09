module Data.VersionedGlossaryChanges exposing (VersionedGlossaryChanges, create, glossaryChanges, applyToVersionNumber, codec)

{-| A sequence of glossary changes together with a version number.
The version number indicates the version of the glossary that this sequence of changes is to be applied to.


# Versioned Glossaries

@docs VersionedGlossaryChanges, create, glossaryChanges, applyToVersionNumber, codec

-}

import Codec exposing (Codec)
import Data.GlossaryChanges as GlossaryChanges exposing (GlossaryChanges)
import Data.GlossaryVersionNumber as GlossaryVersionNumber exposing (GlossaryVersionNumber)


{-| A versioned sequence of glossary changes.
-}
type VersionedGlossaryChanges
    = VersionedGlossaryChanges
        { applyToVersionNumber : GlossaryVersionNumber
        , glossaryChanges : GlossaryChanges
        }


{-| Create a versioned sequence of glossary changes.
-}
create : GlossaryVersionNumber -> GlossaryChanges -> VersionedGlossaryChanges
create applyToVersionNumber_ glossaryChanges_ =
    VersionedGlossaryChanges
        { applyToVersionNumber = applyToVersionNumber_
        , glossaryChanges = glossaryChanges_
        }


{-| Get the sequence of glossary changes.
-}
glossaryChanges : VersionedGlossaryChanges -> GlossaryChanges
glossaryChanges (VersionedGlossaryChanges versionedGlossaryChanges) =
    versionedGlossaryChanges.glossaryChanges


{-| Get the version number of the glossary that the changes are to be applied to.
-}
applyToVersionNumber : VersionedGlossaryChanges -> GlossaryVersionNumber
applyToVersionNumber (VersionedGlossaryChanges versionedGlossaryChanges) =
    versionedGlossaryChanges.applyToVersionNumber


{-| An encoder/decoder for versioned sequences of glossary changes.
-}
codec : Codec VersionedGlossaryChanges
codec =
    Codec.object create
        |> Codec.field "applyToVersionNumber" applyToVersionNumber GlossaryVersionNumber.codec
        |> Codec.field "glossaryChanges" glossaryChanges GlossaryChanges.codec
        |> Codec.buildObject
