module Data.VersionedGlossary exposing (VersionedGlossary, create, glossary, versionNumber, codec)

{-| A glossary together with a version number.


# Versioned Glossaries

@docs VersionedGlossary, create, glossary, versionNumber, codec

-}

import Codec exposing (Codec)
import Data.Glossary as Glossary exposing (Glossary)
import Data.GlossaryVersionNumber as GlossaryVersionNumber exposing (GlossaryVersionNumber)


{-| A versioned glossary.
-}
type VersionedGlossary
    = VersionedGlossary
        { versionNumber : GlossaryVersionNumber
        , glossary : Glossary
        }


{-| Create a versioned glossary.
-}
create : GlossaryVersionNumber -> Glossary -> VersionedGlossary
create versionNumber_ glossary_ =
    VersionedGlossary
        { versionNumber = versionNumber_
        , glossary = glossary_
        }


{-| Get the glossary.
-}
glossary : VersionedGlossary -> Glossary
glossary (VersionedGlossary versionedGlossary) =
    versionedGlossary.glossary


{-| Get the version number.
-}
versionNumber : VersionedGlossary -> GlossaryVersionNumber
versionNumber (VersionedGlossary versionedGlossary) =
    versionedGlossary.versionNumber


{-| An encoder/decoder for versioned glossaries.
-}
codec : Codec VersionedGlossary
codec =
    Codec.object create
        |> Codec.field "versionNumber" versionNumber GlossaryVersionNumber.codec
        |> Codec.field "glossary" glossary Glossary.codec
        |> Codec.buildObject
