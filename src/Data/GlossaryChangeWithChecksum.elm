module Data.GlossaryChangeWithChecksum exposing (GlossaryChangeWithChecksum, codec)

{-| A representation of a change to be made to a glossary, together with a checksum for the existing data being changed (to prevent edit conflicts).


# Glossary Change with Checksum

@docs GlossaryChangeWithChecksum, codec

-}

import Codec exposing (Codec)
import Data.Checksum as Checksum exposing (Checksum)
import Data.GlossaryChange as GlossaryChange exposing (GlossaryChange)


{-| A glossary change with a checksum against the existing data being changed.
-}
type alias GlossaryChangeWithChecksum =
    { glossaryChange : GlossaryChange
    , checksum : Checksum
    }


{-| An encoder/decoder for GlossaryChangeWithChecksums.
-}
codec : Codec GlossaryChangeWithChecksum
codec =
    Codec.object (\glossaryChange checksum -> { glossaryChange = glossaryChange, checksum = checksum })
        |> Codec.field "glossaryChange" .glossaryChange GlossaryChange.codec
        |> Codec.field "checksum" .checksum Checksum.codec
        |> Codec.buildObject
