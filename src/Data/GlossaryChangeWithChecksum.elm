module Data.GlossaryChangeWithChecksum exposing (GlossaryChangeWithChecksum, create, glossaryChange, checksum, codec)

{-| A representation of a change to be made to a glossary, together with a checksum for the existing data being changed (to prevent edit conflicts).


# Glossary Change with Checksum

@docs GlossaryChangeWithChecksum, create, glossaryChange, checksum, codec

-}

import Codec exposing (Codec)
import Data.Checksum as Checksum exposing (Checksum)
import Data.GlossaryChange as GlossaryChange exposing (GlossaryChange)
import Data.GlossaryForUi as GlossaryForUi exposing (GlossaryForUi)


{-| A glossary change with a checksum against the existing data being changed.
-}
type GlossaryChangeWithChecksum
    = GlossaryChangeWithChecksum
        { glossaryChange_ : GlossaryChange
        , checksum_ : Checksum
        }


{-| Construct a glossary change.
-}
create : GlossaryChange -> GlossaryForUi -> GlossaryChangeWithChecksum
create glossaryChange_ glossaryForUi =
    GlossaryChangeWithChecksum
        { glossaryChange_ = glossaryChange_
        , checksum_ = GlossaryForUi.checksumForChange glossaryForUi glossaryChange_
        }


{-| Get the glossary change.
-}
glossaryChange : GlossaryChangeWithChecksum -> GlossaryChange
glossaryChange (GlossaryChangeWithChecksum glossaryChangeWithChecksum) =
    glossaryChangeWithChecksum.glossaryChange_


{-| Get the checksum.
-}
checksum : GlossaryChangeWithChecksum -> Checksum
checksum (GlossaryChangeWithChecksum glossaryChangeWithChecksum) =
    glossaryChangeWithChecksum.checksum_


{-| An encoder/decoder for GlossaryChangeWithChecksums.
-}
codec : Codec GlossaryChangeWithChecksum
codec =
    Codec.object (\glossaryChange_ checksum_ -> GlossaryChangeWithChecksum { glossaryChange_ = glossaryChange_, checksum_ = checksum_ })
        |> Codec.field "change" glossaryChange GlossaryChange.codec
        |> Codec.field "checksum" checksum Checksum.codec
        |> Codec.buildObject
