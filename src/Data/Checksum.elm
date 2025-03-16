module Data.Checksum exposing (Checksum, create, toString, codec)

{-| Checksums for preventing edit conflicts.


# Checksums

@docs Checksum, create, toString, codec

-}

import Codec exposing (Codec)
import Data.GlossaryChange exposing (GlossaryChange(..))


{-| A checksum.
-}
type Checksum
    = Checksum String


{-| Create a checksum.
-}
create : String -> Checksum
create =
    Checksum


{-| Retrieve the underlying value for a checksum.
-}
toString : Checksum -> String
toString (Checksum checksum_) =
    checksum_


{-| An encoder/decoder for checksums.
-}
codec : Codec Checksum
codec =
    Codec.map create toString Codec.string
