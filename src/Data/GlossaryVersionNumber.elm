module Data.GlossaryVersionNumber exposing (GlossaryVersionNumber, create, initial, toInt, codec)

{-| A number representing the version of a glossary.
These numbers are used to coordinate changes to the glossary.
In particular, they are used to ensure that changes sent to a separate backend are applied to the version the frontend applied them to.


# Glossary Version Numbers

@docs GlossaryVersionNumber, create, initial, toInt, codec

-}

import Codec exposing (Codec)


{-| A glossary version number.
-}
type GlossaryVersionNumber
    = GlossaryVersionNumber Int


{-| Construct a glossary version number from an integer.
-}
create : Int -> GlossaryVersionNumber
create =
    GlossaryVersionNumber


{-| The initial glossary version number.
-}
initial : GlossaryVersionNumber
initial =
    create 0


{-| Return a glossary version number as an integer.
-}
toInt : GlossaryVersionNumber -> Int
toInt (GlossaryVersionNumber versionNumber) =
    versionNumber


{-| An encoder/decoder for glossary version numbers.
-}
codec : Codec GlossaryVersionNumber
codec =
    Codec.map create toInt Codec.int
