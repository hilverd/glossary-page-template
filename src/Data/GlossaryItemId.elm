module Data.GlossaryItemId exposing (GlossaryItemId, create, toInt, codec)

{-| An identifier for a glossary item.
These are used to keep track of items in `GlossaryItems` so that those can be changed safely.

@docs GlossaryItemId, create, toInt, codec

-}

import Codec exposing (Codec)


{-| An opaque type representing the ID of a glossary item.
-}
type GlossaryItemId
    = GlossaryItemId Int


{-| Create a glossary item ID.
-}
create : Int -> GlossaryItemId
create =
    GlossaryItemId


{-| Retrieve the underlying value for a glossary item ID.
-}
toInt : GlossaryItemId -> Int
toInt (GlossaryItemId id) =
    id


{-| An encoder/decoder for glossary item IDs.
-}
codec : Codec GlossaryItemId
codec =
    Codec.map create toInt Codec.int
