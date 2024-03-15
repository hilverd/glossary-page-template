module Data.GlossaryItemId exposing (GlossaryItemId, create, toString, codec)

{-| An identifier for a glossary item.
These are used to keep track of items in `GlossaryItems` so that those can be changed safely.

@docs GlossaryItemId, create, toString, codec

-}

import Codec exposing (Codec)


{-| An opaque type representing the ID of a glossary item.
-}
type GlossaryItemId
    = GlossaryItemId String


{-| Create a glossary item ID.
-}
create : String -> GlossaryItemId
create =
    GlossaryItemId


{-| Retrieve the underlying value for a glossary item ID.
-}
toString : GlossaryItemId -> String
toString (GlossaryItemId id) =
    id


{-| An encoder/decoder for glossary item IDs.
-}
codec : Codec GlossaryItemId
codec =
    Codec.map create toString Codec.string
