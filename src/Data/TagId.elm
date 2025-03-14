module Data.TagId exposing (TagId, create, toString, codec)

{-| An identifier for a tag used in a glossary.
These are used to keep track of tags in `GlossaryItems` so that those can be changed safely.

@docs TagId, create, toString, codec

-}

import Codec exposing (Codec)


{-| An opaque type representing the ID of a tag.
-}
type TagId
    = TagId String


{-| Create a tag ID.
-}
create : String -> TagId
create =
    TagId


{-| Retrieve the underlying value for a tag ID.
-}
toString : TagId -> String
toString (TagId id) =
    id


{-| An encoder/decoder for tag IDs.
-}
codec : Codec TagId
codec =
    Codec.map create toString Codec.string
