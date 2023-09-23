module Data.TagId exposing (TagId, create, toInt)

{-| An identifier for a tag used in a glossary.
These are used to keep track of tags in `GlossaryItems` so that those can be changed safely.

@docs TagId, create, toInt

-}


{-| An opaque type representing the ID of a tag.
-}
type TagId
    = TagId Int


{-| Create a tag ID.
-}
create : Int -> TagId
create =
    TagId


{-| Retrieve the underlying value for a tag ID.
-}
toInt : TagId -> Int
toInt tagId =
    case tagId of
        TagId id ->
            id
