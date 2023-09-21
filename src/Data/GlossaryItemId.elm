module Data.GlossaryItemId exposing (GlossaryItemId, create, toInt)

{-| An identifier for a glossary item.
These are used to keep track of items in `GlossaryItems` so that those can be changed safely.

@docs GlossaryItemId, create, toInt

-}


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
toInt glossaryItemId =
    case glossaryItemId of
        GlossaryItemId id ->
            id
