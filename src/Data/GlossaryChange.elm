module Data.GlossaryChange exposing (GlossaryChange(..))

{-| A representation of a change to be made to a glossary.


# Glossary Change

@docs GlossaryChange

-}

import Data.GlossaryItemForHtml exposing (GlossaryItemForHtml)
import Data.GlossaryItemId exposing (GlossaryItemId)
import Data.TagsChanges exposing (TagsChanges)


{-| Represents the different types of changes that can be made to a glossary.
-}
type GlossaryChange
    = ChangeTags TagsChanges
    | Insert GlossaryItemForHtml
    | Update GlossaryItemId GlossaryItemForHtml
    | Remove GlossaryItemId
