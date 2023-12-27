module Data.GlossaryChange exposing (GlossaryChange(..))

{-| A representation of a change to be made to a glossary.


# Glossary Change

@docs GlossaryChange

-}

import Data.AboutSection exposing (AboutSection)
import Data.CardWidth exposing (CardWidth)
import Data.GlossaryItemForHtml exposing (GlossaryItemForHtml)
import Data.GlossaryItemId exposing (GlossaryItemId)
import Data.GlossaryTitle exposing (GlossaryTitle)
import Data.TagsChanges exposing (TagsChanges)


{-| Represents the different types of changes that can be made to a glossary.
-}
type GlossaryChange
    = ToggleEnableLastUpdatedDates
    | ToggleEnableExportMenu
    | ToggleEnableOrderItemsButtons
    | SetTitle GlossaryTitle
    | SetAboutSection AboutSection
    | SetCardWidth CardWidth
    | ChangeTags TagsChanges
    | Insert GlossaryItemForHtml
    | Update GlossaryItemId GlossaryItemForHtml
    | Remove GlossaryItemId
