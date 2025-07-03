module Data.GlossaryChange exposing (GlossaryChange(..), codec, setLastUpdatedBy)

{-| A representation of a change to be made to a glossary.


# Glossary Change

@docs GlossaryChange, codec, setLastUpdatedBy

-}

import Codec exposing (Codec)
import Data.AboutSection as AboutSection exposing (AboutSection)
import Data.CardWidth as CardWidth exposing (CardWidth)
import Data.GlossaryItem.TermFromDom as TermFromDom exposing (TermFromDom)
import Data.GlossaryItemFromDom as GlossaryItemFromDom exposing (GlossaryItemFromDom)
import Data.GlossaryItemId as GlossaryItemId exposing (GlossaryItemId)
import Data.GlossaryTitle as GlossaryTitle exposing (GlossaryTitle)
import Data.TagsChanges as TagsChanges exposing (TagsChanges)
import Data.Theme as Theme exposing (Theme)


{-| Represents the different types of changes that can be made to a glossary.
-}
type GlossaryChange
    = ToggleEnableLastUpdatedDates
    | ToggleEnableExportMenu
    | ToggleEnableOrderItemsButtons
    | SetStartingItem TermFromDom
    | ClearStartingItem
    | SetTitle GlossaryTitle
    | SetAboutSection AboutSection
    | SetCardWidth CardWidth
    | SetDefaultTheme Theme
    | ChangeTags TagsChanges
    | Insert GlossaryItemFromDom
    | Update GlossaryItemFromDom
    | Remove GlossaryItemId


{-| An encoder/decoder for glossary changes.
-}
codec : Codec GlossaryChange
codec =
    Codec.custom
        (\toggleEnableLastUpdatedDates toggleEnableExportMenu toggleEnableOrderItemsButtons setStartingItem clearStartingItem setTitle setAboutSection setCardWidth setDefaultTheme changeTags insert update remove value ->
            case value of
                ToggleEnableLastUpdatedDates ->
                    toggleEnableLastUpdatedDates

                ToggleEnableExportMenu ->
                    toggleEnableExportMenu

                ToggleEnableOrderItemsButtons ->
                    toggleEnableOrderItemsButtons

                SetStartingItem itemId ->
                    setStartingItem itemId

                ClearStartingItem ->
                    clearStartingItem

                SetTitle title ->
                    setTitle title

                SetAboutSection aboutSection ->
                    setAboutSection aboutSection

                SetCardWidth cardWidth ->
                    setCardWidth cardWidth

                SetDefaultTheme theme ->
                    setDefaultTheme theme

                ChangeTags tagsChanges ->
                    changeTags tagsChanges

                Insert itemForUi ->
                    insert itemForUi

                Update itemForUi ->
                    update itemForUi

                Remove itemId ->
                    remove itemId
        )
        |> Codec.variant0 "ToggleEnableLastUpdatedDates" ToggleEnableLastUpdatedDates
        |> Codec.variant0 "ToggleEnableExportMenu" ToggleEnableExportMenu
        |> Codec.variant0 "ToggleEnableOrderItemsButtons" ToggleEnableOrderItemsButtons
        |> Codec.variant1 "SetStartingItem" SetStartingItem TermFromDom.codec
        |> Codec.variant0 "ClearStartingItem" ClearStartingItem
        |> Codec.variant1 "SetTitle" SetTitle GlossaryTitle.codec
        |> Codec.variant1 "SetAboutSection" SetAboutSection AboutSection.codec
        |> Codec.variant1 "SetCardWidth" SetCardWidth CardWidth.codec
        |> Codec.variant1 "SetDefaultTheme" SetDefaultTheme Theme.codec
        |> Codec.variant1 "ChangeTags" ChangeTags TagsChanges.codec
        |> Codec.variant1 "Insert" Insert GlossaryItemFromDom.codec
        |> Codec.variant1 "Update" Update GlossaryItemFromDom.codec
        |> Codec.variant1 "Remove" Remove GlossaryItemId.codec
        |> Codec.buildCustom


{-| Set the name and email address of the person making this glossary change.
-}
setLastUpdatedBy : { name : String, emailAddress : String } -> GlossaryChange -> GlossaryChange
setLastUpdatedBy { name, emailAddress } change =
    case change of
        Insert itemFromDom ->
            { itemFromDom | lastUpdatedByName = Just name, lastUpdatedByEmailAddress = Just emailAddress }
                |> Insert

        Update itemFromDom ->
            { itemFromDom | lastUpdatedByName = Just name, lastUpdatedByEmailAddress = Just emailAddress }
                |> Update

        _ ->
            change
