module Data.GlossaryChange exposing (GlossaryChange(..), codec, setLastUpdatedBy)

{-| A representation of a change to be made to a glossary.


# Glossary Change

@docs GlossaryChange, codec, setLastUpdatedBy

-}

import Codec exposing (Codec)
import Data.AboutSection as AboutSection exposing (AboutSection)
import Data.CardWidth as CardWidth exposing (CardWidth)
import Data.GlossaryItemForUi as GlossaryItemForUi exposing (GlossaryItemForUi)
import Data.GlossaryItemId as GlossaryItemId exposing (GlossaryItemId)
import Data.GlossaryTitle as GlossaryTitle exposing (GlossaryTitle)
import Data.TagsChanges as TagsChanges exposing (TagsChanges)


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
    | Insert GlossaryItemForUi
    | Update GlossaryItemForUi
    | Remove GlossaryItemId


{-| An encoder/decoder for glossary changes.
-}
codec : Codec GlossaryChange
codec =
    Codec.custom
        (\toggleEnableLastUpdatedDates toggleEnableExportMenu toggleEnableOrderItemsButtons setTitle setAboutSection setCardWidth changeTags insert update remove value ->
            case value of
                ToggleEnableLastUpdatedDates ->
                    toggleEnableLastUpdatedDates

                ToggleEnableExportMenu ->
                    toggleEnableExportMenu

                ToggleEnableOrderItemsButtons ->
                    toggleEnableOrderItemsButtons

                SetTitle title ->
                    setTitle title

                SetAboutSection aboutSection ->
                    setAboutSection aboutSection

                SetCardWidth cardWidth ->
                    setCardWidth cardWidth

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
        |> Codec.variant1 "SetTitle" SetTitle GlossaryTitle.codec
        |> Codec.variant1 "SetAboutSection" SetAboutSection AboutSection.codec
        |> Codec.variant1 "SetCardWidth" SetCardWidth CardWidth.codec
        |> Codec.variant1 "ChangeTags" ChangeTags TagsChanges.codec
        |> Codec.variant1 "Insert" Insert GlossaryItemForUi.codec
        |> Codec.variant1 "Update" Update GlossaryItemForUi.codec
        |> Codec.variant1 "Remove" Remove GlossaryItemId.codec
        |> Codec.buildCustom


{-| Set the name and email address of the person making this glossary change.
-}
setLastUpdatedBy : { name : String, emailAddress : String } -> GlossaryChange -> GlossaryChange
setLastUpdatedBy { name, emailAddress } change =
    case change of
        Insert itemForUi ->
            itemForUi
                |> GlossaryItemForUi.setLastUpdatedBy { name = name, emailAddress = emailAddress }
                |> Insert

        Update itemForUi ->
            itemForUi
                |> GlossaryItemForUi.setLastUpdatedBy { name = name, emailAddress = emailAddress }
                |> Update

        _ ->
            change
