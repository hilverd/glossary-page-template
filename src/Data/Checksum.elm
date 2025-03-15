module Data.Checksum exposing (Checksum, againstGlossaryForUi, againstGlossaryFromDom)

{-| Checksums for preventing edit conflicts.


# Checksums

@docs Checksum, againstGlossaryForUi, againstGlossaryFromDom

-}

import Codec
import Data.AboutSection as AboutSection
import Data.CardWidth as CardWidth
import Data.GlossaryChange exposing (GlossaryChange(..))
import Data.GlossaryForUi as GlossaryForUi exposing (GlossaryForUi)
import Data.GlossaryFromDom exposing (GlossaryFromDom)
import Data.GlossaryItem.Tag as Tag
import Data.GlossaryItemForUi as GlossaryItemForUi
import Data.GlossaryItemFromDom as GlossaryItemFromDom
import Data.GlossaryItemId as GlossaryItemId
import Data.GlossaryItemsForUi as GlossaryItemsForUi
import Data.GlossaryTitle as GlossaryTitle
import Data.Theme as Theme
import Extras.Md5


{-| A checksum.
-}
type Checksum
    = Checksum String


{-| Calculate the checksum of the existing data being changed in a `GlossaryForUi`.
-}
againstGlossaryForUi : GlossaryForUi -> GlossaryChange -> Checksum
againstGlossaryForUi glossaryForUi glossaryChange =
    case glossaryChange of
        ToggleEnableLastUpdatedDates ->
            glossaryForUi
                |> GlossaryForUi.enableLastUpdatedDates
                |> stringFromBool
                |> Extras.Md5.hexWithCrlfToLf
                |> Checksum

        ToggleEnableExportMenu ->
            glossaryForUi
                |> GlossaryForUi.enableExportMenu
                |> stringFromBool
                |> Extras.Md5.hexWithCrlfToLf
                |> Checksum

        ToggleEnableOrderItemsButtons ->
            glossaryForUi
                |> GlossaryForUi.enableOrderItemsButtons
                |> stringFromBool
                |> Extras.Md5.hexWithCrlfToLf
                |> Checksum

        SetTitle _ ->
            glossaryForUi
                |> GlossaryForUi.title
                |> GlossaryTitle.raw
                |> Extras.Md5.hexWithCrlfToLf
                |> Checksum

        SetAboutSection _ ->
            glossaryForUi
                |> GlossaryForUi.aboutSection
                |> Codec.encodeToString 0 AboutSection.codec
                |> Extras.Md5.hexWithCrlfToLf
                |> Checksum

        SetCardWidth _ ->
            glossaryForUi
                |> GlossaryForUi.cardWidth
                |> Codec.encodeToString 0 CardWidth.codec
                |> Extras.Md5.hexWithCrlfToLf
                |> Checksum

        SetDefaultTheme _ ->
            glossaryForUi
                |> GlossaryForUi.defaultTheme
                |> Codec.encodeToString 0 Theme.codec
                |> Extras.Md5.hexWithCrlfToLf
                |> Checksum

        ChangeTags _ ->
            glossaryForUi
                |> GlossaryForUi.items
                |> GlossaryItemsForUi.tags
                |> List.map (Codec.encodeToString 0 Tag.codec)
                |> String.join "\n"
                |> Extras.Md5.hexWithCrlfToLf
                |> Checksum

        Update { id } ->
            glossaryForUi
                |> GlossaryForUi.items
                |> GlossaryItemsForUi.get (GlossaryItemId.create id)
                |> Maybe.map
                    (\glossaryItemForUi ->
                        GlossaryItemForUi.toGlossaryItemFromDom glossaryItemForUi
                            |> Codec.encodeToString 0 GlossaryItemFromDom.codec
                    )
                |> Maybe.withDefault ""
                |> Extras.Md5.hexWithCrlfToLf
                |> Checksum

        _ ->
            Checksum ""


{-| Calculate the checksum of the existing data being changed in a `GlossaryFromDom`.
-}
againstGlossaryFromDom : GlossaryFromDom -> GlossaryChange -> Checksum
againstGlossaryFromDom glossaryFromDom glossaryChange =
    case glossaryChange of
        ToggleEnableLastUpdatedDates ->
            glossaryFromDom
                |> .enableLastUpdatedDates
                |> stringFromBool
                |> Extras.Md5.hexWithCrlfToLf
                |> Checksum

        _ ->
            Checksum ""


stringFromBool : Bool -> String
stringFromBool bool =
    if bool then
        "true"

    else
        "false"
