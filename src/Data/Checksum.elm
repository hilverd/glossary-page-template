module Data.Checksum exposing (Checksum, againstGlossaryForUi, againstGlossaryFromDom)

{-| Checksums for preventing edit conflicts.


# Checksums

@docs Checksum, againstGlossaryForUi, againstGlossaryFromDom

-}

import Codec exposing (Codec)
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
                |> checkSumUsingCodec Codec.bool

        ToggleEnableExportMenu ->
            glossaryForUi
                |> GlossaryForUi.enableExportMenu
                |> checkSumUsingCodec Codec.bool

        ToggleEnableOrderItemsButtons ->
            glossaryForUi
                |> GlossaryForUi.enableOrderItemsButtons
                |> checkSumUsingCodec Codec.bool

        SetTitle _ ->
            glossaryForUi
                |> GlossaryForUi.title
                |> checkSumUsingCodec GlossaryTitle.codec

        SetAboutSection _ ->
            glossaryForUi
                |> GlossaryForUi.aboutSection
                |> checkSumUsingCodec AboutSection.codec

        SetCardWidth _ ->
            glossaryForUi
                |> GlossaryForUi.cardWidth
                |> checkSumUsingCodec CardWidth.codec

        SetDefaultTheme _ ->
            glossaryForUi
                |> GlossaryForUi.defaultTheme
                |> checkSumUsingCodec Theme.codec

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
                |> Codec.encodeToString 0 Codec.bool
                |> Extras.Md5.hexWithCrlfToLf
                |> Checksum

        _ ->
            Checksum ""


checkSumUsingCodec : Codec a -> a -> Checksum
checkSumUsingCodec codec =
    Codec.encodeToString 0 codec
        >> Extras.Md5.hexWithCrlfToLf
        >> Checksum
