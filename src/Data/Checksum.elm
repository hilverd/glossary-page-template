module Data.Checksum exposing (Checksum, againstGlossaryForUi, againstGlossaryFromDom, codec)

{-| Checksums for preventing edit conflicts.


# Checksums

@docs Checksum, againstGlossaryForUi, againstGlossaryFromDom, codec

-}

import Codec exposing (Codec)
import Data.AboutSection as AboutSection
import Data.CardWidth as CardWidth
import Data.DescribedTagFromDom as DescribedTagFromDom
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
                |> GlossaryTitle.raw
                |> checkSumUsingCodec Codec.string

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
                |> checkSumUsingCodec Codec.bool

        ToggleEnableExportMenu ->
            glossaryFromDom
                |> .enableExportMenu
                |> checkSumUsingCodec Codec.bool

        ToggleEnableOrderItemsButtons ->
            glossaryFromDom
                |> .enableOrderItemsButtons
                |> checkSumUsingCodec Codec.bool

        SetTitle _ ->
            glossaryFromDom
                |> .title
                |> checkSumUsingCodec Codec.string

        SetAboutSection _ ->
            let
                aboutParagraph : String
                aboutParagraph =
                    glossaryFromDom.aboutParagraph

                aboutLinks : String
                aboutLinks =
                    glossaryFromDom.aboutLinks
                        |> List.map (\{ href, body } -> "[" ++ body ++ "](" ++ href ++ ")")
                        |> String.join "\n"
            in
            aboutParagraph
                ++ "\n\n"
                ++ aboutLinks
                |> Extras.Md5.hexWithCrlfToLf
                |> Checksum

        SetCardWidth _ ->
            glossaryFromDom
                |> .cardWidth
                |> checkSumUsingCodec CardWidth.codec

        SetDefaultTheme _ ->
            glossaryFromDom
                |> .defaultTheme
                |> checkSumUsingCodec Theme.codec

        ChangeTags _ ->
            glossaryFromDom.tags
                |> List.map (Codec.encodeToString 0 DescribedTagFromDom.codec)
                |> String.join "\n"
                |> Extras.Md5.hexWithCrlfToLf
                |> Checksum

        Update { id } ->
            glossaryFromDom.items
                |> List.filterMap
                    (\glossaryItemFromDom ->
                        if id == glossaryItemFromDom.id then
                            Just (Codec.encodeToString 0 GlossaryItemFromDom.codec glossaryItemFromDom)

                        else
                            Nothing
                    )
                |> String.join "\n"
                |> Extras.Md5.hexWithCrlfToLf
                |> Checksum

        _ ->
            Checksum ""


checkSumUsingCodec : Codec a -> a -> Checksum
checkSumUsingCodec codec_ =
    Codec.encodeToString 0 codec_
        >> Extras.Md5.hexWithCrlfToLf
        >> Checksum


{-| An encoder/decoder for checksums.
-}
codec : Codec Checksum
codec =
    Codec.map Checksum
        (\(Checksum checksum_) -> checksum_)
        Codec.string
