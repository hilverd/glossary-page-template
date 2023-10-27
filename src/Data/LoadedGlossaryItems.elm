module Data.LoadedGlossaryItems exposing (LoadedGlossaryItems, decodeFromFlags, decodeIncubatingFromFlags)

import Data.GlossaryItem as GlossaryItem
import Data.GlossaryItem.Tag as Tag exposing (Tag)
import Data.GlossaryItemForHtml as GlossaryItemForHtml
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Data.IncubatingGlossaryItems as IncubatingGlossaryItems exposing (IncubatingGlossaryItems)
import Data.TagDescription as TagDescription exposing (TagDescription)
import Json.Decode as Decode


type alias LoadedGlossaryItems =
    Result Decode.Error GlossaryItems


decodeFromFlags : Bool -> Decode.Value -> LoadedGlossaryItems
decodeFromFlags enableMarkdownBasedSyntax =
    Decode.decodeValue
        (Decode.field "glossaryItems" <|
            Decode.list (GlossaryItem.decode enableMarkdownBasedSyntax)
        )
        >> Result.map GlossaryItems.fromList


decodeIncubatingFromFlags : Bool -> Decode.Value -> Result Decode.Error IncubatingGlossaryItems
decodeIncubatingFromFlags enableMarkdownBasedSyntax flags =
    let
        tagsWithDescriptions : List ( Tag, TagDescription )
        tagsWithDescriptions =
            flags
                |> Decode.decodeValue
                    (Decode.field "tagsWithDescriptions" <|
                        Decode.list <|
                            Decode.map2
                                (\tagString descriptionString ->
                                    ( Tag.fromMarkdown tagString
                                    , if enableMarkdownBasedSyntax then
                                        TagDescription.fromMarkdown descriptionString

                                      else
                                        TagDescription.fromPlaintext descriptionString
                                    )
                                )
                                (Decode.field "tag" <| Decode.string)
                                (Decode.field "description" <| Decode.string)
                    )
                |> Result.withDefault []
    in
    flags
        |> Decode.decodeValue
            (Decode.field "glossaryItems" <|
                Decode.list (GlossaryItemForHtml.decode enableMarkdownBasedSyntax)
            )
        |> Result.map (IncubatingGlossaryItems.fromList tagsWithDescriptions)
