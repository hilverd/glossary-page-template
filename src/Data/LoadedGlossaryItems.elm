module Data.LoadedGlossaryItems exposing (decodeFromFlags)

import Data.GlossaryItem.Tag as Tag exposing (Tag)
import Data.GlossaryItemForHtml as GlossaryItemForHtml
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Data.TagDescription as TagDescription exposing (TagDescription)
import Json.Decode as Decode


decodeFromFlags : Bool -> Decode.Value -> Result Decode.Error GlossaryItems
decodeFromFlags enableMarkdownBasedSyntax flags =
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
        |> Result.map (GlossaryItems.fromList tagsWithDescriptions)
