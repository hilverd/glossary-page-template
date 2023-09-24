module Data.LoadedGlossaryItems exposing (LoadedGlossaryItems, decodeFromFlags, decodeIncubatingFromFlags)

import Data.GlossaryItem as GlossaryItem
import Data.GlossaryItemForHtml as GlossaryItemForHtml
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Data.IncubatingGlossaryItems as IncubatingGlossaryItems exposing (IncubatingGlossaryItems)
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
decodeIncubatingFromFlags enableMarkdownBasedSyntax =
    Decode.decodeValue
        (Decode.field "glossaryItems" <|
            Decode.list (GlossaryItemForHtml.decode enableMarkdownBasedSyntax)
        )
        >> Result.map IncubatingGlossaryItems.fromList
