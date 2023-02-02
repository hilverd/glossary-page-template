module Data.LoadedGlossaryItems exposing (LoadedGlossaryItems, decodeFromFlags)

import Data.GlossaryItem as GlossaryItem
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Extras.HtmlTree exposing (HtmlTree(..))
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
