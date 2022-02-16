module Data.LoadedGlossaryItems exposing (LoadedGlossaryItems, decodeFromFlags)

import Data.GlossaryItem as GlossaryItem
import Data.GlossaryItems exposing (GlossaryItems)
import Extras.HtmlTree exposing (HtmlTree(..))
import Json.Decode as Decode


type alias LoadedGlossaryItems =
    Result Decode.Error GlossaryItems


decodeFromFlags : Decode.Value -> LoadedGlossaryItems
decodeFromFlags =
    Decode.decodeValue (Decode.field "glossaryItems" <| Decode.list GlossaryItem.decode)
