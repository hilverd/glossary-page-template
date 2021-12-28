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



{--

* What sort of changes might happen when you edit an item?
  * A term might be removed or renamed. We can't know for sure, the user should probably tell us -- but this is not straightforward.

* What sort of changes might happen when you delete an item item_1?
  * An existing item might list item_1 in its related terms. It needs to be removed from the list of related terms.

For now it's probably best to "sanitise" or "clean up" a list of glossary items just before saving.
Sanitising means removing any related terms that don't actually exist.

--}
