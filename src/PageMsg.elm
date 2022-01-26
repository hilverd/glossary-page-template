module PageMsg exposing (PageMsg(..))

import Data.GlossaryItemIndex as GlossaryItemIndex exposing (GlossaryItemIndex)
import Data.LoadedGlossaryItems exposing (LoadedGlossaryItems)


type PageMsg a
    = NavigateToListAll Bool (Maybe GlossaryItemIndex) LoadedGlossaryItems
    | NavigateToCreateOrEdit Bool (Maybe GlossaryItemIndex) LoadedGlossaryItems
    | Internal a
