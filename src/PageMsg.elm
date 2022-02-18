module PageMsg exposing (PageMsg(..))

import CommonModel exposing (CommonModel)
import Data.GlossaryItemIndex exposing (GlossaryItemIndex)
import Data.LoadedGlossaryItems exposing (LoadedGlossaryItems)


type PageMsg a
    = NavigateToListAll CommonModel (Maybe GlossaryItemIndex) LoadedGlossaryItems
    | NavigateToCreateOrEdit CommonModel (Maybe GlossaryItemIndex) LoadedGlossaryItems
    | Internal a
