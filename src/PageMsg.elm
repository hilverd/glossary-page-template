module PageMsg exposing (PageMsg(..))

import Data.LoadedGlossaryItems exposing (LoadedGlossaryItems)


type PageMsg a
    = NavigateToListAll Bool (Maybe Int) LoadedGlossaryItems
    | NavigateToCreateOrEdit Bool (Maybe Int) LoadedGlossaryItems
    | Internal a
