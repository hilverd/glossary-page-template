module PageMsg exposing (PageMsg(..))

import CommonModel exposing (CommonModel)
import Data.GlossaryItemId exposing (GlossaryItemId)


type PageMsg a
    = NavigateToListAll CommonModel
    | NavigateToCreateOrEdit (Maybe GlossaryItemId)
    | NavigateToEditTitleAndAbout
    | NavigateToManageTags CommonModel
    | Internal a
