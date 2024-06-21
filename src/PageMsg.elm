module PageMsg exposing (PageMsg(..))

import CommonModel exposing (CommonModel)
import Data.GlossaryItemId exposing (GlossaryItemId)


type PageMsg a
    = NavigateToListAll CommonModel (Maybe GlossaryItemId)
    | NavigateToCreateOrEdit (Maybe GlossaryItemId)
    | NavigateToEditTitleAndAbout
    | NavigateToManageTags
    | Internal a
