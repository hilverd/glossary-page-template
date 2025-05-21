module PageMsg exposing (PageMsg(..))

import CommonModel exposing (CommonModel)
import Data.GlossaryItemId exposing (GlossaryItemId)
import Data.Notification exposing (Notification)


type PageMsg a
    = NavigateToListAll CommonModel (Maybe GlossaryItemId) (Maybe Notification)
    | NavigateToCreateOrEdit (Maybe GlossaryItemId)
    | NavigateToEditTitleAndAbout
    | NavigateToManageTags
    | Internal a
