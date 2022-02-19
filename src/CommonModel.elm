module CommonModel exposing (CommonModel, OrderItemsBy(..))

import Data.AboutHtml exposing (AboutHtml)
import Data.GlossaryItemIndex exposing (GlossaryItemIndex)
import Data.LoadedGlossaryItems exposing (LoadedGlossaryItems)
import Data.TitleHeaderHtml exposing (TitleHeaderHtml)
import Extras.HtmlTree exposing (HtmlTree(..))


type OrderItemsBy
    = Alphabetically
    | MostFrequentFirst


type alias CommonModel =
    { enableHelpForMakingChanges : Bool
    , titleHeaderHtml : TitleHeaderHtml
    , aboutHtml : AboutHtml
    , orderItemsBy : OrderItemsBy
    , loadedGlossaryItems : LoadedGlossaryItems
    , maybeIndex : Maybe GlossaryItemIndex
    }
