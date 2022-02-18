module CommonModel exposing (CommonModel, OrderItemsBy(..))

import Data.AboutHtml exposing (AboutHtml)
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
    }
