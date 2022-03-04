module CommonModel exposing (CommonModel, OrderItemsBy(..))

import Data.AboutLink exposing (AboutLink)
import Data.GlossaryItemIndex exposing (GlossaryItemIndex)
import Data.LoadedGlossaryItems exposing (LoadedGlossaryItems)
import Extras.HtmlTree exposing (HtmlTree(..))


type OrderItemsBy
    = Alphabetically
    | MostFrequentFirst


type alias CommonModel =
    { enableHelpForMakingChanges : Bool
    , title : String
    , aboutParagraph : String
    , aboutLinks : List AboutLink
    , orderItemsBy : OrderItemsBy
    , loadedGlossaryItems : LoadedGlossaryItems
    , maybeIndex : Maybe GlossaryItemIndex
    }
