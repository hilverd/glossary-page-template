module CommonModel exposing (CommonModel, OrderItemsBy(..))

import Data.Glossary exposing (Glossary)
import Data.GlossaryItemIndex exposing (GlossaryItemIndex)
import Extras.HtmlTree exposing (HtmlTree(..))
import Json.Decode as Decode


type OrderItemsBy
    = Alphabetically
    | MostFrequentFirst


type alias CommonModel =
    { filename : Maybe String
    , enableSavingChangesInMemory : Bool
    , orderItemsBy : OrderItemsBy
    , maybeIndex : Maybe GlossaryItemIndex
    , fragment : Maybe String
    , glossary : Result Decode.Error Glossary
    }
