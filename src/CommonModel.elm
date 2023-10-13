module CommonModel exposing (CommonModel)

import Data.Glossary exposing (Glossary)
import Data.GlossaryItemIndex exposing (GlossaryItemIndex)
import Data.OrderItemsBy exposing (OrderItemsBy)
import Data.Theme exposing (Theme)
import Json.Decode as Decode


type alias CommonModel =
    { filename : Maybe String
    , enableHelpForMakingChanges : Bool
    , theme : Theme
    , enableExportMenu : Bool
    , enableOrderItemsButtons : Bool
    , enableSavingChangesInMemory : Bool
    , orderItemsBy : OrderItemsBy
    , maybeIndex : Maybe GlossaryItemIndex
    , fragment : Maybe String
    , glossary : Result Decode.Error Glossary
    }
