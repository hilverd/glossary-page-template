module CommonModel exposing (CommonModel)

import Data.Glossary exposing (Glossary)
import Data.GlossaryItemId exposing (GlossaryItemId)
import Data.OrderItemsBy exposing (OrderItemsBy)
import Data.TagId exposing (TagId)
import Data.Theme exposing (Theme)
import Json.Decode as Decode


type alias CommonModel =
    { filename : Maybe String
    , enableHelpForMakingChanges : Bool
    , theme : Theme
    , enableExportMenu : Bool
    , enableOrderItemsButtons : Bool
    , enableSavingChangesInMemory : Bool
    , filterByTag : Maybe TagId
    , orderItemsBy : OrderItemsBy
    , maybeId : Maybe GlossaryItemId
    , fragment : Maybe String
    , glossary : Result Decode.Error Glossary
    }
