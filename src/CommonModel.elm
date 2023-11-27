module CommonModel exposing (CommonModel)

import Browser.Navigation exposing (Key)
import Data.Glossary exposing (Glossary)
import Data.GlossaryItemId exposing (GlossaryItemId)
import Data.OrderItemsBy exposing (OrderItemsBy)
import Data.TagId exposing (TagId)
import Data.Theme exposing (Theme)
import QueryParameters exposing (QueryParameters)


type alias CommonModel =
    { key : Key
    , filename : Maybe String
    , enableHelpForMakingChanges : Bool
    , theme : Theme
    , enableExportMenu : Bool
    , enableOrderItemsButtons : Bool
    , enableSavingChangesInMemory : Bool
    , queryParameters : QueryParameters
    , filterByTag : Maybe TagId
    , orderItemsBy : OrderItemsBy
    , maybeId : Maybe GlossaryItemId
    , fragment : Maybe String
    , glossary : Result String Glossary
    }
