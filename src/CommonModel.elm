module CommonModel exposing (CommonModel, OrderItemsBy(..))

import Data.Glossary exposing (Glossary)
import Data.GlossaryItemIndex exposing (GlossaryItemIndex)
import Json.Decode as Decode


type OrderItemsBy
    = Alphabetically
    | MostMentionedFirst


type alias CommonModel =
    { filename : Maybe String
    , enableHelpForMakingChanges : Bool
    , enableExportMenu : Bool
    , enableSavingChangesInMemory : Bool
    , orderItemsBy : OrderItemsBy
    , maybeIndex : Maybe GlossaryItemIndex
    , fragment : Maybe String
    , glossary : Result Decode.Error Glossary
    }
