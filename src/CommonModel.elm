module CommonModel exposing (CommonModel)

import Data.Glossary exposing (Glossary)
import Data.GlossaryItemId exposing (GlossaryItemId)
import Data.GlossaryItemIndex exposing (GlossaryItemIndex)
import Data.IncubatingGlossary exposing (IncubatingGlossary)
import Data.OrderItemsBy exposing (OrderItemsBy)
import Data.Theme exposing (Theme)
import Json.Decode as Decode


type alias CommonModel =
    { filename : Maybe String
    , enableHelpForMakingChanges : Bool
    , theme : Theme
    , enableExportMenu : Bool
    , enableSavingChangesInMemory : Bool
    , orderItemsBy : OrderItemsBy
    , maybeId : Maybe GlossaryItemId
    , fragment : Maybe String
    , glossary : Result Decode.Error Glossary
    , incubatingGlossary : Result Decode.Error IncubatingGlossary
    }
