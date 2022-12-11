module CommonModel exposing (CommonModel, OrderItemsBy(..))

import Data.AboutLink exposing (AboutLink)
import Data.AboutParagraph exposing (AboutParagraph)
import Data.AboutSection exposing (AboutSection)
import Data.GlossaryItemIndex exposing (GlossaryItemIndex)
import Data.GlossaryTitle exposing (GlossaryTitle)
import Data.LoadedGlossaryItems exposing (LoadedGlossaryItems)
import Extras.HtmlTree exposing (HtmlTree(..))


type OrderItemsBy
    = Alphabetically
    | MostFrequentFirst


type alias CommonModel =
    { filename : Maybe String
    , enableHelpForMakingChanges : Bool
    , enableSavingChangesInMemory : Bool
    , title : GlossaryTitle
    , aboutSection : AboutSection
    , orderItemsBy : OrderItemsBy
    , loadedGlossaryItems : LoadedGlossaryItems
    , maybeIndex : Maybe GlossaryItemIndex
    , fragment : Maybe String
    }
