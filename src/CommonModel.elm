module CommonModel exposing (CommonModel)

import Data.AboutHtml exposing (AboutHtml)
import Data.TitleHeaderHtml exposing (TitleHeaderHtml)
import Extras.HtmlTree exposing (HtmlTree(..))


type alias CommonModel =
    { enableHelpForMakingChanges : Bool
    , titleHeaderHtml : TitleHeaderHtml
    , aboutHtml : AboutHtml
    }
