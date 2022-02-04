module PageMsg exposing (PageMsg(..))

import Data.AboutHtml as AboutHtml exposing (AboutHtml)
import Data.GlossaryItemIndex as GlossaryItemIndex exposing (GlossaryItemIndex)
import Data.LoadedGlossaryItems exposing (LoadedGlossaryItems)
import Data.TitleHeaderHtml as TitleHeaderHtml exposing (TitleHeaderHtml)


type PageMsg a
    = NavigateToListAll Bool TitleHeaderHtml AboutHtml (Maybe GlossaryItemIndex) LoadedGlossaryItems
    | NavigateToCreateOrEdit Bool TitleHeaderHtml AboutHtml (Maybe GlossaryItemIndex) LoadedGlossaryItems
    | Internal a
