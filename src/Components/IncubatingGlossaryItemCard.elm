module Components.IncubatingGlossaryItemCard exposing (view, viewRelatedItem)

import Data.GlossaryItem.Tag exposing (Tag)
import Data.GlossaryItemForUi as GlossaryItemForUi exposing (GlossaryItemForUi)
import Data.GlossaryItemId exposing (GlossaryItemId)
import Html exposing (Html, text)


view :
    { enableMathSupport : Bool
    , enableLastUpdatedDates : Bool
    , onClickCopyToClipboard : msg
    , onClickEdit : msg
    , onClickDelete : msg
    , onClickTag : Tag -> msg
    , resultOfAttemptingToCopyItemTextToClipboard : Maybe Bool
    , editable : Bool
    }
    -> Maybe Tag
    -> GlossaryItemForUi
    -> Html msg
view { enableMathSupport, enableLastUpdatedDates, onClickCopyToClipboard, onClickEdit, onClickDelete, onClickTag, resultOfAttemptingToCopyItemTextToClipboard, editable } tagBeingFilteredBy glossaryItem =
    let
        index : GlossaryItemId
        index =
            GlossaryItemForUi.id glossaryItem

        tags : List Tag
        tags =
            GlossaryItemForUi.allTags glossaryItem

        tagsNotBeingFilteredBy : List Tag
        tagsNotBeingFilteredBy =
            tags |> List.filter (Just >> (/=) tagBeingFilteredBy)

        lastUpdatedDate : Maybe String
        lastUpdatedDate =
            GlossaryItemForUi.lastUpdatedDateAsIso8601 glossaryItem

        lastUpdatedByName : Maybe String
        lastUpdatedByName =
            GlossaryItemForUi.lastUpdatedByName glossaryItem

        lastUpdatedByEmailAddress : Maybe String
        lastUpdatedByEmailAddress =
            GlossaryItemForUi.lastUpdatedByEmailAddress glossaryItem
    in
    text "TODO"


viewRelatedItem : Html msg
viewRelatedItem =
    text "TODO"
