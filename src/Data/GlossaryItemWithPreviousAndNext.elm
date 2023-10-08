module Data.GlossaryItemWithPreviousAndNext exposing (GlossaryItemWithPreviousAndNext)

import Data.GlossaryItemForHtml exposing (GlossaryItemForHtml)
import Data.GlossaryItemId exposing (GlossaryItemId)


type alias GlossaryItemWithPreviousAndNext =
    { previous : Maybe ( GlossaryItemId, GlossaryItemForHtml )
    , item : Maybe ( GlossaryItemId, GlossaryItemForHtml )
    , next : Maybe ( GlossaryItemId, GlossaryItemForHtml )
    }
