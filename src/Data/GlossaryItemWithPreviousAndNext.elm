module Data.GlossaryItemWithPreviousAndNext exposing (GlossaryItemWithPreviousAndNext)

import Data.GlossaryItemForHtml exposing (GlossaryItemForHtml)


type alias GlossaryItemWithPreviousAndNext =
    { previous : Maybe GlossaryItemForHtml
    , item : Maybe GlossaryItemForHtml
    , next : Maybe GlossaryItemForHtml
    }
