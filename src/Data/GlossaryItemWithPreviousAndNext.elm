module Data.GlossaryItemWithPreviousAndNext exposing (GlossaryItemWithPreviousAndNext)

import Data.GlossaryItemForUi exposing (GlossaryItemForUi)


type alias GlossaryItemWithPreviousAndNext =
    { previous : Maybe GlossaryItemForUi
    , item : Maybe GlossaryItemForUi
    , next : Maybe GlossaryItemForUi
    }
