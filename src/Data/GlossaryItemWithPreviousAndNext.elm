module Data.GlossaryItemWithPreviousAndNext exposing (GlossaryItemWithPreviousAndNext)

import Data.GlossaryItem exposing (GlossaryItem)
import Data.GlossaryItemIndex exposing (GlossaryItemIndex)


type alias GlossaryItemWithPreviousAndNext =
    { previous : Maybe ( GlossaryItemIndex, GlossaryItem )
    , item : Maybe ( GlossaryItemIndex, GlossaryItem )
    , next : Maybe ( GlossaryItemIndex, GlossaryItem )
    }
