module Data.IncubatingGlossaryItems exposing
    ( IncubatingGlossaryItems
    , fromList
    )

{-| The glossary items that make up a glossary.


# Glossary Items

@docs IncubatingGlossaryItems


# Build

@docs fromList

-}

import Data.GlossaryItem as GlossaryItem exposing (GlossaryItem)
import Data.GlossaryItem.RelatedTerm as RelatedTerm
import Data.GlossaryItem.Term as Term
import Data.GlossaryItem.TermId as TermId exposing (TermId)
import Data.GlossaryItemForHtml exposing (GlossaryItemForHtml)
import Data.GlossaryItemId as GlossaryItemId exposing (GlossaryItemId)
import Data.GlossaryItemIdDict as GlossaryItemIdDict exposing (GlossaryItemIdDict)
import Data.GlossaryItemIndex exposing (GlossaryItemIndex)
import Data.IncubatingGlossaryItem exposing (IncubatingGlossaryItem)
import Dict exposing (Dict)


{-| A set of glossary items.
-}
type IncubatingGlossaryItems
    = IncubatingGlossaryItems
        { itemById : GlossaryItemIdDict IncubatingGlossaryItem
        , relatedItemIdsById : GlossaryItemIdDict (List GlossaryItemId)
        }



-- TODO: copy the Tag module to the top level.
-- TODO: create a TagId module.


{-| Convert a list of glossary items into a `GlossaryItems`.
-}
fromList : List GlossaryItemForHtml -> IncubatingGlossaryItems
fromList glossaryItemForHtmlList =
    -- let
    -- foo =
    --     glossaryItemForHtmlList
    --         |> List.indexedMap Tuple.pair
    --         |> List.foldl
    --             (\( index, glossaryItemForHtml ) { itemById_ } ->
    --                 { itemById_ = itemById_ }
    --             )
    --             { itemById_ = GlossaryItemIdDict.empty
    --             , itemIdByPreferredTerm
    --             }
    -- in
    IncubatingGlossaryItems
        { itemById = GlossaryItemIdDict.empty
        , relatedItemIdsById = GlossaryItemIdDict.empty
        }
