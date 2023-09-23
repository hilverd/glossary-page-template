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
import Data.GlossaryItem.Tag as Tag exposing (Tag)
import Data.GlossaryItem.Term as Term
import Data.GlossaryItem.TermId as TermId exposing (TermId)
import Data.GlossaryItemForHtml as GlossaryItemForHtml exposing (GlossaryItemForHtml)
import Data.GlossaryItemId as GlossaryItemId exposing (GlossaryItemId)
import Data.GlossaryItemIdDict as GlossaryItemIdDict exposing (GlossaryItemIdDict)
import Data.GlossaryItemIndex exposing (GlossaryItemIndex)
import Data.IncubatingGlossaryItem as IncubatingGlossaryItem exposing (IncubatingGlossaryItem)
import Data.TagId as TagId
import Data.TagIdDict as TagIdDict exposing (TagIdDict)
import Dict exposing (Dict)
import Set


{-| A set of glossary items.
-}
type IncubatingGlossaryItems
    = IncubatingGlossaryItems
        { itemById : GlossaryItemIdDict IncubatingGlossaryItem
        , tagById : TagIdDict Tag

        -- , disambiguationTagByItemId : GlossaryItemIdDict (Maybe Tag)
        -- , normalTagsByItemId : GlossaryItemIdDict (List Tag)
        -- , relatedItemIdsById : GlossaryItemIdDict (List GlossaryItemId)
        }


{-| Convert a list of glossary items into a `GlossaryItems`.
-}
fromList : List GlossaryItemForHtml -> IncubatingGlossaryItems
fromList glossaryItemForHtmlList =
    let
        ( itemById, tagById ) =
            glossaryItemForHtmlList
                |> List.indexedMap Tuple.pair
                |> List.foldl
                    (\( index, glossaryItemForHtml ) { itemById_, tagById_, allRawTags, nextTagIdInt } ->
                        let
                            itemId =
                                GlossaryItemId.create index

                            glossaryItem : IncubatingGlossaryItem
                            glossaryItem =
                                IncubatingGlossaryItem.init
                                    (GlossaryItemForHtml.preferredTerm glossaryItemForHtml)
                                    (GlossaryItemForHtml.alternativeTerms glossaryItemForHtml)
                                    (GlossaryItemForHtml.definition glossaryItemForHtml)
                                    (GlossaryItemForHtml.needsUpdating glossaryItemForHtml)
                                    (GlossaryItemForHtml.lastUpdatedDateAsIso8601 glossaryItemForHtml)

                            itemById1 =
                                GlossaryItemIdDict.insert itemId glossaryItem itemById_

                            updated =
                                glossaryItemForHtml
                                    |> GlossaryItemForHtml.allTags
                                    |> List.foldl
                                        (\tag { tagById1, allRawTags1, nextTagIdInt1 } ->
                                            let
                                                rawTag =
                                                    Tag.raw tag
                                            in
                                            if Set.member rawTag allRawTags1 then
                                                { tagById1 = tagById1
                                                , allRawTags1 = allRawTags1
                                                , nextTagIdInt1 = nextTagIdInt1
                                                }

                                            else
                                                { tagById1 =
                                                    TagIdDict.insert
                                                        (TagId.create nextTagIdInt1)
                                                        tag
                                                        tagById1
                                                , allRawTags1 = Set.insert rawTag allRawTags1
                                                , nextTagIdInt1 = nextTagIdInt1 + 1
                                                }
                                        )
                                        { tagById1 = tagById_
                                        , allRawTags1 = allRawTags
                                        , nextTagIdInt1 = nextTagIdInt
                                        }
                        in
                        { itemById_ = itemById1
                        , tagById_ = updated.tagById1
                        , allRawTags = updated.allRawTags1
                        , nextTagIdInt = updated.nextTagIdInt1
                        }
                    )
                    { itemById_ = GlossaryItemIdDict.empty
                    , tagById_ = TagIdDict.empty
                    , allRawTags = Set.empty
                    , nextTagIdInt = 0
                    }
                |> (\{ itemById_, tagById_ } -> ( itemById_, tagById_ ))
    in
    IncubatingGlossaryItems
        { itemById = itemById
        , tagById = tagById
        }
