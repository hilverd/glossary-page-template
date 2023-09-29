module Data.IncubatingGlossaryItems exposing
    ( IncubatingGlossaryItems
    , fromList, insertTag
    , get
    , enableFocusingOn
    , orderedAlphabetically, orderedByMostMentionedFirst, orderedFocusedOn
    )

{-| The glossary items that make up a glossary.


# Glossary Items

@docs IncubatingGlossaryItems


# Build

@docs fromList, insertTag


# Query

@docs get


# Prepare to Export

@docs enableFocusingOn


# Export

@docs orderedAlphabetically, orderedByMostMentionedFirst, orderedFocusedOn

-}

import Data.GlossaryItem.Definition as Definition exposing (Definition)
import Data.GlossaryItem.Tag as Tag exposing (Tag)
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItem.TermId as TermId
import Data.GlossaryItemForHtml as GlossaryItemForHtml exposing (GlossaryItemForHtml, relatedPreferredTerms)
import Data.GlossaryItemId as GlossaryItemId exposing (GlossaryItemId)
import Data.GlossaryItemIdDict as GlossaryItemIdDict exposing (GlossaryItemIdDict)
import Data.IncubatingGlossaryItem as IncubatingGlossaryItem exposing (IncubatingGlossaryItem, alternativeTerms, lastUpdatedDateAsIso8601)
import Data.TagId as TagId exposing (TagId)
import Data.TagIdDict as TagIdDict exposing (TagIdDict)
import Dict exposing (Dict)
import DirectedGraph exposing (DirectedGraph)
import Extras.Regex
import Maybe
import Regex
import Set


{-| A set of glossary items.
-}
type IncubatingGlossaryItems
    = IncubatingGlossaryItems
        { itemById : GlossaryItemIdDict IncubatingGlossaryItem
        , tagById : TagIdDict Tag
        , disambiguationTagIdByItemId : GlossaryItemIdDict (Maybe TagId)
        , normalTagIdsByItemId : GlossaryItemIdDict (List TagId)
        , itemIdsByTagId : TagIdDict (List GlossaryItemId)
        , relatedItemIdsById : GlossaryItemIdDict (List GlossaryItemId)
        , orderedAlphabetically : List GlossaryItemId
        , orderedByMostMentionedFirst : List GlossaryItemId
        , orderedFocusedOn : Maybe ( GlossaryItemId, ( List GlossaryItemId, List GlossaryItemId ) )
        }


orderAlphabetically : List ( GlossaryItemId, GlossaryItemForHtml ) -> List GlossaryItemId
orderAlphabetically =
    List.sortWith
        (\( _, item1 ) ( _, item2 ) ->
            Term.compareAlphabetically
                (GlossaryItemForHtml.preferredTerm item1)
                (GlossaryItemForHtml.preferredTerm item2)
        )
        >> List.map Tuple.first


orderByMostMentionedFirst : List ( GlossaryItemId, GlossaryItemForHtml ) -> List GlossaryItemId
orderByMostMentionedFirst indexedGlossaryItemsForHtml =
    let
        -- Maps a term to a score based on whether or not it occurs in glossaryItem.
        -- This is done in a primitive way. A more sophisticated solution could use stemming
        -- or other techniques.
        termScoreInItem : Term -> GlossaryItemForHtml -> Int
        termScoreInItem term glossaryItem =
            let
                termAsWord : Regex.Regex
                termAsWord =
                    ("\\b" ++ Extras.Regex.escapeStringForUseInRegex (Term.raw term) ++ "\\b")
                        |> Regex.fromString
                        |> Maybe.withDefault Regex.never

                score : Int
                score =
                    (glossaryItem |> GlossaryItemForHtml.allTerms |> List.map (Term.raw >> Regex.find termAsWord >> List.length) |> List.sum)
                        + (glossaryItem |> GlossaryItemForHtml.definition |> Maybe.map (Definition.raw >> Regex.find termAsWord >> List.length) |> Maybe.withDefault 0)
                        + (glossaryItem |> GlossaryItemForHtml.relatedPreferredTerms |> List.map Term.raw |> List.map (Regex.find termAsWord >> List.length) |> List.sum)
            in
            if score > 0 then
                1

            else
                0

        -- Maps a term to a score based on how often it occurs in indexedGlossaryItemsForHtml.
        termScore : Term -> GlossaryItemId -> Int
        termScore term exceptId =
            indexedGlossaryItemsForHtml
                |> List.foldl
                    (\( id, glossaryItem ) result ->
                        result
                            + (if id == exceptId then
                                0

                               else
                                termScoreInItem term glossaryItem
                              )
                    )
                    0

        termBodyScores : Dict String Int
        termBodyScores =
            indexedGlossaryItemsForHtml
                |> List.concatMap
                    (\( id, glossaryItem ) ->
                        glossaryItem
                            |> GlossaryItemForHtml.allTerms
                            |> List.map (Tuple.pair id)
                    )
                |> List.foldl
                    (\( glossaryItemId, term ) result ->
                        Dict.insert
                            (Term.raw term)
                            (termScore term glossaryItemId)
                            result
                    )
                    Dict.empty
    in
    indexedGlossaryItemsForHtml
        |> List.sortWith
            (\( _, item1 ) ( _, item2 ) ->
                let
                    itemScore : GlossaryItemForHtml -> Int
                    itemScore =
                        GlossaryItemForHtml.allTerms
                            >> List.map
                                (\term ->
                                    termBodyScores
                                        |> Dict.get (Term.raw term)
                                        |> Maybe.withDefault 0
                                )
                            >> List.sum
                in
                case compare (itemScore item1) (itemScore item2) of
                    LT ->
                        GT

                    EQ ->
                        compare
                            (item1 |> GlossaryItemForHtml.preferredTerm |> Term.raw |> String.toUpper)
                            (item2 |> GlossaryItemForHtml.preferredTerm |> Term.raw |> String.toUpper)

                    GT ->
                        LT
            )
        |> List.map Tuple.first


{-| Convert a list of glossary items for/from HTML into a `GlossaryItems`.
-}
fromList : List GlossaryItemForHtml -> IncubatingGlossaryItems
fromList glossaryItemsForHtml =
    let
        indexedGlossaryItemsForHtml : List ( GlossaryItemId, GlossaryItemForHtml )
        indexedGlossaryItemsForHtml =
            List.indexedMap (GlossaryItemId.create >> Tuple.pair) glossaryItemsForHtml

        itemIdByPreferredTermId : Dict String GlossaryItemId
        itemIdByPreferredTermId =
            indexedGlossaryItemsForHtml
                |> List.foldl
                    (\( itemId, item ) ->
                        Dict.insert
                            (GlossaryItemForHtml.preferredTerm item
                                |> Term.id
                                |> TermId.toString
                            )
                            itemId
                    )
                    Dict.empty

        ( itemById, tagById ) =
            indexedGlossaryItemsForHtml
                |> List.foldl
                    (\( itemId, glossaryItemForHtml ) { itemById_, tagById_, allRawTags, nextTagIdInt } ->
                        let
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

        tagIdByRawTag : Dict String TagId
        tagIdByRawTag =
            TagIdDict.foldl
                (\tagId tag ->
                    Dict.insert (Tag.raw tag) tagId
                )
                Dict.empty
                tagById

        ( disambiguationTagIdByItemId, normalTagIdsByItemId ) =
            indexedGlossaryItemsForHtml
                |> List.foldl
                    (\( id, item ) ( disambiguationTagByItemId_, normalTagsByItemId_ ) ->
                        ( GlossaryItemIdDict.insert id
                            (item
                                |> GlossaryItemForHtml.disambiguationTag
                                |> Maybe.andThen
                                    (\disambiguationTag ->
                                        Dict.get (Tag.raw disambiguationTag) tagIdByRawTag
                                    )
                            )
                            disambiguationTagByItemId_
                        , GlossaryItemIdDict.insert id
                            (item
                                |> GlossaryItemForHtml.normalTags
                                |> List.filterMap
                                    (\tag ->
                                        Dict.get (Tag.raw tag) tagIdByRawTag
                                    )
                            )
                            normalTagsByItemId_
                        )
                    )
                    ( GlossaryItemIdDict.empty, GlossaryItemIdDict.empty )

        itemIdsByTagId_ : TagIdDict (List GlossaryItemId)
        itemIdsByTagId_ =
            let
                result0 =
                    disambiguationTagIdByItemId
                        |> GlossaryItemIdDict.foldl
                            (\itemId disambiguationTagId result ->
                                disambiguationTagId
                                    |> Maybe.map
                                        (\disambiguationTagId_ ->
                                            TagIdDict.update disambiguationTagId_
                                                (\itemIds ->
                                                    itemIds
                                                        |> Maybe.map ((::) itemId)
                                                        |> Maybe.withDefault [ itemId ]
                                                        |> Just
                                                )
                                                result
                                        )
                                    |> Maybe.withDefault result
                            )
                            TagIdDict.empty
            in
            normalTagIdsByItemId
                |> GlossaryItemIdDict.foldl
                    (\itemId normalTagIds result ->
                        normalTagIds
                            |> List.foldl
                                (\normalTagId result_ ->
                                    TagIdDict.update normalTagId
                                        (\itemIds ->
                                            itemIds
                                                |> Maybe.map ((::) itemId)
                                                |> Maybe.withDefault [ itemId ]
                                                |> Just
                                        )
                                        result_
                                )
                                result
                    )
                    result0

        relatedItemIdsById : GlossaryItemIdDict (List GlossaryItemId)
        relatedItemIdsById =
            indexedGlossaryItemsForHtml
                |> List.foldl
                    (\( id, item ) ->
                        item
                            |> GlossaryItemForHtml.relatedPreferredTerms
                            |> List.filterMap
                                (\relatedPreferredTerm ->
                                    Dict.get
                                        (relatedPreferredTerm |> Term.id |> TermId.toString)
                                        itemIdByPreferredTermId
                                )
                            |> GlossaryItemIdDict.insert id
                    )
                    GlossaryItemIdDict.empty

        orderedAlphabetically_ : List GlossaryItemId
        orderedAlphabetically_ =
            orderAlphabetically indexedGlossaryItemsForHtml

        orderedByMostMentionedFirst_ =
            orderByMostMentionedFirst indexedGlossaryItemsForHtml
    in
    IncubatingGlossaryItems
        { itemById = itemById
        , tagById = tagById
        , disambiguationTagIdByItemId = disambiguationTagIdByItemId
        , normalTagIdsByItemId = normalTagIdsByItemId
        , itemIdsByTagId = itemIdsByTagId_
        , relatedItemIdsById = relatedItemIdsById
        , orderedAlphabetically = orderedAlphabetically_
        , orderedByMostMentionedFirst = orderedByMostMentionedFirst_
        , orderedFocusedOn = Nothing
        }


{-| Insert a tag.
-}
insertTag : Tag -> IncubatingGlossaryItems -> IncubatingGlossaryItems
insertTag tag glossaryItems =
    case glossaryItems of
        IncubatingGlossaryItems items ->
            let
                tagById_ : TagIdDict Tag
                tagById_ =
                    items.tagById
                        |> TagIdDict.insertWithNextTagId tag
            in
            IncubatingGlossaryItems { items | tagById = tagById_ }


{-| Get the item associated with an ID. If the ID is not found, return `Nothing`.
-}
get : GlossaryItemId -> IncubatingGlossaryItems -> Maybe GlossaryItemForHtml
get itemId glossaryItems =
    case glossaryItems of
        IncubatingGlossaryItems items ->
            GlossaryItemIdDict.get itemId items.itemById
                |> Maybe.map
                    (\item ->
                        let
                            preferredTerm : Term
                            preferredTerm =
                                IncubatingGlossaryItem.preferredTerm item

                            alternativeTerms =
                                IncubatingGlossaryItem.alternativeTerms item

                            disambiguationTag =
                                items.disambiguationTagIdByItemId
                                    |> GlossaryItemIdDict.get itemId
                                    |> Maybe.andThen identity
                                    |> Maybe.andThen
                                        (\disambiguationTagId ->
                                            TagIdDict.get disambiguationTagId items.tagById
                                        )

                            normalTags =
                                items.normalTagIdsByItemId
                                    |> GlossaryItemIdDict.get itemId
                                    |> Maybe.map
                                        (List.filterMap
                                            (\normalTagId ->
                                                TagIdDict.get normalTagId items.tagById
                                            )
                                        )
                                    |> Maybe.withDefault []

                            definition : Maybe Definition
                            definition =
                                IncubatingGlossaryItem.definition item

                            relatedPreferredTerms : List Term
                            relatedPreferredTerms =
                                items.relatedItemIdsById
                                    |> GlossaryItemIdDict.get itemId
                                    |> Maybe.map
                                        (\relatedItemIds ->
                                            relatedItemIds
                                                |> List.filterMap
                                                    (\relatedItemId ->
                                                        items.itemById
                                                            |> GlossaryItemIdDict.get relatedItemId
                                                            |> Maybe.map
                                                                (\relatedItem ->
                                                                    let
                                                                        preferredTerm_ =
                                                                            IncubatingGlossaryItem.preferredTerm relatedItem
                                                                    in
                                                                    preferredTerm_
                                                                )
                                                    )
                                        )
                                    |> Maybe.withDefault []

                            needsUpdating =
                                IncubatingGlossaryItem.needsUpdating item

                            lastUpdatedDateAsIso8601 =
                                IncubatingGlossaryItem.lastUpdatedDateAsIso8601 item
                        in
                        GlossaryItemForHtml.create
                            preferredTerm
                            alternativeTerms
                            disambiguationTag
                            normalTags
                            definition
                            relatedPreferredTerms
                            needsUpdating
                            lastUpdatedDateAsIso8601
                    )


{-| Make it easy to retrieve the items ordered "focused on" a specific item.
Returns an updated `GlossaryItems` where the necessary computations have been done.
-}
enableFocusingOn : GlossaryItemId -> IncubatingGlossaryItems -> IncubatingGlossaryItems
enableFocusingOn itemId glossaryItems =
    case glossaryItems of
        IncubatingGlossaryItems items ->
            let
                itemIdsGraph : DirectedGraph GlossaryItemId
                itemIdsGraph =
                    items.itemById
                        |> GlossaryItemIdDict.keys
                        |> List.foldl
                            DirectedGraph.insertVertex
                            (DirectedGraph.empty
                                (GlossaryItemId.toInt >> String.fromInt)
                                (String.toInt >> Maybe.withDefault 0 >> GlossaryItemId.create)
                            )

                relatedItemsGraph : DirectedGraph GlossaryItemId
                relatedItemsGraph =
                    items.relatedItemIdsById
                        |> GlossaryItemIdDict.foldl
                            (\id relatedItemIds result ->
                                let
                                    itemHasDefinition =
                                        items.itemById
                                            |> GlossaryItemIdDict.get id
                                            |> Maybe.map (IncubatingGlossaryItem.definition >> (/=) Nothing)
                                            |> Maybe.withDefault False
                                in
                                if itemHasDefinition then
                                    List.foldl
                                        (DirectedGraph.insertEdge id)
                                        result
                                        relatedItemIds

                                else
                                    result
                            )
                            itemIdsGraph

                itemIdsByDistance : ( List GlossaryItemId, List GlossaryItemId )
                itemIdsByDistance =
                    DirectedGraph.verticesByDistance itemId relatedItemsGraph
            in
            IncubatingGlossaryItems
                { items
                    | orderedFocusedOn = Just ( itemId, itemIdsByDistance )
                }


toList : IncubatingGlossaryItems -> List GlossaryItemId -> List ( GlossaryItemId, GlossaryItemForHtml )
toList glossaryItems =
    List.filterMap
        (\itemId ->
            glossaryItems
                |> get itemId
                |> Maybe.map (Tuple.pair itemId)
        )


{-| Retrieve the glossary items ordered alphabetically.
-}
orderedAlphabetically : IncubatingGlossaryItems -> List ( GlossaryItemId, GlossaryItemForHtml )
orderedAlphabetically glossaryItems =
    case glossaryItems of
        IncubatingGlossaryItems items ->
            toList glossaryItems items.orderedAlphabetically


{-| Retrieve the glossary items ordered by most mentioned first.
-}
orderedByMostMentionedFirst : IncubatingGlossaryItems -> List ( GlossaryItemId, GlossaryItemForHtml )
orderedByMostMentionedFirst glossaryItems =
    case glossaryItems of
        IncubatingGlossaryItems items ->
            toList glossaryItems items.orderedByMostMentionedFirst


{-| Retrieve the glossary items ordered "focused on" a specific item.
-}
orderedFocusedOn :
    GlossaryItemId
    -> IncubatingGlossaryItems
    ->
        Maybe
            ( List ( GlossaryItemId, GlossaryItemForHtml )
            , List ( GlossaryItemId, GlossaryItemForHtml )
            )
orderedFocusedOn glossaryItemId glossaryItems =
    case glossaryItems of
        IncubatingGlossaryItems items ->
            items.orderedFocusedOn
                |> Maybe.andThen
                    (\( itemIdFocusedOn, ( ids, otherIds ) ) ->
                        if itemIdFocusedOn == glossaryItemId then
                            Just ( toList glossaryItems ids, toList glossaryItems otherIds )

                        else
                            Nothing
                    )
