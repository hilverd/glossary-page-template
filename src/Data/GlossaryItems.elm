module Data.GlossaryItems exposing
    ( GlossaryItems
    , fromList, insertTag, insert, update, remove
    , get, tags, tagsWithDescriptions, tagByIdList, tagIdFromTag, tagFromId, tagDescriptionFromId, disambiguatedPreferredTerm, disambiguatedPreferredTerms, disambiguatedPreferredTermsByAlternativeTerm, itemIdFromDisambiguatedPreferredTermId, disambiguatedPreferredTermFromId, disambiguatedPreferredTermsWhichHaveDefinitions, relatedForWhichItems
    , orderedAlphabetically, orderedByMostMentionedFirst, orderedFocusedOn
    )

{-| The glossary items that make up a glossary.


# Glossary Items

@docs GlossaryItems


# Build

@docs fromList, insertTag, insert, update, remove


# Query

@docs get, tags, tagsWithDescriptions, tagByIdList, tagIdFromTag, tagFromId, tagDescriptionFromId, disambiguatedPreferredTerm, disambiguatedPreferredTerms, disambiguatedPreferredTermsByAlternativeTerm, itemIdFromDisambiguatedPreferredTermId, disambiguatedPreferredTermFromId, disambiguatedPreferredTermsWhichHaveDefinitions, relatedForWhichItems


# Export

@docs orderedAlphabetically, orderedByMostMentionedFirst, orderedFocusedOn

-}

import Data.GlossaryItem as GlossaryItem exposing (GlossaryItem, alternativeTerms, lastUpdatedDateAsIso8601, preferredTerm)
import Data.GlossaryItem.Definition as Definition exposing (Definition)
import Data.GlossaryItem.Tag as Tag exposing (Tag)
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItem.TermId as TermId exposing (TermId)
import Data.GlossaryItemForHtml as GlossaryItemForHtml exposing (GlossaryItemForHtml, disambiguatedPreferredTerm, relatedPreferredTerms)
import Data.GlossaryItemId as GlossaryItemId exposing (GlossaryItemId)
import Data.GlossaryItemIdDict as GlossaryItemIdDict exposing (GlossaryItemIdDict)
import Data.TagDescription exposing (TagDescription)
import Data.TagId as TagId exposing (TagId)
import Data.TagIdDict as TagIdDict exposing (TagIdDict, nextTagId)
import Dict exposing (Dict)
import DirectedGraph exposing (DirectedGraph)
import Extras.Regex
import Maybe
import Regex
import Set exposing (Set)


{-| A set of glossary items.
-}
type GlossaryItems
    = GlossaryItems
        { itemById : GlossaryItemIdDict GlossaryItem
        , tagById : TagIdDict Tag
        , tagIdByRawTag : Dict String TagId
        , tagDescriptionById : TagIdDict TagDescription
        , disambiguationTagIdByItemId : GlossaryItemIdDict (Maybe TagId)
        , normalTagIdsByItemId : GlossaryItemIdDict (List TagId)
        , itemIdsByTagId : TagIdDict (List GlossaryItemId)
        , itemIdByDisambiguatedPreferredTermId : Dict String GlossaryItemId
        , relatedItemIdsById : GlossaryItemIdDict (List GlossaryItemId)
        , orderedAlphabetically : List GlossaryItemId
        , orderedByMostMentionedFirst : List GlossaryItemId
        , orderedFocusedOn : Maybe ( GlossaryItemId, ( List GlossaryItemId, List GlossaryItemId ) )
        , nextItemId : GlossaryItemId
        , nextTagId : TagId
        }


orderAlphabetically : List ( GlossaryItemId, GlossaryItemForHtml ) -> List GlossaryItemId
orderAlphabetically =
    List.sortWith
        (\( _, item1 ) ( _, item2 ) ->
            Term.compareAlphabetically
                (GlossaryItemForHtml.disambiguatedPreferredTerm item1)
                (GlossaryItemForHtml.disambiguatedPreferredTerm item2)
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
                            (item1 |> GlossaryItemForHtml.disambiguatedPreferredTerm |> Term.raw |> String.toUpper)
                            (item2 |> GlossaryItemForHtml.disambiguatedPreferredTerm |> Term.raw |> String.toUpper)

                    GT ->
                        LT
            )
        |> List.map Tuple.first


{-| Convert a list of glossary items for/from HTML into a `GlossaryItems`.
-}
fromList : List ( Tag, TagDescription ) -> List GlossaryItemForHtml -> GlossaryItems
fromList tagsWithDescriptions_ glossaryItemsForHtml =
    let
        indexedGlossaryItemsForHtml : List ( GlossaryItemId, GlossaryItemForHtml )
        indexedGlossaryItemsForHtml =
            List.indexedMap (GlossaryItemId.create >> Tuple.pair) glossaryItemsForHtml

        itemIdByDisambiguatedPreferredTermId_ : Dict String GlossaryItemId
        itemIdByDisambiguatedPreferredTermId_ =
            indexedGlossaryItemsForHtml
                |> List.foldl
                    (\( itemId, item ) ->
                        Dict.insert
                            (GlossaryItemForHtml.disambiguatedPreferredTerm item
                                |> Term.id
                                |> TermId.toString
                            )
                            itemId
                    )
                    Dict.empty

        tagById0 : TagIdDict Tag
        tagById0 =
            tagsWithDescriptions_
                |> List.map Tuple.first
                |> List.indexedMap (TagId.create >> Tuple.pair)
                |> TagIdDict.fromList

        ( itemById, tagById ) =
            indexedGlossaryItemsForHtml
                |> List.foldl
                    (\( itemId, glossaryItemForHtml ) { itemById_, tagById_, allRawTags, nextTagIdInt } ->
                        let
                            glossaryItem : GlossaryItem
                            glossaryItem =
                                GlossaryItem.init
                                    (GlossaryItemForHtml.nonDisambiguatedPreferredTerm glossaryItemForHtml)
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
                    , tagById_ = tagById0
                    , allRawTags = tagById0 |> TagIdDict.values |> List.map Tag.raw |> Set.fromList
                    , nextTagIdInt = tagById0 |> TagIdDict.nextTagId |> TagId.toInt
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

        tagDescriptionById : TagIdDict TagDescription
        tagDescriptionById =
            tagsWithDescriptions_
                |> List.foldl
                    (\( tag, description ) result ->
                        tagIdByRawTag
                            |> Dict.get (Tag.raw tag)
                            |> Maybe.map (\tagId -> TagIdDict.insert tagId description result)
                            |> Maybe.withDefault result
                    )
                    TagIdDict.empty

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
                                        itemIdByDisambiguatedPreferredTermId_
                                )
                            |> GlossaryItemIdDict.insert id
                    )
                    GlossaryItemIdDict.empty

        orderedAlphabetically__ : List GlossaryItemId
        orderedAlphabetically__ =
            orderAlphabetically indexedGlossaryItemsForHtml

        orderedByMostMentionedFirst_ =
            orderByMostMentionedFirst indexedGlossaryItemsForHtml

        nextItemId : GlossaryItemId
        nextItemId =
            itemById
                |> GlossaryItemIdDict.keys
                |> List.map GlossaryItemId.toInt
                |> List.maximum
                |> Maybe.map ((+) 1)
                |> Maybe.withDefault 0
                |> GlossaryItemId.create

        nextTagId : TagId
        nextTagId =
            tagById
                |> TagIdDict.keys
                |> List.map TagId.toInt
                |> List.maximum
                |> Maybe.map ((+) 1)
                |> Maybe.withDefault 0
                |> TagId.create
    in
    GlossaryItems
        { itemById = itemById
        , tagById = tagById
        , tagIdByRawTag = tagIdByRawTag
        , tagDescriptionById = tagDescriptionById
        , disambiguationTagIdByItemId = disambiguationTagIdByItemId
        , normalTagIdsByItemId = normalTagIdsByItemId
        , itemIdsByTagId = itemIdsByTagId_
        , itemIdByDisambiguatedPreferredTermId = itemIdByDisambiguatedPreferredTermId_
        , relatedItemIdsById = relatedItemIdsById
        , orderedAlphabetically = orderedAlphabetically__
        , orderedByMostMentionedFirst = orderedByMostMentionedFirst_
        , orderedFocusedOn = Nothing
        , nextItemId = nextItemId
        , nextTagId = nextTagId
        }


{-| Insert a tag. Does nothing if the tag already exists.
-}
insertTag : Tag -> GlossaryItems -> GlossaryItems
insertTag tag glossaryItems =
    case glossaryItems of
        GlossaryItems items ->
            let
                rawTag =
                    Tag.raw tag
            in
            if Dict.get rawTag items.tagIdByRawTag == Nothing then
                let
                    tagById_ : TagIdDict Tag
                    tagById_ =
                        items.tagById
                            |> TagIdDict.insert items.nextTagId tag

                    tagIdByRawTag_ : Dict String TagId
                    tagIdByRawTag_ =
                        items.tagIdByRawTag
                            |> Dict.insert rawTag items.nextTagId
                in
                GlossaryItems
                    { items
                        | tagById = tagById_
                        , tagIdByRawTag = tagIdByRawTag_
                        , nextTagId = TagId.increment items.nextTagId
                    }

            else
                glossaryItems


{-| Insert an item.
-}
insert : GlossaryItemForHtml -> GlossaryItems -> GlossaryItems
insert item glossaryItems =
    glossaryItems
        |> orderedAlphabetically Nothing
        |> List.map Tuple.second
        |> (::) item
        |> fromList (tagsWithDescriptions glossaryItems)


{-| Update an item.
-}
update : GlossaryItemId -> GlossaryItemForHtml -> GlossaryItems -> GlossaryItems
update itemId item glossaryItems =
    let
        disambiguatedPreferredTerm_ itemId_ =
            if itemId_ == itemId then
                always <| Just <| GlossaryItemForHtml.disambiguatedPreferredTerm item

            else
                disambiguatedPreferredTerm itemId_
    in
    glossaryItems
        |> orderedAlphabetically_ disambiguatedPreferredTerm_ Nothing
        |> List.map
            (\( itemId_, item_ ) ->
                if itemId_ == itemId then
                    item

                else
                    item_
            )
        |> fromList (tagsWithDescriptions glossaryItems)


{-| Remove the item associated with an ID. Does nothing if the ID is not found.
-}
remove : GlossaryItemId -> GlossaryItems -> GlossaryItems
remove itemId glossaryItems =
    glossaryItems
        |> orderedAlphabetically Nothing
        |> List.filterMap
            (\( itemId_, itemForHtml ) ->
                if itemId_ == itemId then
                    Nothing

                else
                    Just itemForHtml
            )
        |> fromList (tagsWithDescriptions glossaryItems)


get_ : (GlossaryItemId -> GlossaryItems -> Maybe Term) -> GlossaryItemId -> GlossaryItems -> Maybe GlossaryItemForHtml
get_ disambiguatedPreferredTerm_ itemId glossaryItems =
    case glossaryItems of
        GlossaryItems items ->
            GlossaryItemIdDict.get itemId items.itemById
                |> Maybe.map
                    (\item ->
                        let
                            preferredTerm : Term
                            preferredTerm =
                                GlossaryItem.preferredTerm item

                            alternativeTerms =
                                GlossaryItem.alternativeTerms item

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
                                GlossaryItem.definition item

                            relatedPreferredTerms : List Term
                            relatedPreferredTerms =
                                items.relatedItemIdsById
                                    |> GlossaryItemIdDict.get itemId
                                    |> Maybe.map
                                        (\relatedItemIds ->
                                            relatedItemIds
                                                |> List.filterMap
                                                    (\relatedItemId ->
                                                        disambiguatedPreferredTerm_ relatedItemId glossaryItems
                                                    )
                                        )
                                    |> Maybe.withDefault []

                            needsUpdating =
                                GlossaryItem.needsUpdating item

                            lastUpdatedDateAsIso8601 =
                                GlossaryItem.lastUpdatedDateAsIso8601 item
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


{-| Get the item associated with an ID. If the ID is not found, return `Nothing`.
-}
get : GlossaryItemId -> GlossaryItems -> Maybe GlossaryItemForHtml
get =
    get_ disambiguatedPreferredTerm


{-| The tags for these glossary items. Tags can exist without being used in any items.
-}
tags : GlossaryItems -> List Tag
tags glossaryItems =
    case glossaryItems of
        GlossaryItems items ->
            TagIdDict.values items.tagById


{-| The tags for these glossary items along with their descriptions. Tags can exist without being used in any items.
-}
tagsWithDescriptions : GlossaryItems -> List ( Tag, TagDescription )
tagsWithDescriptions glossaryItems =
    case glossaryItems of
        GlossaryItems items ->
            items.tagDescriptionById
                |> TagIdDict.toList
                |> List.filterMap
                    (\( id, description ) ->
                        TagIdDict.get id items.tagById
                            |> Maybe.andThen (\tag -> Just ( tag, description ))
                    )


{-| The tags for these glossary items along with their tag IDs.
-}
tagByIdList : GlossaryItems -> List ( TagId, Tag )
tagByIdList glossaryItems =
    case glossaryItems of
        GlossaryItems items ->
            TagIdDict.toList items.tagById


{-| Look up a tag ID from its contents.
-}
tagIdFromTag : Tag -> GlossaryItems -> Maybe TagId
tagIdFromTag tag glossaryItems =
    case glossaryItems of
        GlossaryItems items ->
            Dict.get (Tag.raw tag) items.tagIdByRawTag


{-| Look up a tag from its ID.
-}
tagFromId : TagId -> GlossaryItems -> Maybe Tag
tagFromId tagId glossaryItems =
    case glossaryItems of
        GlossaryItems items ->
            TagIdDict.get tagId items.tagById


{-| Look up a tag's description from its ID.
-}
tagDescriptionFromId : TagId -> GlossaryItems -> Maybe TagDescription
tagDescriptionFromId tagId glossaryItems =
    case glossaryItems of
        GlossaryItems items ->
            TagIdDict.get tagId items.tagDescriptionById


{-| The disambiguated preferred term for the item with the given ID.
-}
disambiguatedPreferredTerm : GlossaryItemId -> GlossaryItems -> Maybe Term
disambiguatedPreferredTerm itemId glossaryItems =
    case glossaryItems of
        GlossaryItems items ->
            let
                maybePreferredTerm : Maybe Term
                maybePreferredTerm =
                    items.itemById
                        |> GlossaryItemIdDict.get itemId
                        |> Maybe.map GlossaryItem.preferredTerm

                disambiguationTagId : Maybe TagId
                disambiguationTagId =
                    items.disambiguationTagIdByItemId
                        |> GlossaryItemIdDict.get itemId
                        |> Maybe.andThen identity

                disambiguationTag : Maybe Tag
                disambiguationTag =
                    disambiguationTagId
                        |> Maybe.andThen
                            (\disambiguationTagId_ ->
                                items.tagById
                                    |> TagIdDict.get disambiguationTagId_
                            )
            in
            maybePreferredTerm
                |> Maybe.map
                    (\preferredTerm_ ->
                        disambiguationTag
                            |> Maybe.map
                                (\disambiguationTag_ ->
                                    preferredTerm_
                                        |> Term.updateRaw
                                            (\raw0 -> raw0 ++ " (" ++ Tag.raw disambiguationTag_ ++ ")")
                                )
                            |> Maybe.withDefault preferredTerm_
                    )


{-| All the disambiguated preferred terms in these glossary items.
-}
disambiguatedPreferredTerms : Maybe TagId -> GlossaryItems -> List Term
disambiguatedPreferredTerms filterByTagId glossaryItems =
    case glossaryItems of
        GlossaryItems items ->
            let
                itemIds : List GlossaryItemId
                itemIds =
                    filterByTagId
                        |> Maybe.map
                            (\tagId ->
                                items.itemIdsByTagId
                                    |> TagIdDict.get tagId
                                    |> Maybe.withDefault []
                            )
                        |> Maybe.withDefault (GlossaryItemIdDict.keys items.itemById)
            in
            itemIds
                |> List.filterMap
                    (\itemId -> disambiguatedPreferredTerm itemId glossaryItems)
                |> List.sortWith Term.compareAlphabetically


{-| Look up the ID of the item whose preferred term has the given ID.
-}
itemIdFromDisambiguatedPreferredTermId : TermId -> GlossaryItems -> Maybe GlossaryItemId
itemIdFromDisambiguatedPreferredTermId termId glossaryItems =
    case glossaryItems of
        GlossaryItems items ->
            items.itemIdByDisambiguatedPreferredTermId
                |> Dict.get (TermId.toString termId)


{-| Look up the disambiguated preferred term of the item whose disambiguated preferred term has the given ID.
-}
disambiguatedPreferredTermFromId : TermId -> GlossaryItems -> Maybe Term
disambiguatedPreferredTermFromId termId glossaryItems =
    case glossaryItems of
        GlossaryItems items ->
            items.itemIdByDisambiguatedPreferredTermId
                |> Dict.get (TermId.toString termId)
                |> Maybe.andThen
                    (\itemId ->
                        glossaryItems
                            |> get itemId
                            |> Maybe.map GlossaryItemForHtml.disambiguatedPreferredTerm
                    )


{-| All of the preferred terms which have a definition.
-}
disambiguatedPreferredTermsWhichHaveDefinitions : Maybe TagId -> GlossaryItems -> List Term
disambiguatedPreferredTermsWhichHaveDefinitions filterByTagId glossaryItems =
    case glossaryItems of
        GlossaryItems items ->
            let
                itemIdIntsSet : Set Int
                itemIdIntsSet =
                    filterByTagId
                        |> Maybe.andThen (\tagId -> TagIdDict.get tagId items.itemIdsByTagId)
                        |> Maybe.withDefault (GlossaryItemIdDict.keys items.itemById)
                        |> List.map GlossaryItemId.toInt
                        |> Set.fromList
            in
            items.itemById
                |> GlossaryItemIdDict.toList
                |> List.filterMap
                    (\( itemId, item ) ->
                        if
                            Set.member (GlossaryItemId.toInt itemId) itemIdIntsSet
                                && GlossaryItem.definition item
                                /= Nothing
                        then
                            disambiguatedPreferredTerm itemId glossaryItems

                        else
                            Nothing
                    )
                |> List.sortWith Term.compareAlphabetically


{-| The IDs of the items that list this item as a related one.
-}
relatedForWhichItems : GlossaryItemId -> GlossaryItems -> List GlossaryItemId
relatedForWhichItems itemId glossaryItems =
    case glossaryItems of
        GlossaryItems items ->
            items.relatedItemIdsById
                |> GlossaryItemIdDict.foldl
                    (\otherItemId relatedItemIds result ->
                        if List.any ((==) itemId) relatedItemIds then
                            otherItemId :: result

                        else
                            result
                    )
                    []


{-| A list of pairs associating each alternative term with the disambiguated preferred terms that it appears together with.
-}
disambiguatedPreferredTermsByAlternativeTerm : Maybe TagId -> GlossaryItems -> List ( Term, List Term )
disambiguatedPreferredTermsByAlternativeTerm filterByTagId glossaryItems =
    case glossaryItems of
        GlossaryItems items ->
            let
                ( alternativeTermByRaw, preferredTermsByRawAlternativeTerm ) =
                    items.itemById
                        |> GlossaryItemIdDict.foldl
                            (\itemId item ( alternativeTermByRaw_, preferredTermsByRawAlternativeTerm_ ) ->
                                let
                                    itemMatchesTag : Bool
                                    itemMatchesTag =
                                        filterByTagId
                                            |> Maybe.map
                                                (\filterByTagId_ ->
                                                    (items.disambiguationTagIdByItemId
                                                        |> GlossaryItemIdDict.get itemId
                                                        |> Maybe.map (\disambiguationTagId -> disambiguationTagId == Just filterByTagId_)
                                                        |> Maybe.withDefault False
                                                    )
                                                        || (items.normalTagIdsByItemId
                                                                |> GlossaryItemIdDict.get itemId
                                                                |> Maybe.map (List.member filterByTagId_)
                                                                |> Maybe.withDefault False
                                                           )
                                                )
                                            |> Maybe.withDefault True
                                in
                                if itemMatchesTag then
                                    case disambiguatedPreferredTerm itemId glossaryItems of
                                        Just disambiguatedPreferredTerm_ ->
                                            item
                                                |> GlossaryItem.alternativeTerms
                                                |> List.foldl
                                                    (\alternativeTerm ( alternativeTermByRaw1, preferredTermsByRawAlternativeTerm1 ) ->
                                                        let
                                                            raw =
                                                                Term.raw alternativeTerm
                                                        in
                                                        ( Dict.insert raw alternativeTerm alternativeTermByRaw1
                                                        , Dict.update raw
                                                            (\preferredTerms_ ->
                                                                preferredTerms_
                                                                    |> Maybe.map ((::) disambiguatedPreferredTerm_)
                                                                    |> Maybe.withDefault [ disambiguatedPreferredTerm_ ]
                                                                    |> Just
                                                            )
                                                            preferredTermsByRawAlternativeTerm1
                                                        )
                                                    )
                                                    ( alternativeTermByRaw_, preferredTermsByRawAlternativeTerm_ )

                                        Nothing ->
                                            ( alternativeTermByRaw_, preferredTermsByRawAlternativeTerm_ )

                                else
                                    ( alternativeTermByRaw_, preferredTermsByRawAlternativeTerm_ )
                            )
                            ( Dict.empty, Dict.empty )
            in
            preferredTermsByRawAlternativeTerm
                |> Dict.foldl
                    (\rawAlternativeTerm preferredTerms_ result ->
                        Dict.get rawAlternativeTerm alternativeTermByRaw
                            |> Maybe.map (\alternativeTerm -> ( alternativeTerm, preferredTerms_ ) :: result)
                            |> Maybe.withDefault result
                    )
                    []


toList_ : (GlossaryItemId -> GlossaryItems -> Maybe Term) -> Maybe TagId -> GlossaryItems -> List GlossaryItemId -> List ( GlossaryItemId, GlossaryItemForHtml )
toList_ disambiguatedPreferredTerm_ filterByTagId glossaryItems =
    case glossaryItems of
        GlossaryItems items ->
            let
                itemIdsMatchingTagFilter : Maybe (Set Int)
                itemIdsMatchingTagFilter =
                    filterByTagId
                        |> Maybe.map
                            (\tagId ->
                                items.itemIdsByTagId
                                    |> TagIdDict.get tagId
                                    |> Maybe.map (List.map GlossaryItemId.toInt >> Set.fromList)
                                    |> Maybe.withDefault Set.empty
                            )
            in
            List.filterMap
                (\itemId ->
                    glossaryItems
                        |> get_ disambiguatedPreferredTerm_ itemId
                        |> Maybe.andThen
                            (\glossaryItemForHtml ->
                                if
                                    itemIdsMatchingTagFilter
                                        |> Maybe.map (Set.member (GlossaryItemId.toInt itemId))
                                        |> Maybe.withDefault True
                                then
                                    Just <| Tuple.pair itemId glossaryItemForHtml

                                else
                                    Nothing
                            )
                )


toList : Maybe TagId -> GlossaryItems -> List GlossaryItemId -> List ( GlossaryItemId, GlossaryItemForHtml )
toList =
    toList_ disambiguatedPreferredTerm


orderedAlphabetically_ : (GlossaryItemId -> GlossaryItems -> Maybe Term) -> Maybe TagId -> GlossaryItems -> List ( GlossaryItemId, GlossaryItemForHtml )
orderedAlphabetically_ disambiguatedPreferredTerm_ filterByTagId glossaryItems =
    case glossaryItems of
        GlossaryItems items ->
            toList_ disambiguatedPreferredTerm_ filterByTagId glossaryItems items.orderedAlphabetically


{-| Retrieve the glossary items ordered alphabetically.
-}
orderedAlphabetically : Maybe TagId -> GlossaryItems -> List ( GlossaryItemId, GlossaryItemForHtml )
orderedAlphabetically =
    orderedAlphabetically_ disambiguatedPreferredTerm


{-| Retrieve the glossary items ordered by most mentioned first.
-}
orderedByMostMentionedFirst : Maybe TagId -> GlossaryItems -> List ( GlossaryItemId, GlossaryItemForHtml )
orderedByMostMentionedFirst filterByTagId glossaryItems =
    case glossaryItems of
        GlossaryItems items ->
            toList filterByTagId glossaryItems items.orderedByMostMentionedFirst


{-| Retrieve the glossary items ordered "focused on" a specific item.
-}
orderedFocusedOn :
    Maybe TagId
    -> GlossaryItemId
    -> GlossaryItems
    ->
        ( List ( GlossaryItemId, GlossaryItemForHtml )
        , List ( GlossaryItemId, GlossaryItemForHtml )
        )
orderedFocusedOn filterByTagId glossaryItemId glossaryItems =
    case glossaryItems of
        GlossaryItems items ->
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
                                            |> Maybe.map (GlossaryItem.definition >> (/=) Nothing)
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

                ( ids, otherIds ) =
                    DirectedGraph.verticesByDistance glossaryItemId relatedItemsGraph
            in
            ( toList filterByTagId glossaryItems ids
            , toList filterByTagId glossaryItems otherIds
            )
