module Data.GlossaryItems exposing
    ( GlossaryItems
    , empty, fromList, applyTagsChanges, insert, update, remove
    , get, tags, describedTags, tagByIdList, tagIdFromTag, tagFromId, tagDescriptionFromId, disambiguatedPreferredTerm, disambiguatedPreferredTerms, disambiguatedPreferredTermsByAlternativeTerm, itemIdFromRawDisambiguatedPreferredTerm, itemIdFromFragmentIdentifier, disambiguatedPreferredTermFromRaw, disambiguatedPreferredTermsWhichHaveDefinitions, relatedForWhichItems, preferredTermsOfItemsListingThisItemAsRelated
    , orderedAlphabetically, orderedByMostMentionedFirst, orderedFocusedOn
    )

{-| The glossary items that make up a glossary.


# Glossary Items

@docs GlossaryItems


# Build

@docs empty, fromList, applyTagsChanges, insert, update, remove


# Query

@docs get, tags, describedTags, tagByIdList, tagIdFromTag, tagFromId, tagDescriptionFromId, disambiguatedPreferredTerm, disambiguatedPreferredTerms, disambiguatedPreferredTermsByAlternativeTerm, itemIdFromRawDisambiguatedPreferredTerm, itemIdFromFragmentIdentifier, disambiguatedPreferredTermFromRaw, disambiguatedPreferredTermsWhichHaveDefinitions, relatedForWhichItems, preferredTermsOfItemsListingThisItemAsRelated


# Export

@docs orderedAlphabetically, orderedByMostMentionedFirst, orderedFocusedOn

-}

import Data.DescribedTag as DescribedTag exposing (DescribedTag)
import Data.GlossaryItem as GlossaryItem exposing (GlossaryItem)
import Data.GlossaryItem.Definition as Definition exposing (Definition)
import Data.GlossaryItem.DisambiguatedTerm as DisambiguatedTerm exposing (DisambiguatedTerm)
import Data.GlossaryItem.RawTerm as RawTerm exposing (RawTerm)
import Data.GlossaryItem.Tag as Tag exposing (Tag)
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItemForUi as GlossaryItemForUi exposing (GlossaryItemForUi)
import Data.GlossaryItemId as GlossaryItemId exposing (GlossaryItemId)
import Data.GlossaryItemIdDict as GlossaryItemIdDict exposing (GlossaryItemIdDict)
import Data.TagDescription exposing (TagDescription)
import Data.TagId exposing (TagId)
import Data.TagIdDict as TagIdDict exposing (TagIdDict)
import Data.TagsChanges as TagsChanges exposing (TagsChanges)
import Dict exposing (Dict)
import DirectedGraph exposing (DirectedGraph)
import DuplicateRejectingDict exposing (DuplicateRejectingDict)
import Extras.Regex
import Internationalisation as I18n
import Maybe
import Regex
import Set exposing (Set)


{-| A set of glossary items.
-}
type GlossaryItems
    = GlossaryItems
        { tagById : TagIdDict Tag
        , tagIdByRawTag : Dict String TagId
        , tagDescriptionById : TagIdDict TagDescription
        , itemById : GlossaryItemIdDict GlossaryItem
        , disambiguationTagIdByItemId : GlossaryItemIdDict (Maybe TagId)
        , normalTagIdsByItemId : GlossaryItemIdDict (List TagId)
        , itemIdsByTagId : TagIdDict (List GlossaryItemId)
        , itemIdByFragmentIdentifierForRawDisambiguatedPreferredTerm : Dict String GlossaryItemId
        , relatedItemIdsById : GlossaryItemIdDict (List GlossaryItemId)
        , orderedAlphabetically : List GlossaryItemId
        , orderedByMostMentionedFirst : List GlossaryItemId
        , orderedFocusedOn : Maybe ( GlossaryItemId, ( List GlossaryItemId, List GlossaryItemId ) )
        }


{-| The empty set of glossary items.
-}
empty : GlossaryItems
empty =
    GlossaryItems
        { tagById = TagIdDict.empty
        , tagIdByRawTag = Dict.empty
        , tagDescriptionById = TagIdDict.empty
        , itemById = GlossaryItemIdDict.empty
        , disambiguationTagIdByItemId = GlossaryItemIdDict.empty
        , normalTagIdsByItemId = GlossaryItemIdDict.empty
        , itemIdsByTagId = TagIdDict.empty
        , itemIdByFragmentIdentifierForRawDisambiguatedPreferredTerm = Dict.empty
        , relatedItemIdsById = GlossaryItemIdDict.empty
        , orderedAlphabetically = []
        , orderedByMostMentionedFirst = []
        , orderedFocusedOn = Nothing
        }


{-| Convert a list of glossary items for/from HTML into a `GlossaryItems`.
-}
fromList : List DescribedTag -> List GlossaryItemForUi -> Result String GlossaryItems
fromList describedTags_ glossaryItemsForHtml =
    let
        tagById : TagIdDict Tag
        tagById =
            describedTags_
                |> List.map
                    (\describedTag ->
                        ( DescribedTag.id describedTag, DescribedTag.tag describedTag )
                    )
                |> TagIdDict.fromList

        tagIdByRawTag : DuplicateRejectingDict String TagId
        tagIdByRawTag =
            TagIdDict.foldl
                (\tagId tag ->
                    DuplicateRejectingDict.insert (Tag.raw tag) tagId
                )
                DuplicateRejectingDict.empty
                tagById

        tagDescriptionById : TagIdDict TagDescription
        tagDescriptionById =
            describedTags_
                |> List.foldl
                    (\describedTag result ->
                        tagIdByRawTag
                            |> DuplicateRejectingDict.get (describedTag |> DescribedTag.tag |> Tag.raw)
                            |> Maybe.map (\tagId -> TagIdDict.insert tagId (DescribedTag.description describedTag) result)
                            |> Maybe.withDefault result
                    )
                    TagIdDict.empty

        itemIdByFragmentIdentifierForRawDisambiguatedPreferredTerm_ : DuplicateRejectingDict String GlossaryItemId
        itemIdByFragmentIdentifierForRawDisambiguatedPreferredTerm_ =
            glossaryItemsForHtml
                |> List.foldl
                    (\item ->
                        DuplicateRejectingDict.insert
                            (item
                                |> GlossaryItemForUi.disambiguatedPreferredTerm
                                |> DisambiguatedTerm.toTerm
                                |> Term.id
                            )
                            (GlossaryItemForUi.id item)
                    )
                    DuplicateRejectingDict.empty

        itemById =
            glossaryItemsForHtml
                |> List.foldl
                    (\glossaryItemForUi itemById_ ->
                        let
                            glossaryItem : GlossaryItem
                            glossaryItem =
                                GlossaryItem.init
                                    (GlossaryItemForUi.id glossaryItemForUi)
                                    (GlossaryItemForUi.nonDisambiguatedPreferredTerm glossaryItemForUi)
                                    (GlossaryItemForUi.alternativeTerms glossaryItemForUi)
                                    (GlossaryItemForUi.definition glossaryItemForUi)
                                    (GlossaryItemForUi.needsUpdating glossaryItemForUi)
                                    (GlossaryItemForUi.lastUpdatedDateAsIso8601 glossaryItemForUi)
                                    (GlossaryItemForUi.lastUpdatedByName glossaryItemForUi)
                                    (GlossaryItemForUi.lastUpdatedByEmailAddress glossaryItemForUi)

                            itemById1 =
                                GlossaryItemIdDict.insert (GlossaryItem.id glossaryItem) glossaryItem itemById_
                        in
                        itemById1
                    )
                    GlossaryItemIdDict.empty

        ( disambiguationTagIdByItemId, normalTagIdsByItemId ) =
            glossaryItemsForHtml
                |> List.foldl
                    (\item ( disambiguationTagByItemId_, normalTagsByItemId_ ) ->
                        let
                            id =
                                GlossaryItemForUi.id item
                        in
                        ( GlossaryItemIdDict.insert id
                            (item
                                |> GlossaryItemForUi.disambiguationTag
                                |> Maybe.andThen
                                    (\disambiguationTag ->
                                        DuplicateRejectingDict.get (Tag.raw disambiguationTag) tagIdByRawTag
                                    )
                            )
                            disambiguationTagByItemId_
                        , GlossaryItemIdDict.insert id
                            (item
                                |> GlossaryItemForUi.normalTags
                                |> List.filterMap
                                    (\tag ->
                                        DuplicateRejectingDict.get (Tag.raw tag) tagIdByRawTag
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
            glossaryItemsForHtml
                |> List.foldl
                    (\item ->
                        item
                            |> GlossaryItemForUi.relatedPreferredTerms
                            |> List.filterMap
                                (\relatedPreferredTerm ->
                                    DuplicateRejectingDict.get
                                        (relatedPreferredTerm |> DisambiguatedTerm.toTerm |> Term.id)
                                        itemIdByFragmentIdentifierForRawDisambiguatedPreferredTerm_
                                )
                            |> GlossaryItemIdDict.insert (GlossaryItemForUi.id item)
                    )
                    GlossaryItemIdDict.empty

        sortedNormalTagIdsByItemId : GlossaryItemIdDict (List TagId)
        sortedNormalTagIdsByItemId =
            normalTagIdsByItemId
                |> GlossaryItemIdDict.map
                    (\_ normalTags ->
                        normalTags
                            |> List.sortWith
                                (\tagId1 tagId2 ->
                                    Maybe.map2
                                        Tag.compareAlphabetically
                                        (TagIdDict.get tagId1 tagById)
                                        (TagIdDict.get tagId2 tagById)
                                        |> Maybe.withDefault EQ
                                )
                    )

        itemIdByFragmentIdentifierForRawDisambiguatedPreferredTermResult : Result String (Dict String GlossaryItemId)
        itemIdByFragmentIdentifierForRawDisambiguatedPreferredTermResult =
            itemIdByFragmentIdentifierForRawDisambiguatedPreferredTerm_
                |> DuplicateRejectingDict.toResult
                |> Result.mapError
                    (\{ value1 } ->
                        let
                            preferredTerm1 : Maybe Term
                            preferredTerm1 =
                                itemById
                                    |> GlossaryItemIdDict.get value1
                                    |> Maybe.map GlossaryItem.preferredTerm

                            disambiguationTag1 : Maybe Tag
                            disambiguationTag1 =
                                disambiguationTagIdByItemId
                                    |> GlossaryItemIdDict.get value1
                                    |> Maybe.andThen
                                        (Maybe.andThen
                                            (\tagId -> TagIdDict.get tagId tagById)
                                        )

                            disambiguatedPreferredTerm1 : Maybe DisambiguatedTerm
                            disambiguatedPreferredTerm1 =
                                preferredTerm1
                                    |> Maybe.map
                                        (\preferredTerm1_ ->
                                            disambiguationTag1
                                                |> Maybe.map
                                                    (\tag1 ->
                                                        GlossaryItemForUi.disambiguatedTerm tag1 preferredTerm1_
                                                    )
                                                |> Maybe.withDefault (DisambiguatedTerm.fromTerm preferredTerm1_)
                                        )
                        in
                        Maybe.map
                            (DisambiguatedTerm.toTerm
                                >> Term.id
                                >> I18n.thereAreMultipleItemsWithDisambiguatedPreferredTerm
                            )
                            disambiguatedPreferredTerm1
                            |> -- This should never happen
                               Maybe.withDefault I18n.thereAreMultipleItemsWithTheSameDisambiguatedPreferredTerm
                    )

        tagIdByRawTagResult : Result String (Dict String TagId)
        tagIdByRawTagResult =
            tagIdByRawTag
                |> DuplicateRejectingDict.toResult
                |> Result.mapError (\{ key } -> I18n.tagAppearsMultipleTimes key)
    in
    Result.map2
        (\itemIdByFragmentIdentifierForRawDisambiguatedPreferredTerm1 tagIdByRawTag_ ->
            let
                orderedAlphabetically__ : List GlossaryItemId
                orderedAlphabetically__ =
                    orderAlphabetically glossaryItemsForHtml

                orderedByMostMentionedFirst_ =
                    orderByMostMentionedFirst glossaryItemsForHtml
            in
            GlossaryItems
                { tagById = tagById
                , tagIdByRawTag = tagIdByRawTag_
                , tagDescriptionById = tagDescriptionById
                , itemById = itemById
                , disambiguationTagIdByItemId = disambiguationTagIdByItemId
                , normalTagIdsByItemId = sortedNormalTagIdsByItemId
                , itemIdsByTagId = itemIdsByTagId_
                , itemIdByFragmentIdentifierForRawDisambiguatedPreferredTerm = itemIdByFragmentIdentifierForRawDisambiguatedPreferredTerm1
                , relatedItemIdsById = relatedItemIdsById
                , orderedAlphabetically = orderedAlphabetically__
                , orderedByMostMentionedFirst = orderedByMostMentionedFirst_
                , orderedFocusedOn = Nothing
                }
        )
        itemIdByFragmentIdentifierForRawDisambiguatedPreferredTermResult
        tagIdByRawTagResult


{-| Apply a set of tags changes.
-}
applyTagsChanges : TagsChanges -> GlossaryItems -> Result String GlossaryItems
applyTagsChanges tagsChanges glossaryItems =
    let
        resultBeforeValidation =
            tagsChanges
                |> TagsChanges.toList
                |> List.foldl
                    (\tagsChange result ->
                        case ( result, tagsChange ) of
                            ( GlossaryItems items, TagsChanges.Insertion describedTag ) ->
                                let
                                    id : TagId
                                    id =
                                        DescribedTag.id describedTag

                                    tag : Tag
                                    tag =
                                        DescribedTag.tag describedTag

                                    tagDescription : TagDescription
                                    tagDescription =
                                        DescribedTag.description describedTag
                                in
                                GlossaryItems
                                    { items
                                        | tagById = TagIdDict.insert id tag items.tagById
                                        , tagIdByRawTag = Dict.insert (Tag.raw tag) id items.tagIdByRawTag
                                        , tagDescriptionById = TagIdDict.insert id tagDescription items.tagDescriptionById
                                    }

                            ( GlossaryItems items, TagsChanges.Update tagId describedTag ) ->
                                let
                                    tag =
                                        DescribedTag.tag describedTag

                                    tagDescription =
                                        DescribedTag.description describedTag
                                in
                                GlossaryItems
                                    { items
                                        | tagById = TagIdDict.insert tagId tag items.tagById
                                        , tagIdByRawTag = Dict.insert (Tag.raw tag) tagId items.tagIdByRawTag
                                        , tagDescriptionById = TagIdDict.insert tagId tagDescription items.tagDescriptionById
                                    }

                            ( GlossaryItems items, TagsChanges.Removal tagId ) ->
                                GlossaryItems
                                    { items
                                        | tagById = TagIdDict.remove tagId items.tagById
                                        , tagIdByRawTag = Dict.filter (always <| (/=) tagId) items.tagIdByRawTag
                                        , tagDescriptionById = TagIdDict.remove tagId items.tagDescriptionById
                                    }
                    )
                    glossaryItems
    in
    resultBeforeValidation
        |> orderedAlphabetically Nothing
        |> List.map Tuple.second
        |> fromList (describedTags resultBeforeValidation)


{-| Insert an item, returning the ID of the new item.
-}
insert : GlossaryItemForUi -> GlossaryItems -> Result String ( GlossaryItemId, GlossaryItems )
insert item glossaryItems =
    let
        itemsAfterInserting : Result String GlossaryItems
        itemsAfterInserting =
            glossaryItems
                |> orderedAlphabetically Nothing
                |> List.map Tuple.second
                |> (::) item
                |> fromList (describedTags glossaryItems)

        insertedItemId : GlossaryItemId
        insertedItemId =
            GlossaryItemForUi.id item
    in
    itemsAfterInserting
        |> Result.map (Tuple.pair insertedItemId)


{-| Update an item. Do nothing if there is no item with the given ID.
-}
update : GlossaryItemId -> GlossaryItemForUi -> GlossaryItems -> Result String GlossaryItems
update itemId item glossaryItems =
    let
        disambiguatedPreferredTerm_ itemId_ =
            if itemId_ == itemId then
                always <| Just <| GlossaryItemForUi.disambiguatedPreferredTerm item

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
        |> fromList (describedTags glossaryItems)


{-| Remove the item associated with an ID. Do nothing if the ID is not found.
-}
remove : GlossaryItemId -> GlossaryItems -> Result String GlossaryItems
remove itemId glossaryItems =
    glossaryItems
        |> orderedAlphabetically Nothing
        |> List.filterMap
            (\( itemId_, itemForUi ) ->
                if itemId_ == itemId then
                    Nothing

                else
                    Just itemForUi
            )
        |> fromList (describedTags glossaryItems)


relatedPreferredTerms_ : (GlossaryItemId -> GlossaryItems -> Maybe DisambiguatedTerm) -> Maybe TagId -> GlossaryItemId -> GlossaryItems -> Maybe (List DisambiguatedTerm)
relatedPreferredTerms_ disambiguatedPreferredTerm_ filterByTagId itemId ((GlossaryItems items) as glossaryItems) =
    items.relatedItemIdsById
        |> GlossaryItemIdDict.get itemId
        |> Maybe.map
            (\relatedItemIds ->
                relatedItemIds
                    |> List.filterMap
                        (\relatedItemId ->
                            let
                                relatedItemMatchesTagBeingFilteredBy =
                                    filterByTagId
                                        |> Maybe.map
                                            (\filterByTagId_ ->
                                                (items.disambiguationTagIdByItemId
                                                    |> GlossaryItemIdDict.get relatedItemId
                                                    |> (==) (Just <| Just filterByTagId_)
                                                )
                                                    || (items.normalTagIdsByItemId
                                                            |> GlossaryItemIdDict.get relatedItemId
                                                            |> Maybe.map (List.member filterByTagId_)
                                                            |> Maybe.withDefault False
                                                       )
                                            )
                                        |> Maybe.withDefault True
                            in
                            if relatedItemMatchesTagBeingFilteredBy then
                                disambiguatedPreferredTerm_ relatedItemId glossaryItems

                            else
                                Nothing
                        )
            )


get_ : (GlossaryItemId -> GlossaryItems -> Maybe DisambiguatedTerm) -> Maybe TagId -> GlossaryItemId -> GlossaryItems -> Maybe GlossaryItemForUi
get_ disambiguatedPreferredTerm_ filterByTagId itemId ((GlossaryItems items) as glossaryItems) =
    GlossaryItemIdDict.get itemId items.itemById
        |> Maybe.map
            (\item ->
                let
                    id : GlossaryItemId
                    id =
                        GlossaryItem.id item

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

                    relatedPreferredTerms : List DisambiguatedTerm
                    relatedPreferredTerms =
                        glossaryItems
                            |> relatedPreferredTerms_ disambiguatedPreferredTerm_ filterByTagId itemId
                            |> Maybe.withDefault []

                    needsUpdating =
                        GlossaryItem.needsUpdating item

                    lastUpdatedDateAsIso8601 =
                        GlossaryItem.lastUpdatedDateAsIso8601 item

                    lastUpdatedByName =
                        GlossaryItem.lastUpdatedByName item

                    lastUpdatedByEmailAddress =
                        GlossaryItem.lastUpdatedByEmailAddress item
                in
                GlossaryItemForUi.create
                    id
                    preferredTerm
                    alternativeTerms
                    disambiguationTag
                    normalTags
                    definition
                    relatedPreferredTerms
                    needsUpdating
                    lastUpdatedDateAsIso8601
                    lastUpdatedByName
                    lastUpdatedByEmailAddress
            )


{-| Get the item associated with an ID. If the ID is not found, return `Nothing`.
-}
get : GlossaryItemId -> GlossaryItems -> Maybe GlossaryItemForUi
get =
    get_ disambiguatedPreferredTerm Nothing


{-| The tags for these glossary items. Tags can exist without being used in any items.
-}
tags : GlossaryItems -> List Tag
tags (GlossaryItems items) =
    items.tagById
        |> TagIdDict.values
        |> List.sortWith Tag.compareAlphabetically


{-| The tags for these glossary items along with their IDs and descriptions.
Tags can exist without being used in any items.
-}
describedTags : GlossaryItems -> List DescribedTag
describedTags (GlossaryItems items) =
    items.tagDescriptionById
        |> TagIdDict.toList
        |> List.filterMap
            (\( id, description ) ->
                TagIdDict.get id items.tagById
                    |> Maybe.map
                        (\tag -> DescribedTag.create id tag description)
            )
        |> List.sortWith
            (\describedTag1 describedTag2 ->
                Tag.compareAlphabetically
                    (DescribedTag.tag describedTag1)
                    (DescribedTag.tag describedTag2)
            )


{-| The tags for these glossary items along with their tag IDs.
-}
tagByIdList : GlossaryItems -> List ( TagId, Tag )
tagByIdList (GlossaryItems items) =
    TagIdDict.toList items.tagById


{-| Look up a tag ID from its contents.
-}
tagIdFromTag : Tag -> GlossaryItems -> Maybe TagId
tagIdFromTag tag (GlossaryItems items) =
    Dict.get (Tag.raw tag) items.tagIdByRawTag


{-| Look up a tag from its ID.
-}
tagFromId : TagId -> GlossaryItems -> Maybe Tag
tagFromId tagId (GlossaryItems items) =
    TagIdDict.get tagId items.tagById


{-| Look up a tag's description from its ID.
-}
tagDescriptionFromId : TagId -> GlossaryItems -> Maybe TagDescription
tagDescriptionFromId tagId (GlossaryItems items) =
    TagIdDict.get tagId items.tagDescriptionById


{-| The disambiguated preferred term for the item with the given ID.
-}
disambiguatedPreferredTerm : GlossaryItemId -> GlossaryItems -> Maybe DisambiguatedTerm
disambiguatedPreferredTerm itemId (GlossaryItems items) =
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
                            GlossaryItemForUi.disambiguatedTerm disambiguationTag_ preferredTerm_
                        )
                    |> Maybe.withDefault (DisambiguatedTerm.fromTerm preferredTerm_)
            )


{-| All the disambiguated preferred terms in these glossary items.
-}
disambiguatedPreferredTerms : Maybe TagId -> GlossaryItems -> List ( GlossaryItemId, DisambiguatedTerm )
disambiguatedPreferredTerms filterByTagId ((GlossaryItems items) as glossaryItems) =
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

        compareDisambiguatedTerms : ( GlossaryItemId, DisambiguatedTerm ) -> ( GlossaryItemId, DisambiguatedTerm ) -> Order
        compareDisambiguatedTerms ( _, t1 ) ( _, t2 ) =
            Term.compareAlphabetically
                (DisambiguatedTerm.toTerm t1)
                (DisambiguatedTerm.toTerm t2)
    in
    itemIds
        |> List.filterMap
            (\itemId -> glossaryItems |> disambiguatedPreferredTerm itemId |> Maybe.map (Tuple.pair itemId))
        |> List.sortWith compareDisambiguatedTerms


{-| Look up the ID of the item with the given raw disambiguated preferred term.
-}
itemIdFromRawDisambiguatedPreferredTerm : RawTerm -> GlossaryItems -> Maybe GlossaryItemId
itemIdFromRawDisambiguatedPreferredTerm rawTerm (GlossaryItems items) =
    items.itemIdByFragmentIdentifierForRawDisambiguatedPreferredTerm
        |> Dict.get (rawTerm |> RawTerm.toString |> String.replace " " "_")


{-| Look up the ID of the item with the given fragment identifier.
-}
itemIdFromFragmentIdentifier : String -> GlossaryItems -> Maybe GlossaryItemId
itemIdFromFragmentIdentifier fragmentIdentifier (GlossaryItems items) =
    Dict.get
        fragmentIdentifier
        items.itemIdByFragmentIdentifierForRawDisambiguatedPreferredTerm


{-| Look up the disambiguated preferred term of the item with the given raw disambiguated preferred term.
-}
disambiguatedPreferredTermFromRaw : RawTerm -> GlossaryItems -> Maybe DisambiguatedTerm
disambiguatedPreferredTermFromRaw rawTerm ((GlossaryItems items) as glossaryItems) =
    items.itemIdByFragmentIdentifierForRawDisambiguatedPreferredTerm
        |> Dict.get (rawTerm |> RawTerm.toString |> String.replace " " "_")
        |> Maybe.andThen
            (\itemId ->
                glossaryItems
                    |> get itemId
                    |> Maybe.map GlossaryItemForUi.disambiguatedPreferredTerm
            )


{-| All of the disambiguated preferred terms which have a definition.
-}
disambiguatedPreferredTermsWhichHaveDefinitions : Maybe TagId -> GlossaryItems -> List DisambiguatedTerm
disambiguatedPreferredTermsWhichHaveDefinitions filterByTagId ((GlossaryItems items) as glossaryItems) =
    let
        itemIdsSet : Set String
        itemIdsSet =
            filterByTagId
                |> Maybe.andThen (\tagId -> TagIdDict.get tagId items.itemIdsByTagId)
                |> Maybe.withDefault (GlossaryItemIdDict.keys items.itemById)
                |> List.map GlossaryItemId.toString
                |> Set.fromList
    in
    items.itemById
        |> GlossaryItemIdDict.toList
        |> List.filterMap
            (\( itemId, item ) ->
                if
                    Set.member (GlossaryItemId.toString itemId) itemIdsSet
                        && GlossaryItem.definition item
                        /= Nothing
                then
                    disambiguatedPreferredTerm itemId glossaryItems

                else
                    Nothing
            )
        |> List.sortWith DisambiguatedTerm.compareAlphabetically


{-| The IDs of the items that list this item as a related one.
-}
relatedForWhichItems : GlossaryItemId -> GlossaryItems -> List GlossaryItemId
relatedForWhichItems itemId (GlossaryItems items) =
    items.relatedItemIdsById
        |> GlossaryItemIdDict.foldl
            (\otherItemId relatedItemIds result ->
                if List.any ((==) itemId) relatedItemIds then
                    otherItemId :: result

                else
                    result
            )
            []


{-| The disambiguated preferred terms of the items that list this one as a related item.
-}
preferredTermsOfItemsListingThisItemAsRelated : GlossaryItemId -> GlossaryItems -> List DisambiguatedTerm
preferredTermsOfItemsListingThisItemAsRelated id items =
    items
        |> relatedForWhichItems id
        |> List.filterMap (\id_ -> disambiguatedPreferredTerm id_ items)


{-| A list of pairs associating each alternative term with the disambiguated preferred terms that it appears together with.
-}
disambiguatedPreferredTermsByAlternativeTerm : Maybe TagId -> GlossaryItems -> List ( Term, List ( GlossaryItemId, DisambiguatedTerm ) )
disambiguatedPreferredTermsByAlternativeTerm filterByTagId ((GlossaryItems items) as glossaryItems) =
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
                                                        alternativeTerm
                                                            |> Term.raw
                                                            |> RawTerm.toString
                                                in
                                                ( Dict.insert raw alternativeTerm alternativeTermByRaw1
                                                , Dict.update raw
                                                    (\preferredTerms_ ->
                                                        preferredTerms_
                                                            |> Maybe.map ((::) ( itemId, disambiguatedPreferredTerm_ ))
                                                            |> Maybe.withDefault [ ( itemId, disambiguatedPreferredTerm_ ) ]
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


toList_ : (GlossaryItemId -> GlossaryItems -> Maybe DisambiguatedTerm) -> Maybe TagId -> GlossaryItems -> List GlossaryItemId -> List ( GlossaryItemId, GlossaryItemForUi )
toList_ disambiguatedPreferredTerm_ filterByTagId ((GlossaryItems items) as glossaryItems) =
    let
        itemIdsMatchingTagFilter : Maybe (Set String)
        itemIdsMatchingTagFilter =
            filterByTagId
                |> Maybe.map
                    (\tagId ->
                        items.itemIdsByTagId
                            |> TagIdDict.get tagId
                            |> Maybe.map (List.map GlossaryItemId.toString >> Set.fromList)
                            |> Maybe.withDefault Set.empty
                    )
    in
    List.filterMap
        (\itemId ->
            if
                itemIdsMatchingTagFilter
                    |> Maybe.map (Set.member <| GlossaryItemId.toString itemId)
                    |> Maybe.withDefault True
            then
                glossaryItems
                    |> get_ disambiguatedPreferredTerm_ filterByTagId itemId
                    |> Maybe.andThen (Just << Tuple.pair itemId)

            else
                Nothing
        )


toList : Maybe TagId -> GlossaryItems -> List GlossaryItemId -> List ( GlossaryItemId, GlossaryItemForUi )
toList =
    toList_ disambiguatedPreferredTerm


orderedAlphabetically_ : (GlossaryItemId -> GlossaryItems -> Maybe DisambiguatedTerm) -> Maybe TagId -> GlossaryItems -> List ( GlossaryItemId, GlossaryItemForUi )
orderedAlphabetically_ disambiguatedPreferredTerm_ filterByTagId ((GlossaryItems items) as glossaryItems) =
    toList_ disambiguatedPreferredTerm_ filterByTagId glossaryItems items.orderedAlphabetically


{-| Retrieve the glossary items ordered alphabetically.
-}
orderedAlphabetically : Maybe TagId -> GlossaryItems -> List ( GlossaryItemId, GlossaryItemForUi )
orderedAlphabetically =
    orderedAlphabetically_ disambiguatedPreferredTerm


{-| Retrieve the glossary items ordered by most mentioned first.
-}
orderedByMostMentionedFirst : Maybe TagId -> GlossaryItems -> List ( GlossaryItemId, GlossaryItemForUi )
orderedByMostMentionedFirst filterByTagId ((GlossaryItems items) as glossaryItems) =
    toList filterByTagId glossaryItems items.orderedByMostMentionedFirst


{-| Retrieve the glossary items ordered "focused on" a specific item.
-}
orderedFocusedOn :
    Maybe TagId
    -> GlossaryItemId
    -> GlossaryItems
    ->
        ( List ( GlossaryItemId, GlossaryItemForUi )
        , List ( GlossaryItemId, GlossaryItemForUi )
        )
orderedFocusedOn filterByTagId glossaryItemId ((GlossaryItems items) as glossaryItems) =
    let
        itemIdsGraph : DirectedGraph GlossaryItemId
        itemIdsGraph =
            items.itemById
                |> GlossaryItemIdDict.keys
                |> List.foldl
                    DirectedGraph.insertVertex
                    (DirectedGraph.empty GlossaryItemId.toString GlossaryItemId.create)

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


orderAlphabetically : List GlossaryItemForUi -> List GlossaryItemId
orderAlphabetically =
    List.sortWith
        (\item1 item2 ->
            Term.compareAlphabetically
                (item1 |> GlossaryItemForUi.disambiguatedPreferredTerm |> DisambiguatedTerm.toTerm)
                (item2 |> GlossaryItemForUi.disambiguatedPreferredTerm |> DisambiguatedTerm.toTerm)
        )
        >> List.map GlossaryItemForUi.id


orderByMostMentionedFirst : List GlossaryItemForUi -> List GlossaryItemId
orderByMostMentionedFirst glossaryItemsForHtml =
    let
        -- Maps a term to a score based on whether or not it occurs in glossaryItem.
        -- This is done in a primitive way. A more sophisticated solution could use stemming
        -- or other techniques.
        termScoreInItem : Term -> GlossaryItemForUi -> Int
        termScoreInItem term glossaryItem =
            let
                termAsWord : Regex.Regex
                termAsWord =
                    ("\\b" ++ Extras.Regex.escapeStringForUseInRegex (term |> Term.raw |> RawTerm.toString) ++ "\\b")
                        |> Regex.fromString
                        |> Maybe.withDefault Regex.never

                score : Int
                score =
                    (glossaryItem
                        |> GlossaryItemForUi.allTerms
                        |> List.map (Term.raw >> RawTerm.toString >> Regex.find termAsWord >> List.length)
                        |> List.sum
                    )
                        + (glossaryItem
                            |> GlossaryItemForUi.definition
                            |> Maybe.map (Definition.raw >> Regex.find termAsWord >> List.length)
                            |> Maybe.withDefault 0
                          )
                        + (glossaryItem
                            |> GlossaryItemForUi.relatedPreferredTerms
                            |> List.map (DisambiguatedTerm.toTerm >> Term.raw >> RawTerm.toString)
                            |> List.map (Regex.find termAsWord >> List.length)
                            |> List.sum
                          )
            in
            if score > 0 then
                1

            else
                0

        -- Maps a term to a score based on how often it occurs in glossaryItemsForHtml.
        termScore : Term -> GlossaryItemId -> Int
        termScore term exceptId =
            glossaryItemsForHtml
                |> List.foldl
                    (\glossaryItem result ->
                        result
                            + (if GlossaryItemForUi.id glossaryItem == exceptId then
                                0

                               else
                                termScoreInItem term glossaryItem
                              )
                    )
                    0

        termBodyScores : Dict String Int
        termBodyScores =
            glossaryItemsForHtml
                |> List.concatMap
                    (\glossaryItem ->
                        glossaryItem
                            |> GlossaryItemForUi.allTerms
                            |> List.map (Tuple.pair <| GlossaryItemForUi.id glossaryItem)
                    )
                |> List.foldl
                    (\( glossaryItemId, term ) result ->
                        Dict.insert
                            (term |> Term.raw |> RawTerm.toString)
                            (termScore term glossaryItemId)
                            result
                    )
                    Dict.empty
    in
    glossaryItemsForHtml
        |> List.sortWith
            (\item1 item2 ->
                let
                    itemScore : GlossaryItemForUi -> Int
                    itemScore =
                        GlossaryItemForUi.allTerms
                            >> List.map
                                (\term ->
                                    termBodyScores
                                        |> Dict.get (term |> Term.raw |> RawTerm.toString)
                                        |> Maybe.withDefault 0
                                )
                            >> List.sum
                in
                case compare (itemScore item1) (itemScore item2) of
                    LT ->
                        GT

                    EQ ->
                        compare
                            (item1 |> GlossaryItemForUi.disambiguatedPreferredTerm |> DisambiguatedTerm.toTerm |> Term.raw |> RawTerm.toString |> String.toUpper)
                            (item2 |> GlossaryItemForUi.disambiguatedPreferredTerm |> DisambiguatedTerm.toTerm |> Term.raw |> RawTerm.toString |> String.toUpper)

                    GT ->
                        LT
            )
        |> List.map GlossaryItemForUi.id
