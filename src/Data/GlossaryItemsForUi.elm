module Data.GlossaryItemsForUi exposing
    ( GlossaryItemsForUi
    , empty, fromList
    , isEmpty, get, getWithRelatedTermsFilteredByTagId, relatedItems, startingItem, tags, describedTags, tagByIdList, tagIdFromTag, tagFromId, tagDescriptionFromId, disambiguatedPreferredTerm, disambiguatedPreferredTerms, itemOutlines, disambiguatedPreferredTermsByAlternativeTerm, itemIdFromRawDisambiguatedPreferredTerm, itemIdFromFragmentIdentifier, disambiguatedPreferredTermFromRaw, disambiguatedPreferredTermsWhichHaveDefinitions, relatedForWhichItems, preferredTermsOfItemsListingThisItemAsRelated, outlinesOfItemsListingThisItemAsRelated, itemWhoseDisambiguatedPreferredTermMatchesTag
    , orderedAlphabetically, orderedByMostMentionedFirst, orderedFocusedOn
    )

{-| The glossary items that make up a glossary, ready to be used in a view function.


# Glossary Items for the UI

@docs GlossaryItemsForUi


# Build

@docs empty, fromList


# Query

@docs isEmpty, get, getWithRelatedTermsFilteredByTagId, relatedItems, startingItem, tags, describedTags, tagByIdList, tagIdFromTag, tagFromId, tagDescriptionFromId, disambiguatedPreferredTerm, disambiguatedPreferredTerms, itemOutlines, disambiguatedPreferredTermsByAlternativeTerm, itemIdFromRawDisambiguatedPreferredTerm, itemIdFromFragmentIdentifier, disambiguatedPreferredTermFromRaw, disambiguatedPreferredTermsWhichHaveDefinitions, relatedForWhichItems, preferredTermsOfItemsListingThisItemAsRelated, outlinesOfItemsListingThisItemAsRelated, itemWhoseDisambiguatedPreferredTermMatchesTag


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
import Data.GlossaryItemOutline exposing (GlossaryItemOutline)
import Data.GlossaryTags as GlossaryTags exposing (GlossaryTags)
import Data.TagDescription exposing (TagDescription)
import Data.TagId exposing (TagId)
import Data.TagIdDict as TagIdDict exposing (TagIdDict)
import Dict exposing (Dict)
import DirectedGraph exposing (DirectedGraph)
import DuplicateRejectingDict exposing (DuplicateRejectingDict)
import Extras.Regex
import Extras.String
import Internationalisation as I18n
import Maybe
import Regex
import Set exposing (Set)


{-| A set of glossary items.
-}
type GlossaryItemsForUi
    = GlossaryItemsForUi
        { tags : GlossaryTags
        , itemById : GlossaryItemIdDict GlossaryItem
        , startingItemId : Maybe GlossaryItemId
        , disambiguationTagIdByItemId : GlossaryItemIdDict (Maybe TagId)
        , normalTagIdsByItemId : GlossaryItemIdDict (List TagId)
        , itemIdsByTagId : TagIdDict (List GlossaryItemId)
        , itemIdByFragmentIdentifierForRawDisambiguatedPreferredTerm : Dict String GlossaryItemId
        , relatedItemIdsById : GlossaryItemIdDict (List GlossaryItemId)
        , disambiguatedPreferredTermByItemId : GlossaryItemIdDict (Maybe DisambiguatedTerm)
        , orderedAlphabetically : List GlossaryItemId
        , orderedByMostMentionedFirst : List GlossaryItemId
        }


{-| The empty set of glossary items.
-}
empty : GlossaryItemsForUi
empty =
    GlossaryItemsForUi
        { tags = GlossaryTags.empty
        , itemById = GlossaryItemIdDict.empty
        , startingItemId = Nothing
        , disambiguationTagIdByItemId = GlossaryItemIdDict.empty
        , normalTagIdsByItemId = GlossaryItemIdDict.empty
        , itemIdsByTagId = TagIdDict.empty
        , itemIdByFragmentIdentifierForRawDisambiguatedPreferredTerm = Dict.empty
        , relatedItemIdsById = GlossaryItemIdDict.empty
        , disambiguatedPreferredTermByItemId = GlossaryItemIdDict.empty
        , orderedAlphabetically = []
        , orderedByMostMentionedFirst = []
        }


{-| Check if the set of glossary items is empty.
-}
isEmpty : GlossaryItemsForUi -> Bool
isEmpty (GlossaryItemsForUi items) =
    GlossaryItemIdDict.isEmpty items.itemById


{-| Convert a list of glossary items for/from HTML into a `GlossaryItems`.
-}
fromList : List DescribedTag -> Maybe DisambiguatedTerm -> List GlossaryItemForUi -> Result String GlossaryItemsForUi
fromList describedTags_ disambiguatedPreferredTermForStartingItem_ glossaryItemsForUi =
    GlossaryTags.fromList describedTags_
        |> Result.andThen
            (\tags_ ->
                let
                    tagById : TagIdDict Tag
                    tagById =
                        GlossaryTags.tagById tags_

                    tagIdByRawTag : Dict String TagId
                    tagIdByRawTag =
                        GlossaryTags.tagIdByRawTag tags_

                    itemIdByFragmentIdentifierForRawDisambiguatedPreferredTerm_ : DuplicateRejectingDict String GlossaryItemId
                    itemIdByFragmentIdentifierForRawDisambiguatedPreferredTerm_ =
                        glossaryItemsForUi
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

                    itemById : GlossaryItemIdDict GlossaryItem
                    itemById =
                        glossaryItemsForUi
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
                                    in
                                    GlossaryItemIdDict.insert (GlossaryItem.id glossaryItem) glossaryItem itemById_
                                )
                                GlossaryItemIdDict.empty

                    ( disambiguationTagIdByItemId, ( normalTagIdsByItemId, disambiguatedPreferredTermByItemId ) ) =
                        glossaryItemsForUi
                            |> List.foldl
                                (\item ( disambiguationTagByItemId_, ( normalTagsByItemId_, disambiguatedPreferredTermByItemId_ ) ) ->
                                    let
                                        id : GlossaryItemId
                                        id =
                                            GlossaryItemForUi.id item

                                        maybePreferredTerm : Maybe Term
                                        maybePreferredTerm =
                                            itemById
                                                |> GlossaryItemIdDict.get id
                                                |> Maybe.map GlossaryItem.preferredTerm

                                        disambiguationTag : Maybe Tag
                                        disambiguationTag =
                                            item
                                                |> GlossaryItemForUi.disambiguationTag

                                        disambiguationTagId : Maybe TagId
                                        disambiguationTagId =
                                            disambiguationTag
                                                |> Maybe.andThen
                                                    (\disambiguationTag_ ->
                                                        Dict.get (Tag.raw disambiguationTag_) tagIdByRawTag
                                                    )

                                        disambiguatedPreferredTerm_ : Maybe DisambiguatedTerm
                                        disambiguatedPreferredTerm_ =
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
                                    in
                                    ( GlossaryItemIdDict.insert id
                                        disambiguationTagId
                                        disambiguationTagByItemId_
                                    , ( GlossaryItemIdDict.insert id
                                            (item
                                                |> GlossaryItemForUi.normalTags
                                                |> List.filterMap
                                                    (\tag ->
                                                        Dict.get (Tag.raw tag) tagIdByRawTag
                                                    )
                                            )
                                            normalTagsByItemId_
                                      , GlossaryItemIdDict.insert id
                                            disambiguatedPreferredTerm_
                                            disambiguatedPreferredTermByItemId_
                                      )
                                    )
                                )
                                ( GlossaryItemIdDict.empty, ( GlossaryItemIdDict.empty, GlossaryItemIdDict.empty ) )

                    startingItemId_ : Maybe GlossaryItemId
                    startingItemId_ =
                        disambiguatedPreferredTermForStartingItem_
                            |> Maybe.andThen
                                (\disambiguatedPreferredTermForStartingItem ->
                                    disambiguatedPreferredTermByItemId
                                        |> GlossaryItemIdDict.foldl
                                            (\itemId disambiguatedPreferredTerm_ result ->
                                                if disambiguatedPreferredTerm_ == Just disambiguatedPreferredTermForStartingItem then
                                                    Just itemId

                                                else
                                                    result
                                            )
                                            Nothing
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

                                        disambiguatedPreferredTerm1 : Maybe DisambiguatedTerm
                                        disambiguatedPreferredTerm1 =
                                            preferredTerm1
                                                |> Maybe.map
                                                    (\preferredTerm1_ ->
                                                        let
                                                            disambiguationTag1 : Maybe Tag
                                                            disambiguationTag1 =
                                                                disambiguationTagIdByItemId
                                                                    |> GlossaryItemIdDict.get value1
                                                                    |> Maybe.andThen
                                                                        (Maybe.andThen
                                                                            (\tagId -> TagIdDict.get tagId tagById)
                                                                        )
                                                        in
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
                in
                Result.map
                    (\itemIdByFragmentIdentifierForRawDisambiguatedPreferredTerm1 ->
                        let
                            itemIdsByTagId_ : TagIdDict (List GlossaryItemId)
                            itemIdsByTagId_ =
                                let
                                    result0 : TagIdDict (List GlossaryItemId)
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
                                glossaryItemsForUi
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

                            orderedAlphabetically__ : List GlossaryItemId
                            orderedAlphabetically__ =
                                orderAlphabetically glossaryItemsForUi

                            orderedByMostMentionedFirst_ : List GlossaryItemId
                            orderedByMostMentionedFirst_ =
                                orderByMostMentionedFirst glossaryItemsForUi
                        in
                        GlossaryItemsForUi
                            { tags = tags_
                            , itemById = itemById
                            , startingItemId = startingItemId_
                            , disambiguationTagIdByItemId = disambiguationTagIdByItemId
                            , normalTagIdsByItemId = sortedNormalTagIdsByItemId
                            , itemIdsByTagId = itemIdsByTagId_
                            , itemIdByFragmentIdentifierForRawDisambiguatedPreferredTerm = itemIdByFragmentIdentifierForRawDisambiguatedPreferredTerm1
                            , relatedItemIdsById = relatedItemIdsById
                            , disambiguatedPreferredTermByItemId = disambiguatedPreferredTermByItemId
                            , orderedAlphabetically = orderedAlphabetically__
                            , orderedByMostMentionedFirst = orderedByMostMentionedFirst_
                            }
                    )
                    itemIdByFragmentIdentifierForRawDisambiguatedPreferredTermResult
            )


relatedPreferredTerms_ : (GlossaryItemId -> GlossaryItemsForUi -> Maybe DisambiguatedTerm) -> Maybe TagId -> GlossaryItemId -> GlossaryItemsForUi -> Maybe (List DisambiguatedTerm)
relatedPreferredTerms_ disambiguatedPreferredTerm_ filterByTagId itemId ((GlossaryItemsForUi items) as glossaryItemsForUi) =
    items.relatedItemIdsById
        |> GlossaryItemIdDict.get itemId
        |> Maybe.map
            (\relatedItemIds ->
                relatedItemIds
                    |> List.filterMap
                        (\relatedItemId ->
                            let
                                relatedItemMatchesTagBeingFilteredBy : Bool
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
                                disambiguatedPreferredTerm_ relatedItemId glossaryItemsForUi

                            else
                                Nothing
                        )
            )


get_ : (GlossaryItemId -> GlossaryItemsForUi -> Maybe DisambiguatedTerm) -> Maybe TagId -> GlossaryItemId -> GlossaryItemsForUi -> Maybe GlossaryItemForUi
get_ disambiguatedPreferredTerm_ filterByTagId itemId ((GlossaryItemsForUi items) as glossaryItemsForUi) =
    GlossaryItemIdDict.get itemId items.itemById
        |> Maybe.andThen
            (\item ->
                let
                    id : GlossaryItemId
                    id =
                        GlossaryItem.id item

                    preferredTerm : Term
                    preferredTerm =
                        GlossaryItem.preferredTerm item

                    alternativeTerms : List Term
                    alternativeTerms =
                        GlossaryItem.alternativeTerms item

                    tagById : TagIdDict Tag
                    tagById =
                        GlossaryTags.tagById items.tags

                    disambiguationTagId : Maybe TagId
                    disambiguationTagId =
                        items.disambiguationTagIdByItemId
                            |> GlossaryItemIdDict.get itemId
                            |> Maybe.andThen identity

                    disambiguationTag : Maybe Tag
                    disambiguationTag =
                        disambiguationTagId
                            |> Maybe.andThen
                                (\tagId -> TagIdDict.get tagId tagById)

                    normalTagIds : List TagId
                    normalTagIds =
                        items.normalTagIdsByItemId
                            |> GlossaryItemIdDict.get itemId
                            |> Maybe.withDefault []

                    normalTags : List Tag
                    normalTags =
                        normalTagIds
                            |> List.filterMap
                                (\normalTagId -> TagIdDict.get normalTagId tagById)

                    definition : Maybe Definition
                    definition =
                        GlossaryItem.definition item

                    relatedPreferredTerms : List DisambiguatedTerm
                    relatedPreferredTerms =
                        glossaryItemsForUi
                            |> relatedPreferredTerms_ disambiguatedPreferredTerm_ filterByTagId itemId
                            |> Maybe.withDefault []

                    needsUpdating : Bool
                    needsUpdating =
                        GlossaryItem.needsUpdating item

                    lastUpdatedDateAsIso8601 : Maybe String
                    lastUpdatedDateAsIso8601 =
                        GlossaryItem.lastUpdatedDateAsIso8601 item

                    lastUpdatedByName : Maybe String
                    lastUpdatedByName =
                        GlossaryItem.lastUpdatedByName item

                    lastUpdatedByEmailAddress : Maybe String
                    lastUpdatedByEmailAddress =
                        GlossaryItem.lastUpdatedByEmailAddress item
                in
                -- Only return the item if it matches the filterByTagId, otherwise return Nothing.
                let
                    matchesTagFilter : Bool
                    matchesTagFilter =
                        case filterByTagId of
                            Nothing ->
                                True

                            Just tagId ->
                                (disambiguationTagId == Just tagId)
                                    || List.member tagId normalTagIds
                in
                if matchesTagFilter then
                    Just <|
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

                else
                    Nothing
            )


{-| Get the item associated with the starting item ID, if it exists.
-}
startingItem : GlossaryItemsForUi -> Maybe GlossaryItemForUi
startingItem ((GlossaryItemsForUi items) as glossaryItemsForUi) =
    items.startingItemId
        |> Maybe.andThen
            (\startingItemId -> get startingItemId glossaryItemsForUi)


{-| Get the item associated with an ID. If the ID is not found, return `Nothing`.
-}
get : GlossaryItemId -> GlossaryItemsForUi -> Maybe GlossaryItemForUi
get =
    get_ disambiguatedPreferredTerm Nothing


{-| Get the item associated with an ID, restricting its related terms to those that have the given tag ID. If the item ID is not found, return `Nothing`.
-}
getWithRelatedTermsFilteredByTagId : Maybe TagId -> GlossaryItemId -> GlossaryItemsForUi -> Maybe GlossaryItemForUi
getWithRelatedTermsFilteredByTagId filterByTagId =
    get_ disambiguatedPreferredTerm filterByTagId


{-| Get the item whose disambiguated preferred term matches the given tag, if such an item exists and has that tag. Returns `Nothing` if the tag doesn't exist, if no item's disambiguated preferred term matches the tag's raw string, or if the matching item doesn't have that tag.
-}
itemWhoseDisambiguatedPreferredTermMatchesTag : TagId -> GlossaryItemsForUi -> Maybe GlossaryItemForUi
itemWhoseDisambiguatedPreferredTermMatchesTag tagId ((GlossaryItemsForUi items) as glossaryItemsForUi) =
    let
        tagById : TagIdDict Tag
        tagById =
            GlossaryTags.tagById items.tags

        -- Get the tag's raw string
        maybeTag : Maybe Tag
        maybeTag =
            TagIdDict.get tagId tagById

        maybeTagRaw : Maybe String
        maybeTagRaw =
            maybeTag |> Maybe.map Tag.raw
    in
    maybeTagRaw
        |> Maybe.andThen
            (\tagRaw ->
                -- Find an item whose disambiguated preferred term matches this tag
                items.itemById
                    |> GlossaryItemIdDict.toList
                    |> List.filterMap
                        (\( candidateId, _ ) ->
                            disambiguatedPreferredTerm candidateId glossaryItemsForUi
                                |> Maybe.andThen
                                    (\disambiguatedTerm ->
                                        let
                                            termRaw : String
                                            termRaw =
                                                disambiguatedTerm
                                                    |> DisambiguatedTerm.toTerm
                                                    |> Term.raw
                                                    |> RawTerm.toString
                                        in
                                        if termRaw == tagRaw then
                                            -- Check if this item has this tag
                                            let
                                                hasTag : Bool
                                                hasTag =
                                                    (items.disambiguationTagIdByItemId
                                                        |> GlossaryItemIdDict.get candidateId
                                                        |> (==) (Just <| Just tagId)
                                                    )
                                                        || (items.normalTagIdsByItemId
                                                                |> GlossaryItemIdDict.get candidateId
                                                                |> Maybe.map (List.member tagId)
                                                                |> Maybe.withDefault False
                                                           )
                                            in
                                            if hasTag then
                                                get candidateId glossaryItemsForUi

                                            else
                                                Nothing

                                        else
                                            Nothing
                                    )
                        )
                    |> List.head
            )


itemsWhoseDisambiguatedPreferredTermMatchesATagTheyHave : Maybe TagId -> GlossaryItemsForUi -> List GlossaryItemForUi
itemsWhoseDisambiguatedPreferredTermMatchesATagTheyHave filterByTagId ((GlossaryItemsForUi items) as glossaryItemsForUi) =
    let
        tagById : TagIdDict Tag
        tagById =
            GlossaryTags.tagById items.tags

        -- Find items where their disambiguated preferred term matches one of their tags
        -- by checking each tag using the existing function
        candidateItems : List ( GlossaryItemId, GlossaryItemForUi, TagId )
        candidateItems =
            tagById
                |> TagIdDict.toList
                |> List.filterMap
                    (\( tagId, _ ) ->
                        itemWhoseDisambiguatedPreferredTermMatchesTag tagId glossaryItemsForUi
                            |> Maybe.andThen
                                (\item ->
                                    -- Apply the filterByTagId if present
                                    case filterByTagId of
                                        Nothing ->
                                            Just ( GlossaryItemForUi.id item, item, tagId )

                                        Just _ ->
                                            -- Re-get the item with filtered related terms
                                            getWithRelatedTermsFilteredByTagId filterByTagId (GlossaryItemForUi.id item) glossaryItemsForUi
                                                |> Maybe.map (\filteredItem -> ( GlossaryItemForUi.id item, filteredItem, tagId ))
                                )
                    )

        -- Count how many items have each tag
        itemCountByTagId : TagIdDict Int
        itemCountByTagId =
            items.itemIdsByTagId
                |> TagIdDict.foldl
                    (\tagId itemIds acc ->
                        TagIdDict.insert tagId (List.length itemIds) acc
                    )
                    TagIdDict.empty

        -- Sort by tag popularity (most popular first)
        sortedCandidates : List GlossaryItemForUi
        sortedCandidates =
            candidateItems
                |> List.sortWith
                    (\( _, _, tagId1 ) ( _, _, tagId2 ) ->
                        let
                            count1 : Int
                            count1 =
                                TagIdDict.get tagId1 itemCountByTagId
                                    |> Maybe.withDefault 0

                            count2 : Int
                            count2 =
                                TagIdDict.get tagId2 itemCountByTagId
                                    |> Maybe.withDefault 0
                        in
                        case compare count1 count2 of
                            LT ->
                                GT

                            EQ ->
                                EQ

                            GT ->
                                LT
                    )
                |> List.map (\( _, item, _ ) -> item)
    in
    sortedCandidates


{-| Get the items that are related to the given item.
-}
relatedItems : GlossaryItemId -> Maybe TagId -> GlossaryItemsForUi -> List GlossaryItemForUi
relatedItems itemId filterByTagId ((GlossaryItemsForUi items) as glossaryItemsForUi) =
    let
        relatedItemIds : List GlossaryItemId
        relatedItemIds =
            GlossaryItemIdDict.get itemId items.relatedItemIdsById
                |> Maybe.withDefault []

        isStartingItem : Bool
        isStartingItem =
            items.startingItemId == Just itemId

        hasNoRelatedItems : Bool
        hasNoRelatedItems =
            List.isEmpty relatedItemIds
    in
    if isStartingItem && hasNoRelatedItems then
        itemsWhoseDisambiguatedPreferredTermMatchesATagTheyHave filterByTagId glossaryItemsForUi

    else
        let
            explicitlyRelatedItems : List GlossaryItemForUi
            explicitlyRelatedItems =
                relatedItemIds
                    |> List.filterMap (\relatedItemId -> getWithRelatedTermsFilteredByTagId filterByTagId relatedItemId glossaryItemsForUi)

            itemForTagBeingFilteredBy : Maybe GlossaryItemForUi
            itemForTagBeingFilteredBy =
                if isStartingItem then
                    Nothing

                else
                    filterByTagId
                        |> Maybe.andThen
                            (\tagId ->
                                itemWhoseDisambiguatedPreferredTermMatchesTag tagId glossaryItemsForUi
                            )
                        |> Maybe.andThen
                            (\itemForTag ->
                                if GlossaryItemForUi.id itemForTag == itemId then
                                    Nothing

                                else
                                    Just itemForTag
                            )
        in
        explicitlyRelatedItems
            ++ ([ itemForTagBeingFilteredBy
                    |> Maybe.andThen
                        (\item ->
                            if
                                List.any
                                    (\explicitlyRelatedItem ->
                                        GlossaryItemForUi.id explicitlyRelatedItem == GlossaryItemForUi.id item
                                    )
                                    explicitlyRelatedItems
                            then
                                Nothing

                            else
                                Just item
                        )
                ]
                    |> List.filterMap identity
               )


outline : GlossaryItemId -> GlossaryItemsForUi -> Maybe GlossaryItemOutline
outline glossaryItemId glossaryItemsForUi =
    get_ disambiguatedPreferredTerm Nothing glossaryItemId glossaryItemsForUi
        |> Maybe.map
            (\glossaryItemForUi ->
                { disambiguatedPreferredTerm =
                    glossaryItemForUi
                        |> GlossaryItemForUi.disambiguatedPreferredTerm
                        |> DisambiguatedTerm.toTerm
                        |> Term.raw
                        |> RawTerm.toString
                , preferredTerm =
                    glossaryItemForUi
                        |> GlossaryItemForUi.nonDisambiguatedPreferredTerm
                        |> Term.raw
                        |> RawTerm.toString
                , alternativeTerms =
                    glossaryItemForUi
                        |> GlossaryItemForUi.alternativeTerms
                        |> List.map (Term.raw >> RawTerm.toString)
                , allTags =
                    glossaryItemForUi
                        |> GlossaryItemForUi.allTags
                        |> List.map Tag.raw
                }
            )


{-| The tags for these glossary items. Tags can exist without being used in any items.
-}
tags : GlossaryItemsForUi -> List Tag
tags (GlossaryItemsForUi items) =
    items.tags
        |> GlossaryTags.tagById
        |> TagIdDict.values
        |> List.sortWith Tag.compareAlphabetically


{-| The tags for these glossary items along with their IDs and descriptions.
Tags can exist without being used in any items.
-}
describedTags : GlossaryItemsForUi -> List DescribedTag
describedTags (GlossaryItemsForUi items) =
    let
        tagById : TagIdDict Tag
        tagById =
            GlossaryTags.tagById items.tags
    in
    items.tags
        |> GlossaryTags.tagDescriptionById
        |> TagIdDict.toList
        |> List.filterMap
            (\( id, description ) ->
                TagIdDict.get id tagById
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
tagByIdList : GlossaryItemsForUi -> List ( TagId, Tag )
tagByIdList (GlossaryItemsForUi items) =
    items.tags
        |> GlossaryTags.tagById
        |> TagIdDict.toList


{-| Look up a tag ID from its contents.
-}
tagIdFromTag : Tag -> GlossaryItemsForUi -> Maybe TagId
tagIdFromTag tag (GlossaryItemsForUi items) =
    let
        tagIdByRawTag : Dict String TagId
        tagIdByRawTag =
            GlossaryTags.tagIdByRawTag items.tags
    in
    Dict.get (Tag.raw tag) tagIdByRawTag


{-| Look up a tag from its ID.
-}
tagFromId : TagId -> GlossaryItemsForUi -> Maybe Tag
tagFromId tagId (GlossaryItemsForUi items) =
    items.tags
        |> GlossaryTags.tagById
        |> TagIdDict.get tagId


{-| Look up a tag's description from its ID.
-}
tagDescriptionFromId : TagId -> GlossaryItemsForUi -> Maybe TagDescription
tagDescriptionFromId tagId (GlossaryItemsForUi items) =
    items.tags
        |> GlossaryTags.tagDescriptionById
        |> TagIdDict.get tagId


{-| The disambiguated preferred term for the item with the given ID.
-}
disambiguatedPreferredTerm : GlossaryItemId -> GlossaryItemsForUi -> Maybe DisambiguatedTerm
disambiguatedPreferredTerm itemId (GlossaryItemsForUi items) =
    GlossaryItemIdDict.get itemId items.disambiguatedPreferredTermByItemId
        |> Maybe.andThen identity


{-| All the disambiguated preferred terms in these glossary items.
-}
disambiguatedPreferredTerms : Maybe TagId -> GlossaryItemsForUi -> List ( GlossaryItemId, DisambiguatedTerm )
disambiguatedPreferredTerms filterByTagId ((GlossaryItemsForUi items) as glossaryItemsForUi) =
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
            (\itemId -> glossaryItemsForUi |> disambiguatedPreferredTerm itemId |> Maybe.map (Tuple.pair itemId))
        |> List.sortWith compareDisambiguatedTerms


{-| All the outlines for the items in the glossary, ordered alphabetically by their disambiguated preferred terms.
-}
itemOutlines : Maybe TagId -> GlossaryItemsForUi -> List GlossaryItemOutline
itemOutlines filterByTagId ((GlossaryItemsForUi items) as glossaryItemsForUi) =
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

        compareOutlines : GlossaryItemOutline -> GlossaryItemOutline -> Order
        compareOutlines item1 item2 =
            Extras.String.compareOnlyAlphanumCharsModuloDiacritics
                item1.disambiguatedPreferredTerm
                item2.disambiguatedPreferredTerm
    in
    itemIds
        |> List.filterMap (\itemId -> glossaryItemsForUi |> outline itemId)
        |> List.sortWith compareOutlines


{-| Look up the ID of the item with the given raw disambiguated preferred term.
-}
itemIdFromRawDisambiguatedPreferredTerm : RawTerm -> GlossaryItemsForUi -> Maybe GlossaryItemId
itemIdFromRawDisambiguatedPreferredTerm rawTerm (GlossaryItemsForUi items) =
    items.itemIdByFragmentIdentifierForRawDisambiguatedPreferredTerm
        |> Dict.get (rawTerm |> RawTerm.toString |> String.replace " " "_")


{-| Look up the ID of the item with the given fragment identifier.
-}
itemIdFromFragmentIdentifier : String -> GlossaryItemsForUi -> Maybe GlossaryItemId
itemIdFromFragmentIdentifier fragmentIdentifier (GlossaryItemsForUi items) =
    Dict.get
        fragmentIdentifier
        items.itemIdByFragmentIdentifierForRawDisambiguatedPreferredTerm


{-| Look up the disambiguated preferred term of the item with the given raw disambiguated preferred term.
-}
disambiguatedPreferredTermFromRaw : RawTerm -> GlossaryItemsForUi -> Maybe DisambiguatedTerm
disambiguatedPreferredTermFromRaw rawTerm ((GlossaryItemsForUi items) as glossaryItemsForUi) =
    items.itemIdByFragmentIdentifierForRawDisambiguatedPreferredTerm
        |> Dict.get (rawTerm |> RawTerm.toString |> String.replace " " "_")
        |> Maybe.andThen
            (\itemId ->
                glossaryItemsForUi
                    |> get itemId
                    |> Maybe.map GlossaryItemForUi.disambiguatedPreferredTerm
            )


{-| All of the disambiguated preferred terms which have a definition.
-}
disambiguatedPreferredTermsWhichHaveDefinitions : Maybe TagId -> GlossaryItemsForUi -> List DisambiguatedTerm
disambiguatedPreferredTermsWhichHaveDefinitions filterByTagId ((GlossaryItemsForUi items) as glossaryItemsForUi) =
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
                    disambiguatedPreferredTerm itemId glossaryItemsForUi

                else
                    Nothing
            )
        |> List.sortWith DisambiguatedTerm.compareAlphabetically


{-| The IDs of the items that list this item as a related one.
-}
relatedForWhichItems : GlossaryItemId -> GlossaryItemsForUi -> List GlossaryItemId
relatedForWhichItems itemId (GlossaryItemsForUi items) =
    items.relatedItemIdsById
        |> GlossaryItemIdDict.foldl
            (\otherItemId relatedItemIds result ->
                if List.member itemId relatedItemIds then
                    otherItemId :: result

                else
                    result
            )
            []


{-| The disambiguated preferred terms of the items that list this one as a related item.
-}
preferredTermsOfItemsListingThisItemAsRelated : GlossaryItemId -> GlossaryItemsForUi -> List DisambiguatedTerm
preferredTermsOfItemsListingThisItemAsRelated id glossaryItemsForUi =
    glossaryItemsForUi
        |> relatedForWhichItems id
        |> List.filterMap (\id_ -> disambiguatedPreferredTerm id_ glossaryItemsForUi)


{-| The outlines of the items that list this one as a related item.
-}
outlinesOfItemsListingThisItemAsRelated : GlossaryItemId -> GlossaryItemsForUi -> List GlossaryItemOutline
outlinesOfItemsListingThisItemAsRelated id glossaryItemsForUi =
    glossaryItemsForUi
        |> relatedForWhichItems id
        |> List.filterMap (\id_ -> outline id_ glossaryItemsForUi)


{-| A list of pairs associating each alternative term with the disambiguated preferred terms that it appears together with.
-}
disambiguatedPreferredTermsByAlternativeTerm : Maybe TagId -> GlossaryItemsForUi -> List ( Term, List ( GlossaryItemId, DisambiguatedTerm ) )
disambiguatedPreferredTermsByAlternativeTerm filterByTagId ((GlossaryItemsForUi items) as glossaryItemsForUi) =
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
                            case disambiguatedPreferredTerm itemId glossaryItemsForUi of
                                Just disambiguatedPreferredTerm_ ->
                                    item
                                        |> GlossaryItem.alternativeTerms
                                        |> List.foldl
                                            (\alternativeTerm ( alternativeTermByRaw1, preferredTermsByRawAlternativeTerm1 ) ->
                                                let
                                                    raw : String
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


toList : Maybe TagId -> GlossaryItemsForUi -> List GlossaryItemId -> List ( GlossaryItemId, GlossaryItemForUi )
toList filterByTagId ((GlossaryItemsForUi items) as glossaryItemsForUi) =
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
                glossaryItemsForUi
                    |> get_ disambiguatedPreferredTerm filterByTagId itemId
                    |> Maybe.map (Tuple.pair itemId)

            else
                Nothing
        )


{-| Retrieve the glossary items ordered alphabetically.
-}
orderedAlphabetically : Maybe TagId -> GlossaryItemsForUi -> List ( GlossaryItemId, GlossaryItemForUi )
orderedAlphabetically filterByTagId ((GlossaryItemsForUi items) as glossaryItemsForUi) =
    toList filterByTagId glossaryItemsForUi items.orderedAlphabetically


{-| Retrieve the glossary items ordered by most mentioned first.
-}
orderedByMostMentionedFirst : Maybe TagId -> GlossaryItemsForUi -> List ( GlossaryItemId, GlossaryItemForUi )
orderedByMostMentionedFirst filterByTagId ((GlossaryItemsForUi items) as glossaryItemsForUi) =
    toList filterByTagId glossaryItemsForUi items.orderedByMostMentionedFirst


{-| Retrieve the glossary items ordered "focused on" a specific item.
-}
orderedFocusedOn :
    Maybe TagId
    -> GlossaryItemId
    -> GlossaryItemsForUi
    ->
        ( List ( GlossaryItemId, GlossaryItemForUi )
        , List ( GlossaryItemId, GlossaryItemForUi )
        )
orderedFocusedOn filterByTagId glossaryItemId ((GlossaryItemsForUi items) as glossaryItemsForUi) =
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
                            itemHasDefinition : Bool
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
    ( toList filterByTagId glossaryItemsForUi ids
    , toList filterByTagId glossaryItemsForUi otherIds
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
orderByMostMentionedFirst glossaryItemsForUi =
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

        -- Maps a term to a score based on how often it occurs in glossaryItemsForUi.
        termScore : Term -> GlossaryItemId -> Int
        termScore term exceptId =
            glossaryItemsForUi
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
            glossaryItemsForUi
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
    glossaryItemsForUi
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
