module Data.GlossaryItems exposing (GlossaryItems, fromList, orderedAlphabetically, orderedByMostMentionedFirst, orderedFocusedOn, preferredTermIdsToIndexes, get, insert, update, remove, terms, preferredTerms, preferredTermsWithDefinitions, enableFocusingOn)

{-| A set of glossary items that make up a glossary.


# Glossary Items

@docs GlossaryItems, fromList, orderedAlphabetically, orderedByMostMentionedFirst, orderedFocusedOn, preferredTermIdsToIndexes, get, insert, update, remove, terms, preferredTerms, preferredTermsWithDefinitions, enableFocusingOn

-}

import Array exposing (Array)
import Data.GlossaryItem as GlossaryItem exposing (GlossaryItem)
import Data.GlossaryItem.Definition as Definition
import Data.GlossaryItem.RelatedTerm as RelatedTerm
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItem.TermId as TermId exposing (TermId)
import Data.GlossaryItemIndex as GlossaryItemIndex exposing (GlossaryItemIndex)
import Dict exposing (Dict)
import DirectedGraph exposing (DirectedGraph)
import Extras.Regex
import Regex
import Set exposing (Set)


{-| Glossary items constructed by the functions below.
This is done using an opaque type that supports efficiently retrieving the items ordered alphabetically or by most mentioned first.
-}
type GlossaryItems
    = GlossaryItems
        { orderedAlphabetically : Array ( GlossaryItemIndex, GlossaryItem )
        , orderedByMostMentionedFirst : Array ( GlossaryItemIndex, GlossaryItem )
        , orderedFocusedOn :
            Maybe
                ( TermId
                , ( Array ( GlossaryItemIndex, GlossaryItem )
                  , Array ( GlossaryItemIndex, GlossaryItem )
                  )
                )
        , preferredTermIdsToIndexes : Dict String GlossaryItemIndex
        }


compareForSortingAlphabetically : GlossaryItem -> GlossaryItem -> Order
compareForSortingAlphabetically item1 item2 =
    Term.compareAlphabetically
        (GlossaryItem.preferredTerm item1)
        (GlossaryItem.preferredTerm item2)


{-| Build glossary items from a list.

    import Array
    import Data.GlossaryItem as GlossaryItem exposing (GlossaryItem)
    import Data.GlossaryItem.Term as Term exposing (Term)

    item1 : GlossaryItem
    item1 = GlossaryItem.init
              (Term.fromMarkdown "\\_\\_slots\\_\\_" False)
              []
              []
              []
              False
              Nothing

    item2 : GlossaryItem
    item2 = GlossaryItem.init
              (Term.fromMarkdown "Situation" False)
              []
              []
              []
              False
              Nothing

    item3 : GlossaryItem
    item3 = GlossaryItem.init
              (Term.fromMarkdown "strong" False)
              []
              []
              []
              False
              Nothing

    fromList [item1, item2, item3]
    |> orderedAlphabetically
    |> Array.toList
    |> List.map (Tuple.second >> GlossaryItem.allTerms >> List.map Term.raw)
    --> [["Situation"], ["\\_\\_slots\\_\\_"], ["strong"]]

-}
fromList : List GlossaryItem -> GlossaryItems
fromList glossaryItems =
    let
        sanitised : List GlossaryItem
        sanitised =
            sanitiseList glossaryItems

        alphabetically : List ( GlossaryItemIndex, GlossaryItem )
        alphabetically =
            sanitised
                |> List.sortWith compareForSortingAlphabetically
                |> zipListWithIndexes

        byMostMentionedFirst : List ( GlossaryItemIndex, GlossaryItem )
        byMostMentionedFirst =
            orderListByMostMentionedFirst alphabetically

        preferredTermIdsToIndexes1 : List ( GlossaryItemIndex, GlossaryItem ) -> Dict String GlossaryItemIndex
        preferredTermIdsToIndexes1 =
            List.foldl
                (\( index, item ) result ->
                    let
                        termIdString =
                            item |> GlossaryItem.preferredTerm |> Term.id |> TermId.toString
                    in
                    Dict.insert termIdString index result
                )
                Dict.empty
    in
    GlossaryItems <|
        { orderedAlphabetically = Array.fromList alphabetically
        , orderedByMostMentionedFirst = Array.fromList byMostMentionedFirst
        , orderedFocusedOn = Nothing
        , preferredTermIdsToIndexes = preferredTermIdsToIndexes1 alphabetically
        }


orderListByMostMentionedFirst : List ( GlossaryItemIndex, GlossaryItem ) -> List ( GlossaryItemIndex, GlossaryItem )
orderListByMostMentionedFirst indexedGlossaryItems =
    let
        indexed : List ( Int, GlossaryItem )
        indexed =
            List.map (Tuple.mapFirst GlossaryItemIndex.toInt) indexedGlossaryItems

        -- Maps a term to a score based on whether or not it occurs in glossaryItem.
        -- This is done in a primitive way. A more sophisticated solution could use stemming
        -- or other techniques.
        termScoreInItem : Term -> GlossaryItem -> Int
        termScoreInItem term glossaryItem =
            let
                termAsWord : Regex.Regex
                termAsWord =
                    ("\\b" ++ Extras.Regex.escapeStringForUseInRegex (Term.raw term) ++ "\\b")
                        |> Regex.fromString
                        |> Maybe.withDefault Regex.never

                score : Int
                score =
                    (glossaryItem |> GlossaryItem.allTerms |> List.map (Term.raw >> Regex.find termAsWord >> List.length) |> List.sum)
                        + (glossaryItem |> GlossaryItem.definitions |> List.map (Definition.raw >> Regex.find termAsWord >> List.length) |> List.sum)
                        + (glossaryItem |> GlossaryItem.relatedPreferredTerms |> List.map RelatedTerm.raw |> List.map (Regex.find termAsWord >> List.length) |> List.sum)
            in
            if score > 0 then
                1

            else
                0

        -- Maps a term to a score based on how often it occurs in glossaryItems.
        termScore : Term -> Int -> Int
        termScore term exceptIndex =
            indexed
                |> List.foldl
                    (\( index, glossaryItem ) result ->
                        result
                            + (if index == exceptIndex then
                                0

                               else
                                termScoreInItem term glossaryItem
                              )
                    )
                    0

        termBodyScores : Dict String Int
        termBodyScores =
            indexed
                |> List.concatMap
                    (\( index, glossaryItem ) ->
                        glossaryItem
                            |> GlossaryItem.allTerms
                            |> List.map (\term -> ( index, term ))
                    )
                |> List.foldl
                    (\( glossaryItemIndex, term ) result ->
                        Dict.insert
                            (Term.raw term)
                            (termScore term glossaryItemIndex)
                            result
                    )
                    Dict.empty
    in
    indexedGlossaryItems
        |> List.sortWith
            (\( _, item1 ) ( _, item2 ) ->
                let
                    itemScore : GlossaryItem -> Int
                    itemScore =
                        GlossaryItem.allTerms
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
                            (item1 |> GlossaryItem.preferredTerm |> Term.raw |> String.toUpper)
                            (item2 |> GlossaryItem.preferredTerm |> Term.raw |> String.toUpper)

                    GT ->
                        LT
            )


sanitiseList : List GlossaryItem -> List GlossaryItem
sanitiseList glossaryItems =
    let
        preferredTermIdsSet : Set String
        preferredTermIdsSet =
            glossaryItems
                |> List.map (GlossaryItem.preferredTerm >> Term.id >> TermId.toString)
                |> Set.fromList
    in
    glossaryItems
        |> List.map
            (\glossaryItem ->
                GlossaryItem.updateRelatedTerms
                    (glossaryItem
                        |> GlossaryItem.relatedPreferredTerms
                        |> List.filter
                            (\relatedTerm ->
                                Set.member (relatedTerm |> RelatedTerm.idReference |> TermId.toString) preferredTermIdsSet
                            )
                    )
                    glossaryItem
            )


zipListWithIndexes : List GlossaryItem -> List ( GlossaryItemIndex, GlossaryItem )
zipListWithIndexes =
    List.indexedMap Tuple.pair
        >> List.map (Tuple.mapFirst GlossaryItemIndex.fromInt)


{-| Retrieve the glossary item at the specified index, assuming alphabetical order.
-}
get : GlossaryItemIndex -> GlossaryItems -> Maybe GlossaryItem
get indexWhenOrderedAlphabetically glossaryItems =
    glossaryItems
        |> orderedAlphabetically
        |> Array.map Tuple.second
        |> Array.get (GlossaryItemIndex.toInt indexWhenOrderedAlphabetically)


{-| Add a glossary item to the set.
-}
insert : GlossaryItem -> GlossaryItems -> GlossaryItems
insert glossaryItem glossaryItems =
    let
        itemsList : List GlossaryItem
        itemsList =
            glossaryItems
                |> orderedAlphabetically
                |> Array.toList
                |> List.map Tuple.second
    in
    glossaryItem :: itemsList |> fromList


{-| Replace the glossary item at the given index, assuming alphabetical order.
-}
update : GlossaryItemIndex -> GlossaryItem -> GlossaryItems -> GlossaryItems
update indexWhenOrderedAlphabetically glossaryItem glossaryItems =
    glossaryItems
        |> orderedAlphabetically
        |> Array.toList
        |> List.map
            (\( index, item ) ->
                if index == indexWhenOrderedAlphabetically then
                    glossaryItem

                else
                    item
            )
        |> fromList


{-| Remove the glossary item at the given index, assuming alphabetical order.
-}
remove : GlossaryItemIndex -> GlossaryItems -> GlossaryItems
remove indexWhenOrderedAlphabetically glossaryItems =
    glossaryItems
        |> orderedAlphabetically
        |> Array.toList
        |> List.filterMap
            (\( index, item ) ->
                if index == indexWhenOrderedAlphabetically then
                    Nothing

                else
                    Just item
            )
        |> fromList
        |> (case glossaryItems of
                GlossaryItems items ->
                    case items.orderedFocusedOn of
                        Just ( termId, _ ) ->
                            enableFocusingOn termId

                        _ ->
                            identity
           )


{-| Retrieve the glossary items ordered alphabetically.
-}
orderedAlphabetically : GlossaryItems -> Array ( GlossaryItemIndex, GlossaryItem )
orderedAlphabetically glossaryItems =
    case glossaryItems of
        GlossaryItems items ->
            items.orderedAlphabetically


{-| Retrieve the glossary items ordered by most mentioned first.
-}
orderedByMostMentionedFirst : GlossaryItems -> Array ( GlossaryItemIndex, GlossaryItem )
orderedByMostMentionedFirst glossaryItems =
    case glossaryItems of
        GlossaryItems items ->
            items.orderedByMostMentionedFirst


{-| Retrieve the glossary items ordered "focused on" a specific item, identified by its preferred term.
-}
orderedFocusedOn :
    TermId
    -> GlossaryItems
    ->
        Maybe
            ( Array ( GlossaryItemIndex, GlossaryItem )
            , Array ( GlossaryItemIndex, GlossaryItem )
            )
orderedFocusedOn termId glossaryItems =
    case glossaryItems of
        GlossaryItems items ->
            case items.orderedFocusedOn of
                Just ( termIdFocusedOn, indexedItems ) ->
                    if termIdFocusedOn == termId then
                        Just indexedItems

                    else
                        Nothing

                _ ->
                    Nothing


{-| Retrieve a dict mapping each preferred term ID to its index in the item list (no matter how it's sorted).
-}
preferredTermIdsToIndexes : GlossaryItems -> Dict String GlossaryItemIndex
preferredTermIdsToIndexes glossaryItems =
    case glossaryItems of
        GlossaryItems items ->
            items.preferredTermIdsToIndexes


{-| Retrieve the list of all terms in the glossary.
-}
terms : GlossaryItems -> List Term
terms =
    orderedAlphabetically
        >> Array.toList
        >> List.concatMap (Tuple.second >> GlossaryItem.allTerms)


{-| Retrieve the list of all preferred terms in the glossary.
A _preferred term_ is a term that occurs as the first (and possibly only) one in a glossary item.
When adding related terms in the UI, only preferred terms are available.
This is to encourage standardizing on one "preferred" term for a concept, instead of several synonyms with no clear preferred one.
-}
preferredTerms : GlossaryItems -> List Term
preferredTerms =
    orderedAlphabetically
        >> Array.toList
        >> List.map (Tuple.second >> GlossaryItem.preferredTerm)


{-| Similar to `preferredTerms` but only return those preferred terms whose items have at least one definition.
-}
preferredTermsWithDefinitions : GlossaryItems -> List Term
preferredTermsWithDefinitions =
    orderedAlphabetically
        >> Array.toList
        >> List.filterMap
            (\( _, item ) ->
                if GlossaryItem.hasSomeDefinitions item then
                    Just item

                else
                    Nothing
            )
        >> List.map GlossaryItem.preferredTerm


{-| Make it easy to retrieve the items ordered "focused on" a specific item (identified by the ID of that item's preferred term).
Returns an updated `GlossaryItems` where the necessary computations have been done.
-}
enableFocusingOn : TermId -> GlossaryItems -> GlossaryItems
enableFocusingOn termId glossaryItems =
    case glossaryItems of
        GlossaryItems items ->
            let
                preferredTermsGraph : DirectedGraph TermId
                preferredTermsGraph =
                    items.preferredTermIdsToIndexes
                        |> Dict.keys
                        |> List.foldl
                            (TermId.fromString >> DirectedGraph.insertVertex)
                            (DirectedGraph.empty TermId.toString TermId.fromString)

                relatedTermsGraph : DirectedGraph TermId
                relatedTermsGraph =
                    items.orderedAlphabetically
                        |> Array.toList
                        |> List.filterMap
                            (Tuple.second
                                >> (\item ->
                                        if GlossaryItem.hasSomeDefinitions item then
                                            Just item

                                        else
                                            Nothing
                                   )
                            )
                        |> List.foldl
                            (\item graph ->
                                let
                                    preferredTermId : TermId
                                    preferredTermId =
                                        item |> GlossaryItem.preferredTerm |> Term.id
                                in
                                item
                                    |> GlossaryItem.relatedPreferredTerms
                                    |> List.foldl
                                        (RelatedTerm.idReference >> DirectedGraph.insertEdge preferredTermId)
                                        graph
                            )
                            preferredTermsGraph

                termIdsByDistance : ( List TermId, List TermId )
                termIdsByDistance =
                    DirectedGraph.verticesByDistance termId relatedTermsGraph

                termIdToIndexedItem : TermId -> Maybe ( GlossaryItemIndex, GlossaryItem )
                termIdToIndexedItem termId_ =
                    items.preferredTermIdsToIndexes
                        |> Dict.get (TermId.toString termId_)
                        |> Maybe.andThen
                            (\index ->
                                Array.get (GlossaryItemIndex.toInt index) items.orderedAlphabetically
                            )

                itemsByDistance :
                    ( Array ( GlossaryItemIndex, GlossaryItem )
                    , Array ( GlossaryItemIndex, GlossaryItem )
                    )
                itemsByDistance =
                    termIdsByDistance
                        |> Tuple.mapBoth
                            (List.filterMap
                                termIdToIndexedItem
                                >> Array.fromList
                            )
                            (List.filterMap
                                termIdToIndexedItem
                                >> Array.fromList
                            )
            in
            GlossaryItems { items | orderedFocusedOn = Just ( termId, itemsByDistance ) }
