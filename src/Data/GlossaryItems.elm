module Data.GlossaryItems exposing (GlossaryItems, fromList, orderedAlphabetically, orderedByMostMentionedFirst, orderedFocusedOn, primaryTermIdsToIndexes, get, insert, update, remove, terms, primaryTerms, enableFocusingOn)

{-| A set of glossary items that make up a glossary.


# Glossary Items

@docs GlossaryItems, fromList, orderedAlphabetically, orderedByMostMentionedFirst, orderedFocusedOn, primaryTermIdsToIndexes, get, insert, update, remove, terms, primaryTerms, enableFocusingOn

-}

import Array exposing (Array)
import Data.GlossaryItem exposing (GlossaryItem)
import Data.GlossaryItem.Definition as Definition
import Data.GlossaryItem.RelatedTerm as RelatedTerm
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItem.TermId as TermId exposing (TermId)
import Data.GlossaryItemIndex as GlossaryItemIndex exposing (GlossaryItemIndex)
import Dict exposing (Dict)
import Extras.Regex
import Regex
import Set exposing (Set)
import UndirectedGraph exposing (UndirectedGraph)


{-| Glossary items constructed by the functions below.
This is done using an opaque type that supports efficiently retrieving the items ordered alphabetically or by most mentioned first.
-}
type GlossaryItems
    = GlossaryItems
        { orderedAlphabetically : Array ( GlossaryItemIndex, GlossaryItem )
        , orderedByMostMentionedFirst : Array ( GlossaryItemIndex, GlossaryItem )
        , orderedFocusedOn : Maybe ( TermId, Array ( GlossaryItemIndex, GlossaryItem ) )
        , primaryTermIdsToIndexes : Dict String GlossaryItemIndex
        }


compareForSortingAlphabetically : GlossaryItem -> GlossaryItem -> Order
compareForSortingAlphabetically item1 item2 =
    Term.compareAlphabetically
        (item1.terms |> List.head |> Maybe.withDefault Term.emptyPlaintext)
        (item2.terms |> List.head |> Maybe.withDefault Term.emptyPlaintext)


{-| Build glossary items from a list.

    import Array
    import Data.GlossaryItem exposing (GlossaryItem)
    import Data.GlossaryItem.Term as Term exposing (Term)

    item1 : GlossaryItem
    item1 = { terms = [ Term.fromMarkdown "\\_\\_slots\\_\\_" False ]
            , definitions = []
            , relatedTerms = []
            , needsUpdating = False
            , lastUpdatedDate = Nothing
            }

    item2 : GlossaryItem
    item2 = { terms = [ Term.fromMarkdown "Situation" False ]
            , definitions = []
            , relatedTerms = []
            , needsUpdating = False
            , lastUpdatedDate = Nothing
            }

    item3 : GlossaryItem
    item3 = { terms = [ Term.fromMarkdown "strong" False ]
            , definitions = []
            , relatedTerms = []
            , needsUpdating = False
            , lastUpdatedDate = Nothing
            }

    fromList [item1, item2, item3]
    |> orderedAlphabetically
    |> Array.toList
    |> List.map (Tuple.second >> .terms >> List.map Term.raw)
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

        primaryTermIdsToIndexes1 : List ( GlossaryItemIndex, GlossaryItem ) -> Dict String GlossaryItemIndex
        primaryTermIdsToIndexes1 =
            List.foldl
                (\( index, item ) result ->
                    item.terms
                        |> List.head
                        |> Maybe.map
                            (\primaryTerm ->
                                Dict.insert (primaryTerm |> Term.id |> TermId.toString) index result
                            )
                        |> Maybe.withDefault result
                )
                Dict.empty
    in
    GlossaryItems <|
        { orderedAlphabetically = Array.fromList alphabetically
        , orderedByMostMentionedFirst = Array.fromList byMostMentionedFirst
        , orderedFocusedOn = Nothing
        , primaryTermIdsToIndexes = primaryTermIdsToIndexes1 alphabetically
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
                    (glossaryItem.terms |> List.map (Term.raw >> Regex.find termAsWord >> List.length) |> List.sum)
                        + (glossaryItem.definitions |> List.map (Definition.raw >> Regex.find termAsWord >> List.length) |> List.sum)
                        + (glossaryItem.relatedTerms |> List.map RelatedTerm.raw |> List.map (Regex.find termAsWord >> List.length) |> List.sum)
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
                        List.map (\term -> ( index, term )) glossaryItem.terms
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
                    itemScore : { a | terms : List Term } -> Int
                    itemScore =
                        .terms
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
                            (item1.terms |> List.head |> Maybe.map Term.raw |> Maybe.withDefault "" |> String.toUpper)
                            (item2.terms |> List.head |> Maybe.map Term.raw |> Maybe.withDefault "" |> String.toUpper)

                    GT ->
                        LT
            )


sanitiseList : List GlossaryItem -> List GlossaryItem
sanitiseList glossaryItems =
    let
        primaryTermIdsSet : Set String
        primaryTermIdsSet =
            glossaryItems
                |> List.concatMap (.terms >> List.take 1)
                |> List.map (Term.id >> TermId.toString)
                |> Set.fromList
    in
    glossaryItems
        |> List.map
            (\glossaryItem ->
                { glossaryItem
                    | relatedTerms =
                        glossaryItem.relatedTerms
                            |> List.filter
                                (\relatedTerm ->
                                    Set.member (relatedTerm |> RelatedTerm.idReference |> TermId.toString) primaryTermIdsSet
                                )
                }
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


{-| Retrieve the glossary items ordered "focused on" a specific item, identified by its primary term.
-}
orderedFocusedOn : TermId -> GlossaryItems -> Maybe (Array ( GlossaryItemIndex, GlossaryItem ))
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


{-| Retrieve a dict mapping each primary term ID to its index in the item list (no matter how it's sorted).
-}
primaryTermIdsToIndexes : GlossaryItems -> Dict String GlossaryItemIndex
primaryTermIdsToIndexes glossaryItems =
    case glossaryItems of
        GlossaryItems items ->
            items.primaryTermIdsToIndexes


{-| Retrieve the list of all terms in the glossary.
-}
terms : GlossaryItems -> List Term
terms =
    orderedAlphabetically
        >> Array.toList
        >> List.concatMap (Tuple.second >> .terms)


{-| Retrieve the list of all primary terms in the glossary.
A _primary term_ is a term that occurs as the first (and possibly only) one in a glossary item.
When adding related terms in the UI, only primary terms are available.
This is to encourage standardizing on one "primary" term for a concept, instead of several synonyms with no clear preferred one.
-}
primaryTerms : GlossaryItems -> List Term
primaryTerms =
    orderedAlphabetically
        >> Array.toList
        >> List.concatMap (Tuple.second >> .terms >> List.take 1)


{-| Make it easy to retrieve the items ordered "focused on" a specific item (identified by the ID of that item's primary term).
Returns an updated `GlossaryItems` where the necessary computations have been done.
-}
enableFocusingOn : TermId -> GlossaryItems -> GlossaryItems
enableFocusingOn termId glossaryItems =
    case glossaryItems of
        GlossaryItems items ->
            let
                primaryTermsGraph : UndirectedGraph TermId
                primaryTermsGraph =
                    items.primaryTermIdsToIndexes
                        |> Dict.keys
                        |> List.foldl
                            (TermId.fromString >> UndirectedGraph.insertVertex)
                            (UndirectedGraph.empty TermId.toString TermId.fromString)

                relatedTermsGraph : UndirectedGraph TermId
                relatedTermsGraph =
                    items.orderedAlphabetically
                        |> Array.toList
                        |> List.map Tuple.second
                        |> List.foldl
                            (\item graph ->
                                item.terms
                                    |> List.head
                                    |> Maybe.map
                                        (\primaryTerm ->
                                            item.relatedTerms
                                                |> List.foldl
                                                    (\relatedTerm graph_ ->
                                                        UndirectedGraph.insertEdge
                                                            (Term.id primaryTerm)
                                                            (RelatedTerm.idReference relatedTerm)
                                                            graph_
                                                    )
                                                    graph
                                        )
                                    |> Maybe.withDefault graph
                            )
                            primaryTermsGraph

                termIdsByDistance : List TermId
                termIdsByDistance =
                    UndirectedGraph.verticesByDistance termId relatedTermsGraph

                itemsByDistance : Array ( GlossaryItemIndex, GlossaryItem )
                itemsByDistance =
                    termIdsByDistance
                        |> List.filterMap
                            (\termId_ ->
                                items.primaryTermIdsToIndexes
                                    |> Dict.get (TermId.toString termId_)
                                    |> Maybe.andThen
                                        (\index ->
                                            Array.get (GlossaryItemIndex.toInt index) items.orderedAlphabetically
                                        )
                            )
                        |> Array.fromList
            in
            GlossaryItems { items | orderedFocusedOn = Just ( termId, itemsByDistance ) }
