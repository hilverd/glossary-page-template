module Data.GlossaryItems exposing (GlossaryItems, fromList, orderedAlphabetically, orderedByMostMentionedFirst, get, insert, update, remove, terms, primaryTerms)

{-| A set of glossary items that make up a glossary.


# Glossary Items

@docs GlossaryItems, fromList, orderedAlphabetically, orderedByMostMentionedFirst, get, insert, update, remove, terms, primaryTerms

-}

import Array
import Data.GlossaryItem exposing (GlossaryItem)
import Data.GlossaryItem.Details as Details
import Data.GlossaryItem.RelatedTerm as RelatedTerm
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItemIndex as GlossaryItemIndex exposing (GlossaryItemIndex)
import Dict exposing (Dict)
import Extras.Regex
import Regex
import Set


{-| Glossary items constructed by the functions below.
This is done using an opaque type that supports efficiently retrieving the items ordered alphabetically or by most mentioned first.
-}
type GlossaryItems
    = GlossaryItems
        { orderedAlphabetically : List ( GlossaryItemIndex, GlossaryItem )
        , orderedByMostMentionedFirst : List ( GlossaryItemIndex, GlossaryItem )
        }


compareForSortingAlphabetically : GlossaryItem -> GlossaryItem -> Order
compareForSortingAlphabetically item1 item2 =
    Term.compareAlphabetically
        (item1.terms |> List.head |> Maybe.withDefault Term.emptyPlaintext)
        (item2.terms |> List.head |> Maybe.withDefault Term.emptyPlaintext)


{-| Build glossary items from a list.

    import Data.GlossaryItem exposing (GlossaryItem)
    import Data.GlossaryItem.Term as Term exposing (Term)

    item1 : GlossaryItem
    item1 = { terms = [ Term.fromMarkdown "\\_\\_slots\\_\\_" False ]
            , details = []
            , relatedTerms = []
            , needsUpdating = False
            }

    item2 : GlossaryItem
    item2 = { terms = [ Term.fromMarkdown "Situation" False ]
            , details = []
            , relatedTerms = []
            , needsUpdating = False
            }

    item3 : GlossaryItem
    item3 = { terms = [ Term.fromMarkdown "strong" False ]
            , details = []
            , relatedTerms = []
            , needsUpdating = False
            }

    fromList [item1, item2, item3]
    |> orderedAlphabetically
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
    in
    GlossaryItems <|
        { orderedAlphabetically = alphabetically
        , orderedByMostMentionedFirst = byMostMentionedFirst
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
                        + (glossaryItem.details |> List.map (Details.raw >> Regex.find termAsWord >> List.length) |> List.sum)
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
        primaryTermIdsSet : Set.Set String
        primaryTermIdsSet =
            glossaryItems
                |> List.concatMap (.terms >> List.take 1)
                |> List.map Term.id
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
                                    Set.member (RelatedTerm.idReference relatedTerm) primaryTermIdsSet
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
        |> List.map Tuple.second
        |> Array.fromList
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
                |> List.map Tuple.second
    in
    glossaryItem :: itemsList |> fromList


{-| Replace the glossary item at the given index, assuming alphabetical order.
-}
update : GlossaryItemIndex -> GlossaryItem -> GlossaryItems -> GlossaryItems
update indexWhenOrderedAlphabetically glossaryItem glossaryItems =
    glossaryItems
        |> orderedAlphabetically
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
orderedAlphabetically : GlossaryItems -> List ( GlossaryItemIndex, GlossaryItem )
orderedAlphabetically glossaryItems =
    case glossaryItems of
        GlossaryItems items ->
            items.orderedAlphabetically


{-| Retrieve the glossary items ordered by most mentioned first.
-}
orderedByMostMentionedFirst : GlossaryItems -> List ( GlossaryItemIndex, GlossaryItem )
orderedByMostMentionedFirst glossaryItems =
    case glossaryItems of
        GlossaryItems items ->
            items.orderedByMostMentionedFirst


{-| Retrieve the list of all terms in the glossary.
-}
terms : GlossaryItems -> List Term
terms =
    orderedAlphabetically
        >> List.concatMap (Tuple.second >> .terms)


{-| Retrieve the list of all primary terms in the glossary.
A _primary term_ is a term that occurs as the first (and possibly only) one in a glossary item.
When adding related terms in the UI, only primary terms are available.
This is to encourage standardizing on one "primary" term for a concept, instead of several synonyms with no clear preferred one.
-}
primaryTerms : GlossaryItems -> List Term
primaryTerms =
    orderedAlphabetically
        >> List.concatMap (Tuple.second >> .terms >> List.take 1)
