module Data.GlossaryItems exposing
    ( GlossaryItems, fromList, orderedAlphabetically, orderedByFrequency, get, insert, update, remove, terms, primaryTerms
    , toHtmlTree
    )

{-| A set of glossary items that make up a glossary.


# Glossary Items

@docs GlossaryItems, fromList, orderedAlphabetically, orderedByFrequency, get, insert, update, remove, terms, primaryTerms


# Converting to HTML

@docs toHtmlTree

-}

import Array
import Data.AboutLink as AboutLink exposing (AboutLink)
import Data.GlossaryItem as GlossaryItem exposing (GlossaryItem)
import Data.GlossaryItemIndex as GlossaryItemIndex exposing (GlossaryItemIndex)
import Dict exposing (Dict)
import ElementIds
import Extras.HtmlTree as HtmlTree exposing (HtmlTree(..))
import Extras.Regex
import Regex
import Set


{-| Glossary items constructed by the functions below.
This is done using an opaque type that supports efficiently retrieving the items ordered alphabetically or by frequency.
-}
type GlossaryItems
    = GlossaryItems
        { orderedAlphabetically : List ( GlossaryItemIndex, GlossaryItem )
        , orderedByFrequency : List ( GlossaryItemIndex, GlossaryItem )
        }


{-| Build glossary items from a list.
-}
fromList : List GlossaryItem -> GlossaryItems
fromList glossaryItems =
    let
        sanitised =
            sanitiseList glossaryItems

        alphabetically =
            sanitised
                |> List.sortBy (.terms >> List.head >> Maybe.map .body >> Maybe.withDefault "" >> String.toLower)
                |> zipListWithIndexes

        byFrequency =
            alphabetically
                |> orderListByFrequency
    in
    GlossaryItems <|
        { orderedAlphabetically = alphabetically
        , orderedByFrequency = byFrequency
        }


orderListByFrequency : List ( GlossaryItemIndex, GlossaryItem ) -> List ( GlossaryItemIndex, GlossaryItem )
orderListByFrequency indexedGlossaryItems =
    let
        indexed =
            List.map (Tuple.mapFirst GlossaryItemIndex.toInt) indexedGlossaryItems

        -- Maps a term to a score based on whether or not it occurs in glossaryItem.
        -- This is done in a primitive way. A more sophisticated solution could use stemming
        -- or other techniques.
        termScoreInItem : GlossaryItem.Term -> GlossaryItem -> Int
        termScoreInItem term glossaryItem =
            let
                termAsWord =
                    ("\\b" ++ Extras.Regex.escapeStringForUseInRegex term.body ++ "\\b")
                        |> Regex.fromString
                        |> Maybe.withDefault Regex.never

                score =
                    (glossaryItem.terms |> List.map .body |> List.map (Regex.find termAsWord >> List.length) |> List.sum)
                        + (glossaryItem.details |> List.map (Regex.find termAsWord >> List.length) |> List.sum)
                        + (glossaryItem.relatedTerms |> List.map .body |> List.map (Regex.find termAsWord >> List.length) |> List.sum)
            in
            if score > 0 then
                1

            else
                0

        -- Maps a term to a score based on how often it occurs in glossaryItems.
        termScore : GlossaryItem.Term -> Int -> Int
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
                |> List.map
                    (\( index, glossaryItem ) ->
                        List.map (\term -> ( index, term )) glossaryItem.terms
                    )
                |> List.concat
                |> List.foldl
                    (\( glossaryItemIndex, term ) result ->
                        Dict.insert
                            term.body
                            (termScore term glossaryItemIndex)
                            result
                    )
                    Dict.empty
    in
    indexedGlossaryItems
        |> List.sortWith
            (\( _, item1 ) ( _, item2 ) ->
                let
                    itemScore =
                        .terms
                            >> List.map
                                (\term ->
                                    termBodyScores
                                        |> Dict.get term.body
                                        |> Maybe.withDefault 0
                                )
                            >> List.sum
                in
                case compare (itemScore item1) (itemScore item2) of
                    LT ->
                        GT

                    EQ ->
                        compare
                            (item1.terms |> List.head |> Maybe.map .body |> Maybe.withDefault "" |> String.toLower)
                            (item2.terms |> List.head |> Maybe.map .body |> Maybe.withDefault "" |> String.toLower)

                    GT ->
                        LT
            )


sanitiseList : List GlossaryItem -> List GlossaryItem
sanitiseList glossaryItems =
    let
        primaryTermIdsSet =
            glossaryItems
                |> List.map (.terms >> List.take 1)
                |> List.concat
                |> List.map .id
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
                                    Set.member relatedTerm.idReference primaryTermIdsSet
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


{-| Retrieve the glossary items ordered by frequency.
-}
orderedByFrequency : GlossaryItems -> List ( GlossaryItemIndex, GlossaryItem )
orderedByFrequency glossaryItems =
    case glossaryItems of
        GlossaryItems items ->
            items.orderedByFrequency


{-| Retrieve the list of all terms in the glossary.
-}
terms : GlossaryItems -> List GlossaryItem.Term
terms =
    orderedAlphabetically
        >> List.map (Tuple.second >> .terms)
        >> List.concat


{-| Retrieve the list of all primary terms in the glossary.
A _primary term_ is a term that occurs as the first (and possibly only) one in a glossary item.
When adding related terms in the UI, only primary terms are available.
This is to encourage standardizing on one "primary" term for a concept, instead of several synonyms with no clear preferred one.
-}
primaryTerms : GlossaryItems -> List GlossaryItem.Term
primaryTerms =
    orderedAlphabetically
        >> List.map (Tuple.second >> .terms >> List.take 1)
        >> List.concat


{-| Represent these glossary items as an HTML tree, ready for writing back to the glossary's HTML file.
-}
toHtmlTree : Bool -> String -> String -> List AboutLink -> GlossaryItems -> HtmlTree
toHtmlTree enableHelpForMakingChanges title aboutParagraph aboutLinks glossaryItems =
    HtmlTree.Node "div"
        True
        [ HtmlTree.Attribute "id" ElementIds.container
        , HtmlTree.Attribute "data-enable-help-for-making-changes"
            (if enableHelpForMakingChanges then
                "true"

             else
                "false"
            )
        ]
        [ HtmlTree.Node "header"
            True
            []
            [ HtmlTree.Node "h1"
                True
                [ HtmlTree.Attribute "id" ElementIds.title ]
                [ HtmlTree.Leaf title ]
            ]
        , HtmlTree.Node "main"
            True
            []
            [ HtmlTree.Node "div"
                True
                [ HtmlTree.Attribute "id" ElementIds.about ]
                [ HtmlTree.Node "p"
                    True
                    []
                    [ HtmlTree.Leaf aboutParagraph ]
                , HtmlTree.Node "ul"
                    True
                    []
                    (List.map
                        (\aboutLink ->
                            HtmlTree.Node "li"
                                True
                                []
                                [ HtmlTree.Node "a"
                                    True
                                    [ HtmlTree.Attribute "target" "_blank"
                                    , HtmlTree.Attribute "href" <| AboutLink.href aboutLink
                                    ]
                                    [ HtmlTree.Leaf <| AboutLink.body aboutLink ]
                                ]
                        )
                        aboutLinks
                    )
                ]
            , HtmlTree.Node "article"
                True
                [ HtmlTree.Attribute "id" ElementIds.items ]
                [ HtmlTree.Node "dl"
                    True
                    []
                    (glossaryItems
                        |> orderedAlphabetically
                        |> List.map (Tuple.second >> GlossaryItem.toHtmlTree)
                    )
                ]
            ]
        ]
