module Data.GlossaryItems exposing
    ( GlossaryItems
    , fromList
    , get
    , insert
    , orderedAlphabetically
    , orderedByFrequency
    , primaryTerms
    , remove
    , terms
    , toHtmlTree
    , update
    )

import Array
import Data.AboutLink as AboutLink exposing (AboutLink)
import Data.GlossaryItem as GlossaryItem exposing (GlossaryItem)
import Data.GlossaryItemIndex as GlossaryItemIndex exposing (GlossaryItemIndex)
import Dict exposing (Dict)
import ElementIds
import Extras.HtmlTree as HtmlTree exposing (HtmlTree(..))
import Extras.Regex
import Regex
import Set exposing (Set)


type GlossaryItems
    = GlossaryItems
        { orderedAlphabetically : List ( GlossaryItemIndex, GlossaryItem )
        , orderedByFrequency : List ( GlossaryItemIndex, GlossaryItem )
        }


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


get : GlossaryItemIndex -> GlossaryItems -> Maybe GlossaryItem
get indexWhenOrderedAlphabetically glossaryItems =
    glossaryItems
        |> orderedAlphabetically
        |> List.map Tuple.second
        |> Array.fromList
        |> Array.get (GlossaryItemIndex.toInt indexWhenOrderedAlphabetically)


insert : GlossaryItem -> GlossaryItems -> GlossaryItems
insert glossaryItem glossaryItems =
    let
        itemsList =
            glossaryItems
                |> orderedAlphabetically
                |> List.map Tuple.second
    in
    glossaryItem :: itemsList |> fromList


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


orderedAlphabetically : GlossaryItems -> List ( GlossaryItemIndex, GlossaryItem )
orderedAlphabetically glossaryItems =
    case glossaryItems of
        GlossaryItems items ->
            items.orderedAlphabetically


orderedByFrequency : GlossaryItems -> List ( GlossaryItemIndex, GlossaryItem )
orderedByFrequency glossaryItems =
    case glossaryItems of
        GlossaryItems items ->
            items.orderedByFrequency


terms : GlossaryItems -> List GlossaryItem.Term
terms =
    orderedAlphabetically
        >> List.map (Tuple.second >> .terms)
        >> List.concat


primaryTerms : GlossaryItems -> List GlossaryItem.Term
primaryTerms =
    orderedAlphabetically
        >> List.map (Tuple.second >> .terms >> List.take 1)
        >> List.concat


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
