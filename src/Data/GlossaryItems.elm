module Data.GlossaryItems exposing (GlossaryItems, orderAlphabetically, orderByFrequency, sanitise, toHtmlTree)

import Data.GlossaryItem as GlossaryItem exposing (GlossaryItem)
import Dict exposing (Dict)
import Extras.HtmlTree as HtmlTree exposing (HtmlTree(..))
import Extras.Regex
import Regex
import Set


type alias GlossaryItems =
    List GlossaryItem


toHtmlTree : Bool -> GlossaryItems -> HtmlTree
toHtmlTree enableHelpForMakingChanges glossaryItems =
    HtmlTree.Node "article"
        True
        [ HtmlTree.Attribute "id" "glossary"
        , HtmlTree.Attribute "data-enable-help-for-making-changes"
            (if enableHelpForMakingChanges then
                "true"

             else
                "false"
            )
        ]
        [ HtmlTree.Node "dl"
            True
            []
            (List.map GlossaryItem.toHtmlTree glossaryItems)
        ]


sanitise : GlossaryItems -> GlossaryItems
sanitise glossaryItems =
    let
        termIdsSet =
            glossaryItems
                |> List.map .terms
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
                            |> List.filter (\relatedTerm -> Set.member relatedTerm.idReference termIdsSet)
                }
            )


orderAlphabetically : GlossaryItems -> GlossaryItems
orderAlphabetically =
    List.sortBy (.terms >> List.head >> Maybe.map .body >> Maybe.withDefault "" >> String.toLower)


orderByFrequency : GlossaryItems -> GlossaryItems
orderByFrequency glossaryItems =
    let
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
            glossaryItems
                |> List.indexedMap Tuple.pair
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
            glossaryItems
                |> List.indexedMap
                    (\index glossaryItem ->
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
    glossaryItems
        |> List.sortWith
            (\item1 item2 ->
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
