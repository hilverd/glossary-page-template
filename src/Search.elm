module Search exposing (resultsForItems, viewItemSearchResult)

import Data.GlossaryItem.Definition as Definition exposing (Definition)
import Data.GlossaryItem.DisambiguatedTerm as DisambiguatedTerm exposing (DisambiguatedTerm)
import Data.GlossaryItem.RawTerm as RawTerm
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItemForUi as GlossaryItemForUi
import Data.GlossaryItemsForUi as GlossaryItemsForUi exposing (GlossaryItemsForUi)
import Data.TagId exposing (TagId)
import Extras.Html
import Html exposing (Attribute, Html)
import Html.Attributes exposing (class)
import Icons
import Svg.Attributes


type alias ItemSearchResult =
    { preferredTerm : DisambiguatedTerm
    , alternativeTerm : Maybe Term
    , definition : Maybe Definition
    }


resultsForItems : Maybe TagId -> (ItemSearchResult -> Bool) -> Int -> String -> GlossaryItemsForUi -> { totalNumberOfResults : Int, results : List ItemSearchResult }
resultsForItems filterByTagId filter maximumNumberOfResults searchString glossaryItemsForUi =
    let
        searchStringNormalised : String
        searchStringNormalised =
            searchString |> String.trim |> String.toLower
    in
    if String.isEmpty searchStringNormalised then
        { totalNumberOfResults = 0, results = [] }

    else
        let
            candidates :
                List
                    { preferredTerm : DisambiguatedTerm
                    , alternativeTerm : Maybe Term
                    , definition : Maybe Definition
                    }
            candidates =
                glossaryItemsForUi
                    |> GlossaryItemsForUi.orderedAlphabetically filterByTagId
                    |> List.concatMap
                        (\( _, item ) ->
                            let
                                disambiguatedPreferredTerm : DisambiguatedTerm
                                disambiguatedPreferredTerm =
                                    GlossaryItemForUi.disambiguatedPreferredTerm item

                                definition : Maybe Definition
                                definition =
                                    GlossaryItemForUi.definition item
                            in
                            { preferredTerm = disambiguatedPreferredTerm
                            , alternativeTerm = Nothing
                            , definition = definition
                            }
                                :: (item
                                        |> GlossaryItemForUi.alternativeTerms
                                        |> List.map
                                            (\alternativeTerm ->
                                                { preferredTerm = disambiguatedPreferredTerm
                                                , alternativeTerm = Just alternativeTerm
                                                , definition = definition
                                                }
                                            )
                                   )
                        )

            termRawContainsSearchString : Term -> Bool
            termRawContainsSearchString =
                Term.raw
                    >> RawTerm.toString
                    >> String.toLower
                    >> String.contains searchStringNormalised

            termInlineTextContainsSearchString : Term -> Bool
            termInlineTextContainsSearchString =
                Term.inlineText
                    >> String.toLower
                    >> String.contains searchStringNormalised

            termRawStartsWithSearchString : Term -> Bool
            termRawStartsWithSearchString =
                Term.raw
                    >> RawTerm.toString
                    >> String.toLower
                    >> String.startsWith searchStringNormalised

            termInlineTextStartsWithSearchString : Term -> Bool
            termInlineTextStartsWithSearchString =
                Term.inlineText
                    >> String.toLower
                    >> String.startsWith searchStringNormalised

            definitionInlineTextContainsSearchString : Definition -> Bool
            definitionInlineTextContainsSearchString =
                Definition.inlineText
                    >> String.toLower
                    >> String.contains searchStringNormalised
        in
        candidates
            |> List.filter filter
            |> List.filterMap
                (\({ preferredTerm, alternativeTerm, definition } as candidate) ->
                    if
                        Maybe.map termRawContainsSearchString alternativeTerm
                            == Just True
                            || Maybe.map termInlineTextContainsSearchString alternativeTerm
                            == Just True
                    then
                        if Maybe.map termRawStartsWithSearchString alternativeTerm == Just True || Maybe.map termInlineTextStartsWithSearchString alternativeTerm == Just True then
                            Just ( candidate, 3 )

                        else
                            Just ( candidate, 2 )

                    else
                        let
                            preferredTermAsTerm : Term
                            preferredTermAsTerm =
                                DisambiguatedTerm.toTerm preferredTerm
                        in
                        if termRawContainsSearchString preferredTermAsTerm || termInlineTextContainsSearchString preferredTermAsTerm then
                            if termRawStartsWithSearchString preferredTermAsTerm || termInlineTextStartsWithSearchString preferredTermAsTerm then
                                Just ( candidate, 3 )

                            else
                                Just ( candidate, 2 )

                        else if alternativeTerm == Nothing && Maybe.map definitionInlineTextContainsSearchString definition == Just True then
                            Just ( candidate, 1 )

                        else
                            Nothing
                )
            |> List.sortWith
                (\( candidate1, rank1 ) ( candidate2, rank2 ) ->
                    if rank1 > rank2 then
                        LT

                    else if rank1 < rank2 then
                        GT

                    else
                        case ( candidate1.alternativeTerm, candidate2.alternativeTerm ) of
                            ( Just alternativeTerm1, Just alternativeTerm2 ) ->
                                if alternativeTerm1 == alternativeTerm2 then
                                    DisambiguatedTerm.compareAlphabetically candidate1.preferredTerm candidate2.preferredTerm

                                else
                                    Term.compareAlphabetically alternativeTerm1 alternativeTerm2

                            ( Just alternativeTerm1, Nothing ) ->
                                Term.compareAlphabetically alternativeTerm1 (DisambiguatedTerm.toTerm candidate2.preferredTerm)

                            ( Nothing, Just alternativeTerm2 ) ->
                                Term.compareAlphabetically (DisambiguatedTerm.toTerm candidate1.preferredTerm) alternativeTerm2

                            ( Nothing, Nothing ) ->
                                DisambiguatedTerm.compareAlphabetically candidate1.preferredTerm candidate2.preferredTerm
                )
            |> (\rawResults ->
                    { totalNumberOfResults = List.length rawResults
                    , results =
                        rawResults
                            |> List.take maximumNumberOfResults
                            |> List.map Tuple.first
                    }
               )


viewItemSearchResult : Bool -> List (Attribute msg) -> ItemSearchResult -> Html msg
viewItemSearchResult enableMathSupport additionalAttributes { preferredTerm, alternativeTerm, definition } =
    case alternativeTerm of
        Just alternativeTerm_ ->
            Html.div
                [ class "flex flex-col" ]
                [ Html.div
                    [ class "font-medium" ]
                    [ Term.view enableMathSupport additionalAttributes alternativeTerm_ ]
                , Html.div
                    [ class "inline-flex items-center group-hover:underline font-medium" ]
                    [ Icons.cornerDownRight
                        [ Svg.Attributes.class "h-5 w-5 shrink-0 pb-0.5 mr-1.5 text-gray-400 dark:text-gray-400"
                        ]
                    , Term.view enableMathSupport additionalAttributes (DisambiguatedTerm.toTerm preferredTerm)
                    ]
                , Extras.Html.showMaybe
                    (\definition_ ->
                        Html.div
                            [ if enableMathSupport then
                                class "overflow-hidden whitespace-nowrap"

                              else
                                class "truncate"
                            ]
                            [ Definition.viewInline enableMathSupport additionalAttributes definition_
                            ]
                    )
                    definition
                ]

        Nothing ->
            Html.div
                []
                [ Html.p
                    [ class "font-medium" ]
                    [ Term.view enableMathSupport additionalAttributes (DisambiguatedTerm.toTerm preferredTerm) ]
                , Extras.Html.showMaybe
                    (\definition_ ->
                        Html.div
                            [ if enableMathSupport then
                                class "overflow-hidden whitespace-nowrap"

                              else
                                class "truncate"
                            ]
                            [ Definition.viewInline enableMathSupport additionalAttributes definition_
                            ]
                    )
                    definition
                ]
