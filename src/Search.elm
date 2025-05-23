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
    { disambiguatedPreferredTerm : DisambiguatedTerm
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
                    { disambiguatedPreferredTerm : DisambiguatedTerm
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
                            { disambiguatedPreferredTerm = disambiguatedPreferredTerm
                            , alternativeTerm = Nothing
                            , definition = definition
                            }
                                :: (item
                                        |> GlossaryItemForUi.alternativeTerms
                                        |> List.map
                                            (\alternativeTerm ->
                                                { disambiguatedPreferredTerm = disambiguatedPreferredTerm
                                                , alternativeTerm = Just alternativeTerm
                                                , definition = definition
                                                }
                                            )
                                   )
                        )

            termRawStartsWithSearchString : Term -> Bool
            termRawStartsWithSearchString =
                Term.raw
                    >> RawTerm.toString
                    >> String.toLower
                    >> String.startsWith searchStringNormalised

            termRawContainsSearchString : Term -> Bool
            termRawContainsSearchString =
                Term.raw
                    >> RawTerm.toString
                    >> String.toLower
                    >> String.contains searchStringNormalised

            termInlineTextStartsWithSearchString : Term -> Bool
            termInlineTextStartsWithSearchString =
                Term.inlineText
                    >> String.toLower
                    >> String.startsWith searchStringNormalised

            termInlineTextContainsSearchString : Term -> Bool
            termInlineTextContainsSearchString =
                Term.inlineText
                    >> String.toLower
                    >> String.contains searchStringNormalised

            definitionInlineTextContainsSearchString : Definition -> Bool
            definitionInlineTextContainsSearchString =
                Definition.inlineText
                    >> String.toLower
                    >> String.contains searchStringNormalised
        in
        candidates
            |> List.filter filter
            |> List.filterMap
                (\({ disambiguatedPreferredTerm, alternativeTerm, definition } as candidate) ->
                    let
                        disambiguatedPreferredTermAsTerm : Term
                        disambiguatedPreferredTermAsTerm =
                            DisambiguatedTerm.toTerm disambiguatedPreferredTerm
                    in
                    if alternativeTerm == Nothing && termRawStartsWithSearchString disambiguatedPreferredTermAsTerm then
                        Just ( candidate, 1 )

                    else if alternativeTerm == Nothing && termInlineTextStartsWithSearchString disambiguatedPreferredTermAsTerm then
                        Just ( candidate, 2 )

                    else if Maybe.map termRawStartsWithSearchString alternativeTerm == Just True then
                        Just ( candidate, 3 )

                    else if Maybe.map termInlineTextStartsWithSearchString alternativeTerm == Just True then
                        Just ( candidate, 4 )

                    else if alternativeTerm == Nothing && termRawContainsSearchString disambiguatedPreferredTermAsTerm then
                        Just ( candidate, 5 )

                    else if alternativeTerm == Nothing && termInlineTextContainsSearchString disambiguatedPreferredTermAsTerm then
                        Just ( candidate, 6 )

                    else if Maybe.map termRawContainsSearchString alternativeTerm == Just True then
                        Just ( candidate, 7 )

                    else if Maybe.map termInlineTextContainsSearchString alternativeTerm == Just True then
                        Just ( candidate, 8 )

                    else if alternativeTerm == Nothing && Maybe.map definitionInlineTextContainsSearchString definition == Just True then
                        Just ( candidate, 9 )

                    else
                        Nothing
                )
            |> List.sortBy Tuple.second
            |> (\rawResults ->
                    { totalNumberOfResults = List.length rawResults
                    , results =
                        rawResults
                            |> List.take maximumNumberOfResults
                            |> List.map Tuple.first
                    }
               )


viewItemSearchResult : Bool -> List (Attribute msg) -> ItemSearchResult -> Html msg
viewItemSearchResult enableMathSupport additionalAttributes { disambiguatedPreferredTerm, alternativeTerm, definition } =
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
                    , Term.view enableMathSupport additionalAttributes (DisambiguatedTerm.toTerm disambiguatedPreferredTerm)
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
                    [ Term.view enableMathSupport additionalAttributes (DisambiguatedTerm.toTerm disambiguatedPreferredTerm) ]
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
