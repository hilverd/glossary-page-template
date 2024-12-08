module Search exposing (search)

import Components.SearchDialog as SearchDialog
import Data.GlossaryItem.Definition as Definition exposing (Definition)
import Data.GlossaryItem.DisambiguatedTerm as DisambiguatedTerm exposing (DisambiguatedTerm)
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItemForUi as GlossaryItemForUi
import Data.GlossaryItemsForUi as GlossaryItemsForUi exposing (GlossaryItemsForUi)
import Data.TagId exposing (TagId)
import Extras.Html
import Extras.Url
import Html
import Html.Attributes exposing (class)
import Icons
import Svg.Attributes


search : Bool -> Maybe TagId -> String -> GlossaryItemsForUi -> List SearchDialog.SearchResult
search enableMathSupport filterByTagId searchString glossaryItemsForUi =
    let
        searchStringNormalised : String
        searchStringNormalised =
            searchString |> String.trim |> String.toLower
    in
    if String.isEmpty searchStringNormalised then
        []

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

            termContainsSearchString : Term -> Bool
            termContainsSearchString =
                Term.inlineText
                    >> String.toLower
                    >> String.contains searchStringNormalised

            termStartsWithSearchString : Term -> Bool
            termStartsWithSearchString =
                Term.inlineText
                    >> String.toLower
                    >> String.startsWith searchStringNormalised

            definitionContainsSearchString : Definition -> Bool
            definitionContainsSearchString =
                Definition.inlineText
                    >> String.toLower
                    >> String.contains searchStringNormalised
        in
        candidates
            |> List.filterMap
                (\({ preferredTerm, alternativeTerm, definition } as candidate) ->
                    if Maybe.map termContainsSearchString alternativeTerm == Just True then
                        if Maybe.map termStartsWithSearchString alternativeTerm == Just True then
                            Just ( candidate, 3 )

                        else
                            Just ( candidate, 2 )

                    else
                        let
                            preferredTermAsTerm : Term
                            preferredTermAsTerm =
                                DisambiguatedTerm.toTerm preferredTerm
                        in
                        if termContainsSearchString preferredTermAsTerm then
                            if termStartsWithSearchString preferredTermAsTerm then
                                Just ( candidate, 3 )

                            else
                                Just ( candidate, 2 )

                        else if alternativeTerm == Nothing && Maybe.map definitionContainsSearchString definition == Just True then
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
            |> List.take 40
            |> List.map
                (\( { preferredTerm, alternativeTerm, definition }, _ ) ->
                    case alternativeTerm of
                        Just alternativeTerm_ ->
                            SearchDialog.searchResult
                                (Extras.Url.fragmentOnly <| Term.id <| DisambiguatedTerm.toTerm preferredTerm)
                                [ Html.div
                                    [ class "flex flex-col" ]
                                    [ Html.div
                                        [ class "font-medium" ]
                                        [ Term.view enableMathSupport [] alternativeTerm_ ]
                                    , Html.div
                                        [ class "inline-flex items-center group-hover:underline font-medium" ]
                                        [ Icons.cornerDownRight
                                            [ Svg.Attributes.class "h-5 w-5 shrink-0 pb-0.5 mr-1.5 text-gray-400 dark:text-gray-400"
                                            ]
                                        , Term.view enableMathSupport [] (DisambiguatedTerm.toTerm preferredTerm)
                                        ]
                                    , Extras.Html.showMaybe
                                        (\definition_ ->
                                            Html.div
                                                [ if enableMathSupport then
                                                    class "overflow-hidden whitespace-nowrap"

                                                  else
                                                    class "truncate"
                                                ]
                                                [ Definition.viewInline enableMathSupport [] definition_
                                                ]
                                        )
                                        definition
                                    ]
                                ]

                        Nothing ->
                            SearchDialog.searchResult
                                (Extras.Url.fragmentOnly <| Term.id <| DisambiguatedTerm.toTerm preferredTerm)
                                [ Html.div
                                    []
                                    [ Html.p
                                        [ class "font-medium" ]
                                        [ Term.view enableMathSupport [] (DisambiguatedTerm.toTerm preferredTerm) ]
                                    , Extras.Html.showMaybe
                                        (\definition_ ->
                                            Html.div
                                                [ if enableMathSupport then
                                                    class "overflow-hidden whitespace-nowrap"

                                                  else
                                                    class "truncate"
                                                ]
                                                [ Definition.viewInline enableMathSupport [] definition_
                                                ]
                                        )
                                        definition
                                    ]
                                ]
                )
