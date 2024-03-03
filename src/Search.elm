module Search exposing (search)

import Components.SearchDialog as SearchDialog
import Data.GlossaryItem.Definition as Definition exposing (Definition)
import Data.GlossaryItem.DisambiguatedTerm as DisambiguatedTerm exposing (DisambiguatedTerm)
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItemForHtml as GlossaryItemForHtml
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Data.TagId exposing (TagId)
import Extras.Html
import Extras.Url
import Html
import Html.Attributes exposing (class)
import Icons
import Svg.Attributes


search : Bool -> Maybe TagId -> String -> GlossaryItems -> List SearchDialog.SearchResult
search enableMathSupport filterByTagId searchString glossaryItems =
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
                glossaryItems
                    |> GlossaryItems.orderedAlphabetically filterByTagId
                    |> List.concatMap
                        (\( _, item ) ->
                            let
                                disambiguatedPreferredTerm : DisambiguatedTerm
                                disambiguatedPreferredTerm =
                                    GlossaryItemForHtml.disambiguatedPreferredTerm item

                                definition : Maybe Definition
                                definition =
                                    GlossaryItemForHtml.definition item
                            in
                            { preferredTerm = disambiguatedPreferredTerm
                            , alternativeTerm = Nothing
                            , definition = definition
                            }
                                :: (item
                                        |> GlossaryItemForHtml.alternativeTerms
                                        |> List.map
                                            (\alternativeTerm ->
                                                { preferredTerm = disambiguatedPreferredTerm
                                                , alternativeTerm = Just alternativeTerm
                                                , definition = definition
                                                }
                                            )
                                   )
                        )
                    |> List.sortWith
                        (\candidate1 candidate2 ->
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
        in
        candidates
            |> List.filter
                (\{ preferredTerm, alternativeTerm } ->
                    alternativeTerm
                        |> Maybe.map termContainsSearchString
                        |> Maybe.withDefault
                            (preferredTerm
                                |> DisambiguatedTerm.toTerm
                                |> termContainsSearchString
                            )
                )
            |> List.partition
                (\{ preferredTerm, alternativeTerm } ->
                    alternativeTerm
                        |> Maybe.map termStartsWithSearchString
                        |> Maybe.withDefault
                            (preferredTerm
                                |> DisambiguatedTerm.toTerm
                                |> termStartsWithSearchString
                            )
                )
            |> (\( whereSearchStringIsPrefix, whereSearchStringIsNotPrefix ) ->
                    whereSearchStringIsPrefix ++ whereSearchStringIsNotPrefix
               )
            |> List.map
                (\{ preferredTerm, alternativeTerm, definition } ->
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
