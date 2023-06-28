module Search exposing (search)

import Array
import Components.SearchDialog as SearchDialog
import Data.GlossaryItem.Details as Details
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Extras.Html
import Extras.Url
import Html
import Html.Attributes exposing (class)


search : Bool -> String -> GlossaryItems -> List SearchDialog.SearchResult
search enableMathSupport searchString glossaryItems =
    let
        searchStringNormalised : String
        searchStringNormalised =
            searchString |> String.trim |> String.toLower
    in
    if String.isEmpty searchStringNormalised then
        []

    else
        let
            termsAndDetails : List ( Term, Maybe Details.Details )
            termsAndDetails =
                glossaryItems
                    |> GlossaryItems.orderedByMostMentionedFirst
                    |> Array.toList
                    |> List.concatMap
                        (\( _, item ) ->
                            item.terms
                                |> List.map
                                    (\term ->
                                        ( term, List.head item.details )
                                    )
                        )
        in
        termsAndDetails
            |> List.filterMap
                (\( term, details ) ->
                    if String.contains searchStringNormalised (term |> Term.inlineText |> String.toLower) then
                        Just ( term, details )

                    else
                        Nothing
                )
            |> List.partition
                (\( term, _ ) ->
                    String.startsWith searchStringNormalised (term |> Term.inlineText |> String.toLower)
                )
            |> (\( whereSearchStringIsPrefix, whereSearchStringIsNotPrefix ) ->
                    whereSearchStringIsPrefix ++ whereSearchStringIsNotPrefix
               )
            |> List.map
                (\( term, maybeDetails ) ->
                    SearchDialog.searchResult
                        (Extras.Url.fragmentOnly <| Term.id term)
                        [ Html.div
                            []
                            [ Html.p
                                [ class "font-medium" ]
                                [ Term.view enableMathSupport [] term ]
                            , Extras.Html.showMaybe
                                (\details ->
                                    Html.div
                                        [ class "truncate" ]
                                        [ Details.viewInline enableMathSupport [ class "text-sm" ] details
                                        ]
                                )
                                maybeDetails
                            ]
                        ]
                )
