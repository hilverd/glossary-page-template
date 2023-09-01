module Search exposing (search)

import Array
import Components.SearchDialog as SearchDialog
import Data.GlossaryItem as GlossaryItem
import Data.GlossaryItem.Definition as Definition exposing (Definition)
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItem.TermId as TermId
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
            termsAndDefinitions : List ( Term, Maybe Definition )
            termsAndDefinitions =
                glossaryItems
                    |> GlossaryItems.orderedByMostMentionedFirst
                    |> Array.toList
                    |> List.concatMap
                        (\( _, item ) ->
                            item
                                |> GlossaryItem.allTerms
                                |> List.map
                                    (\term ->
                                        ( term, item |> GlossaryItem.definitions |> List.head )
                                    )
                        )
        in
        termsAndDefinitions
            |> List.filterMap
                (\( term, definition ) ->
                    if String.contains searchStringNormalised (term |> Term.inlineText |> String.toLower) then
                        Just ( term, definition )

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
                (\( term, maybeDefinition ) ->
                    SearchDialog.searchResult
                        (Extras.Url.fragmentOnly <| TermId.toString <| Term.id term)
                        [ Html.div
                            []
                            [ Html.p
                                [ class "font-medium" ]
                                [ Term.view enableMathSupport [] term ]
                            , Extras.Html.showMaybe
                                (\definition ->
                                    Html.div
                                        [ if enableMathSupport then
                                            class "overflow-hidden whitespace-nowrap"

                                          else
                                            class "truncate"
                                        ]
                                        [ Definition.viewInline enableMathSupport [] definition
                                        ]
                                )
                                maybeDefinition
                            ]
                        ]
                )
