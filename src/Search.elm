module Search exposing (search)

import Components.SearchDialog as SearchDialog
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Extras.Url


search : String -> GlossaryItems -> List SearchDialog.SearchResult
search searchTerm glossaryItems =
    let
        searchTermNormalised =
            searchTerm |> String.trim |> String.toLower

        terms : List Term
        terms =
            glossaryItems
                |> GlossaryItems.orderedByFrequency
                |> List.concatMap (Tuple.second >> .terms)
    in
    if String.isEmpty searchTermNormalised then
        []

    else
        terms
            |> List.filterMap
                (\term ->
                    if String.contains searchTermNormalised (String.toLower (Term.raw term)) then
                        Just <| SearchDialog.searchResult (Extras.Url.fragmentOnly <| Term.id term) (Term.raw term)

                    else
                        Nothing
                )
