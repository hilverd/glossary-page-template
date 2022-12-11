module Search exposing (search)

import Components.SearchDialog as SearchDialog
import Data.GlossaryItem as GlossaryItem
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Extras.Url


search : String -> GlossaryItems -> List SearchDialog.SearchResult
search searchTerm glossaryItems =
    let
        searchTermNormalised =
            searchTerm |> String.trim |> String.toLower

        terms : List GlossaryItem.Term
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
                    if String.contains searchTermNormalised (String.toLower term.body) then
                        Just <| SearchDialog.searchResult (Extras.Url.fragmentOnly term.id) term.body

                    else
                        Nothing
                )
