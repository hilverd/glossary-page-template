module Search exposing (..)

import Components.SearchDialog as SearchDialog
import Data.GlossaryItem as GlossaryItem
import Data.GlossaryItems as GlossaryItems
import Data.LoadedGlossaryItems exposing (LoadedGlossaryItems)
import Extras.Url


search : String -> LoadedGlossaryItems -> List SearchDialog.SearchResult
search searchTerm loadedGlossaryItems =
    let
        searchTermNormalised =
            searchTerm |> String.trim |> String.toLower

        terms : List GlossaryItem.Term
        terms =
            loadedGlossaryItems
                |> Result.toMaybe
                |> Maybe.map GlossaryItems.orderedAlphabetically
                |> Maybe.withDefault []
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
