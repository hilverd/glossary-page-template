module Search exposing (..)

import Components.SearchDialog as SearchDialog
import Data.GlossaryItem as GlossaryItem
import Extras.Url


search : String -> List GlossaryItem.Term -> List SearchDialog.SearchResult
search searchTerm terms =
    let
        searchTermNormalised =
            searchTerm |> String.trim |> String.toLower
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
