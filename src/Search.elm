module Search exposing (search)

import Components.SearchDialog as SearchDialog
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Extras.Url


search : Bool -> String -> GlossaryItems -> List SearchDialog.SearchResult
search enableMathSupport searchTerm glossaryItems =
    let
        searchTermNormalised : String
        searchTermNormalised =
            searchTerm |> String.trim |> String.toLower
    in
    if String.isEmpty searchTermNormalised then
        []

    else
        let
            terms : List Term
            terms =
                glossaryItems
                    |> GlossaryItems.orderedByFrequency
                    |> List.concatMap (Tuple.second >> .terms)
        in
        terms
            |> List.filterMap
                (\term ->
                    if String.contains searchTermNormalised (term |> Term.inlineText |> String.toLower) then
                        Just <| SearchDialog.searchResult (Extras.Url.fragmentOnly <| Term.id term) [ Term.view enableMathSupport term ]

                    else
                        Nothing
                )
