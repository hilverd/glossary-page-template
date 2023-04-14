module Search exposing (search)

import Components.SearchDialog as SearchDialog
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Extras.Url


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
            terms : List Term
            terms =
                glossaryItems
                    |> GlossaryItems.orderedByFrequency
                    |> List.concatMap (Tuple.second >> .terms)
        in
        terms
            |> List.filterMap
                (\term ->
                    if String.contains searchStringNormalised (term |> Term.inlineText |> String.toLower) then
                        Just term

                    else
                        Nothing
                )
            |> List.map
                (\term ->
                    SearchDialog.searchResult (Extras.Url.fragmentOnly <| Term.id term) [ Term.view enableMathSupport term ]
                )
