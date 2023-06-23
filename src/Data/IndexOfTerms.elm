module Data.IndexOfTerms exposing (TermGroup, IndexOfTerms, fromGlossaryItems, termGroups)

{-| An index of terms, grouped in alphabetical order by their first character.


# Index of Terms

@docs TermGroup, IndexOfTerms, fromGlossaryItems, termGroups

-}

import Array exposing (Array)
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Dict exposing (Dict)


{-| A group of terms, where `label` is the first character of each item in `terms`.
-}
type alias TermGroup =
    { label : String
    , terms : List Term
    }


{-| An opaque type representing an index of terms.
-}
type IndexOfTerms
    = IndexOfTerms (List TermGroup)


{-| Create an index of terms from glossary items.
-}
fromGlossaryItems : GlossaryItems -> IndexOfTerms
fromGlossaryItems glossaryItems =
    let
        termListsByFirstAlphabeticCharacterOrEllpisis : Dict String (List Term)
        termListsByFirstAlphabeticCharacterOrEllpisis =
            glossaryItems
                |> GlossaryItems.orderedAlphabetically
                |> Array.toList
                |> List.concatMap (Tuple.second >> .terms)
                |> List.foldl
                    (\term result ->
                        let
                            first : String
                            first =
                                Term.indexGroupCharacter term
                        in
                        Dict.update first
                            (\termList ->
                                termList
                                    |> Maybe.map (\terms -> term :: terms)
                                    |> Maybe.withDefault [ term ]
                                    |> Just
                            )
                            result
                    )
                    Dict.empty

        alphabetAndEllipsis : List String
        alphabetAndEllipsis =
            (List.range (Char.toCode 'A') (Char.toCode 'Z')
                |> List.map (Char.fromCode >> String.fromChar)
            )
                ++ [ "…" ]

        termListsByFirstCharacterIncludingAlphabetAndEllipsis : Dict String (List Term)
        termListsByFirstCharacterIncludingAlphabetAndEllipsis =
            List.foldl
                (\letter result ->
                    Dict.update letter
                        (\maybeTermList ->
                            if maybeTermList == Nothing then
                                Just []

                            else
                                maybeTermList
                        )
                        result
                )
                termListsByFirstAlphabeticCharacterOrEllpisis
                alphabetAndEllipsis

        termIndexGroups : List TermGroup
        termIndexGroups =
            termListsByFirstCharacterIncludingAlphabetAndEllipsis
                |> (\d ->
                        if Dict.get "…" d == Just [] then
                            Dict.remove "…" d

                        else
                            d
                   )
                |> Dict.toList
                |> List.map (Tuple.mapSecond <| List.sortWith Term.compareAlphabetically)
                |> List.map (\( label, terms ) -> TermGroup label terms)
    in
    IndexOfTerms termIndexGroups


{-| Get the list of term groups that make up an index of terms.
-}
termGroups : IndexOfTerms -> List TermGroup
termGroups indexOfTerms =
    case indexOfTerms of
        IndexOfTerms index ->
            index
