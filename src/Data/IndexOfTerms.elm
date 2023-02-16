module Data.IndexOfTerms exposing (TermGroup, IndexOfTerms, fromGlossaryItems, termGroups)

{-| An index of terms, grouped in alphabetical order by their first character.


# Index of Terms

@docs TermGroup, IndexOfTerms, fromGlossaryItems, termGroups

-}

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
        termListsByFirstCharacter : Dict String (List Term)
        termListsByFirstCharacter =
            glossaryItems
                |> GlossaryItems.orderedAlphabetically
                |> List.concatMap (Tuple.second >> .terms)
                |> List.foldl
                    (\term result ->
                        let
                            firstCharacterUpper =
                                term |> Term.raw |> String.toUpper |> String.left 1
                        in
                        Dict.update
                            firstCharacterUpper
                            (\termList ->
                                termList
                                    |> Maybe.map (\terms -> term :: terms)
                                    |> Maybe.withDefault [ term ]
                                    |> Just
                            )
                            result
                    )
                    Dict.empty

        alphabet : List String
        alphabet =
            List.range (Char.toCode 'A') (Char.toCode 'Z')
                |> List.map (Char.fromCode >> String.fromChar)

        termListsByFirstCharacterIncludingAlphabet : Dict String (List Term)
        termListsByFirstCharacterIncludingAlphabet =
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
                termListsByFirstCharacter
                alphabet

        termIndexGroups : List TermGroup
        termIndexGroups =
            termListsByFirstCharacterIncludingAlphabet
                |> Dict.toList
                |> List.map (Tuple.mapSecond <| List.sortBy <| Term.raw >> String.toUpper)
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
