module Data.IndexOfTerms exposing (TermGroup, IndexOfTerms, fromGlossaryItems, termGroups)

{-| An index of terms, grouped in alphabetical order by their first character.


# Index of Terms

@docs TermGroup, IndexOfTerms, fromGlossaryItems, termGroups

-}

import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Dict exposing (Dict)
import Extras.String
import String.Normalize


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


compareTerms : Term -> Term -> Order
compareTerms term1 term2 =
    let
        raw1 =
            term1 |> Term.raw |> String.Normalize.removeDiacritics |> String.toUpper

        raw2 =
            term2 |> Term.raw |> String.Normalize.removeDiacritics |> String.toUpper

        first1 =
            raw1 |> Extras.String.firstAlphabeticCharacter |> Maybe.withDefault ""

        first2 =
            raw2 |> Extras.String.firstAlphabeticCharacter |> Maybe.withDefault ""
    in
    case
        compare first1 first2
    of
        LT ->
            LT

        GT ->
            GT

        EQ ->
            compare raw1 raw2


{-| Create an index of terms from glossary items.
-}
fromGlossaryItems : GlossaryItems -> IndexOfTerms
fromGlossaryItems glossaryItems =
    let
        termListsByFirstAlphabeticCharacterOrEllpisis : Dict String (List Term)
        termListsByFirstAlphabeticCharacterOrEllpisis =
            glossaryItems
                |> GlossaryItems.orderedAlphabetically
                |> List.concatMap (Tuple.second >> .terms)
                |> List.foldl
                    (\term result ->
                        let
                            first : String
                            first =
                                Term.groupCharacter term
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
                |> List.map (Tuple.mapSecond <| List.sortWith compareTerms)
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
