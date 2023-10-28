module Data.IndexOfTerms exposing (Entry(..), TermGroup, IndexOfTerms, fromIncubatingGlossaryItems, termGroups)

{-| An index of terms, grouped in alphabetical order by their first character.


# Indexes of Terms

@docs Entry, TermGroup, IndexOfTerms, fromIncubatingGlossaryItems, termGroups

-}

import Data.GlossaryItem.Term as Term exposing (Term)
import Data.IncubatingGlossaryItems as IncubatingGlossaryItems exposing (IncubatingGlossaryItems)
import Data.TagId exposing (TagId)
import Dict exposing (Dict)


{-| An entry in the index.
Alternative terms may be associated with multiple items, so they do not link anywhere.
Instead, each alternative term is followed by the list of all preferred terms for items that the alternative term appears in.
-}
type Entry
    = PreferredTerm Term
    | AlternativeTerm Term (List Term)


{-| A group of terms, where `label` is the first character of each item in `terms`.
-}
type alias TermGroup =
    { label : String
    , entries : List Entry
    }


{-| An opaque type representing an index of terms.
-}
type IndexOfTerms
    = IndexOfTerms (List TermGroup)


{-| Create an index of terms from glossary items.
-}
fromIncubatingGlossaryItems : Maybe TagId -> IncubatingGlossaryItems -> IndexOfTerms
fromIncubatingGlossaryItems filterByTagId glossaryItems =
    let
        disambiguatedPreferredTerms =
            IncubatingGlossaryItems.disambiguatedPreferredTerms filterByTagId glossaryItems

        preferredTermsByAlternativeTerm =
            IncubatingGlossaryItems.disambiguatedPreferredTermsByAlternativeTerm filterByTagId glossaryItems

        entryListsByFirstAlphabeticCharacterOrEllpisis : Dict String (List Entry)
        entryListsByFirstAlphabeticCharacterOrEllpisis =
            let
                resultAfterAddingPreferredTerms : Dict String (List Entry)
                resultAfterAddingPreferredTerms =
                    disambiguatedPreferredTerms
                        |> List.foldl
                            (\preferredTerm result ->
                                Dict.update
                                    (Term.indexGroupString preferredTerm)
                                    (\termList ->
                                        termList
                                            |> Maybe.map (\terms -> PreferredTerm preferredTerm :: terms)
                                            |> Maybe.withDefault [ PreferredTerm preferredTerm ]
                                            |> Just
                                    )
                                    result
                            )
                            Dict.empty
            in
            preferredTermsByAlternativeTerm
                |> List.foldl
                    (\( alternativeTerm, preferredTerms_ ) result ->
                        let
                            sortedPreferredTerms =
                                preferredTerms_
                                    |> List.sortWith Term.compareAlphabetically

                            entry =
                                AlternativeTerm alternativeTerm sortedPreferredTerms
                        in
                        Dict.update
                            (Term.indexGroupString alternativeTerm)
                            (\entries ->
                                entries
                                    |> Maybe.map (\entries_ -> entry :: entries_)
                                    |> Maybe.withDefault [ entry ]
                                    |> Just
                            )
                            result
                    )
                    resultAfterAddingPreferredTerms

        alphabetAndEllipsis : List String
        alphabetAndEllipsis =
            (List.range (Char.toCode 'A') (Char.toCode 'Z')
                |> List.map (Char.fromCode >> String.fromChar)
            )
                ++ [ "…" ]

        entryListsByFirstCharacterIncludingAlphabetAndEllipsis : Dict String (List Entry)
        entryListsByFirstCharacterIncludingAlphabetAndEllipsis =
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
                entryListsByFirstAlphabeticCharacterOrEllpisis
                alphabetAndEllipsis

        termForEntry : Entry -> Term
        termForEntry entry =
            case entry of
                PreferredTerm term ->
                    term

                AlternativeTerm term _ ->
                    term

        termIndexGroups : List TermGroup
        termIndexGroups =
            entryListsByFirstCharacterIncludingAlphabetAndEllipsis
                |> (\d ->
                        if Dict.get "…" d == Just [] then
                            Dict.remove "…" d

                        else
                            d
                   )
                |> Dict.toList
                |> List.map
                    (Tuple.mapSecond <|
                        List.sortWith
                            (\entry1 entry2 ->
                                Term.compareAlphabetically (termForEntry entry1) (termForEntry entry2)
                            )
                    )
                |> List.map (\( label, entries ) -> TermGroup label entries)
    in
    IndexOfTerms termIndexGroups


{-| Get the list of term groups that make up an index of terms.
-}
termGroups : IndexOfTerms -> List TermGroup
termGroups indexOfTerms =
    case indexOfTerms of
        IndexOfTerms index ->
            index
