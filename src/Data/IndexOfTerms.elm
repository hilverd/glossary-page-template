module Data.IndexOfTerms exposing (Entry(..), TermGroup, IndexOfTerms, fromGlossaryItems, termGroups)

{-| An index of terms, grouped in alphabetical order by their first character.


# Indexes of Terms

@docs Entry, TermGroup, IndexOfTerms, fromGlossaryItems, termGroups

-}

import Array
import Data.GlossaryItem as GlossaryItem exposing (alternativeTerms, preferredTerm)
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItem.TermId as TermId
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
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
fromGlossaryItems : GlossaryItems -> IndexOfTerms
fromGlossaryItems glossaryItems =
    let
        preferredTermsByAlternativeTermId : Dict String (List Term)
        preferredTermsByAlternativeTermId =
            glossaryItems
                |> GlossaryItems.orderedAlphabetically
                |> Array.map Tuple.second
                |> Array.foldl
                    (\item result ->
                        let
                            preferredTerm =
                                GlossaryItem.preferredTerm item

                            alternativeTerms_ =
                                GlossaryItem.alternativeTerms item
                        in
                        alternativeTerms_
                            |> List.foldl
                                (\alternativeTerm result_ ->
                                    Dict.update
                                        (alternativeTerm |> Term.id |> TermId.toString)
                                        (\preferredTerms_ ->
                                            preferredTerms_
                                                |> Maybe.map (\terms -> preferredTerm :: terms)
                                                |> Maybe.withDefault [ preferredTerm ]
                                                |> Just
                                        )
                                        result_
                                )
                                result
                    )
                    Dict.empty

        preferredTerms : List Term
        preferredTerms =
            glossaryItems
                |> GlossaryItems.orderedAlphabetically
                |> Array.toList
                |> List.map (Tuple.second >> GlossaryItem.preferredTerm)

        alternativeTerms : List Term
        alternativeTerms =
            glossaryItems
                |> GlossaryItems.orderedAlphabetically
                |> Array.map Tuple.second
                |> Array.foldl
                    (\item result ->
                        item
                            |> GlossaryItem.alternativeTerms
                            |> List.foldl
                                (\alternativeTerm ->
                                    Dict.insert
                                        (alternativeTerm |> Term.id |> TermId.toString)
                                        alternativeTerm
                                )
                                result
                    )
                    Dict.empty
                |> Dict.values

        entryListsByFirstAlphabeticCharacterOrEllpisis : Dict String (List Entry)
        entryListsByFirstAlphabeticCharacterOrEllpisis =
            let
                resultAfterAddingPreferredTerms : Dict String (List Entry)
                resultAfterAddingPreferredTerms =
                    preferredTerms
                        |> List.foldl
                            (\preferredTerm result ->
                                Dict.update
                                    (Term.indexGroupCharacter preferredTerm)
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
            alternativeTerms
                |> List.foldl
                    (\alternativeTerm result ->
                        let
                            preferredTermsForThisAlternativeTerm : List Term
                            preferredTermsForThisAlternativeTerm =
                                Dict.get
                                    (alternativeTerm |> Term.id |> TermId.toString)
                                    preferredTermsByAlternativeTermId
                                    |> Maybe.withDefault []
                                    |> List.sortWith Term.compareAlphabetically

                            entry =
                                AlternativeTerm alternativeTerm preferredTermsForThisAlternativeTerm
                        in
                        Dict.update
                            (Term.indexGroupCharacter alternativeTerm)
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
