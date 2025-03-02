module Data.IndexOfTerms exposing (Entry(..), TermGroup, IndexOfTerms, fromGlossaryItems, termGroups, filterByString)

{-| An index of terms, grouped in alphabetical order by their first character.


# Indexes of Terms

@docs Entry, TermGroup, IndexOfTerms, fromGlossaryItems, termGroups, filterByString

-}

import Data.GlossaryItem.DisambiguatedTerm as DisambiguatedTerm exposing (DisambiguatedTerm)
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItemId exposing (GlossaryItemId)
import Data.GlossaryItemsForUi as GlossaryItemsForUi exposing (GlossaryItemsForUi)
import Data.TagId exposing (TagId)
import Dict exposing (Dict)


{-| An entry in the index.
Alternative terms may be associated with multiple items, so they do not link anywhere.
Instead, each alternative term is followed by the list of all preferred terms for items that the alternative term appears in.
-}
type Entry
    = PreferredTerm GlossaryItemId DisambiguatedTerm
    | AlternativeTerm Term (List ( GlossaryItemId, DisambiguatedTerm ))


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
fromGlossaryItems : Maybe TagId -> GlossaryItemsForUi -> IndexOfTerms
fromGlossaryItems filterByTagId glossaryItemsForUi =
    let
        entryListsByFirstAlphabeticCharacterOrEllipsis : Dict String (List Entry)
        entryListsByFirstAlphabeticCharacterOrEllipsis =
            let
                disambiguatedPreferredTerms : List ( GlossaryItemId, DisambiguatedTerm )
                disambiguatedPreferredTerms =
                    GlossaryItemsForUi.disambiguatedPreferredTerms filterByTagId glossaryItemsForUi

                preferredTermsByAlternativeTerm : List ( Term, List ( GlossaryItemId, DisambiguatedTerm ) )
                preferredTermsByAlternativeTerm =
                    GlossaryItemsForUi.disambiguatedPreferredTermsByAlternativeTerm filterByTagId glossaryItemsForUi

                resultAfterAddingPreferredTerms : Dict String (List Entry)
                resultAfterAddingPreferredTerms =
                    disambiguatedPreferredTerms
                        |> List.foldl
                            (\( itemId, preferredTerm ) result ->
                                Dict.update
                                    (preferredTerm |> DisambiguatedTerm.toTerm |> Term.indexGroupString)
                                    (\termList ->
                                        termList
                                            |> Maybe.map (\terms -> PreferredTerm itemId preferredTerm :: terms)
                                            |> Maybe.withDefault [ PreferredTerm itemId preferredTerm ]
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
                            sortedPreferredTerms : List ( GlossaryItemId, DisambiguatedTerm )
                            sortedPreferredTerms =
                                preferredTerms_
                                    |> List.sortWith
                                        (\( _, t1 ) ( _, t2 ) -> DisambiguatedTerm.compareAlphabetically t1 t2)

                            entry : Entry
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
                entryListsByFirstAlphabeticCharacterOrEllipsis
                alphabetAndEllipsis

        termForEntry : Entry -> Term
        termForEntry entry =
            case entry of
                PreferredTerm _ term ->
                    DisambiguatedTerm.toTerm term

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
termGroups (IndexOfTerms index) =
    index


{-| Filter this index by a given string.
-}
filterByString : String -> IndexOfTerms -> IndexOfTerms
filterByString filterString (IndexOfTerms index) =
    let
        filterStringLower : String
        filterStringLower =
            String.toLower filterString

        filterTermGroup : TermGroup -> TermGroup
        filterTermGroup termGroup =
            let
                entries_ : List Entry
                entries_ =
                    termGroup.entries
                        |> List.filter
                            (\entry ->
                                case entry of
                                    PreferredTerm _ disambiguatedTerm ->
                                        disambiguatedTerm
                                            |> DisambiguatedTerm.toTerm
                                            |> Term.inlineText
                                            |> String.toLower
                                            |> String.contains filterStringLower

                                    AlternativeTerm term _ ->
                                        term
                                            |> Term.inlineText
                                            |> String.toLower
                                            |> String.contains filterStringLower
                            )
            in
            { termGroup | entries = entries_ }
    in
    index
        |> List.map filterTermGroup
        |> IndexOfTerms
