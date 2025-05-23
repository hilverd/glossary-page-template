module RelatedTermSuggestions exposing (suggest)

import Data.GlossaryItem.DisambiguatedTerm as DisambiguatedTerm exposing (DisambiguatedTerm)
import Data.GlossaryItem.RawTerm as RawTerm
import Data.GlossaryItem.Term as Term
import Data.GlossaryItemOutline exposing (GlossaryItemOutline)
import Extras.Regex
import Regex
import Set exposing (Set)


suggest :
    Set String
    -> Set String
    -> List GlossaryItemOutline
    -> String
    -> List DisambiguatedTerm
suggest relatedRawTermsAlreadyInForm rawDisambiguatedPreferredTermsOfItemsListingThisItemAsRelated outlinesOfItemsOutside definitionFieldBody =
    let
        outlinesOfCandidateItems : List GlossaryItemOutline
        outlinesOfCandidateItems =
            outlinesOfItemsOutside
                |> List.filter
                    (\outlineOfItemOutside ->
                        not <| Set.member outlineOfItemOutside.disambiguatedPreferredTerm relatedRawTermsAlreadyInForm
                    )

        candidateTerms : List DisambiguatedTerm
        candidateTerms =
            outlinesOfCandidateItems
                |> List.map
                    (\itemOutline ->
                        Term.fromMarkdown itemOutline.disambiguatedPreferredTerm False
                            |> DisambiguatedTerm.fromTerm
                    )

        definitionFieldBodyLower : String
        definitionFieldBodyLower =
            String.toLower definitionFieldBody
    in
    candidateTerms
        |> List.filter
            (\candidateTerm ->
                let
                    candidateTermAsWord : Regex.Regex
                    candidateTermAsWord =
                        ("(\\b| )"
                            ++ Extras.Regex.escapeStringForUseInRegex
                                (String.toLower
                                    (candidateTerm |> DisambiguatedTerm.toTerm |> Term.raw |> RawTerm.toString)
                                )
                            ++ "(\\b| )"
                        )
                            |> Regex.fromString
                            |> Maybe.withDefault Regex.never
                in
                Set.member (candidateTerm |> DisambiguatedTerm.toTerm |> Term.raw |> RawTerm.toString) rawDisambiguatedPreferredTermsOfItemsListingThisItemAsRelated
                    || Regex.contains candidateTermAsWord definitionFieldBodyLower
            )
