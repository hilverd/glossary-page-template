module RelatedTermSuggestions exposing (suggest)

import Data.GlossaryItem.DisambiguatedTerm as DisambiguatedTerm exposing (DisambiguatedTerm)
import Data.GlossaryItem.RawTerm as RawTerm
import Data.GlossaryItem.Term as Term
import Extras.Regex
import Regex
import Set exposing (Set)


suggest :
    Set String
    -> Set String
    -> List DisambiguatedTerm
    -> String
    -> List DisambiguatedTerm
suggest relatedRawTermsAlreadyInForm rawDisambiguatedPreferredTermsOfItemsListingThisItemAsRelated disambiguatedPreferredTermsOutside_ definitionFieldBody =
    let
        candidateTerms : List DisambiguatedTerm
        candidateTerms =
            disambiguatedPreferredTermsOutside_
                |> List.filter
                    (\term -> not <| Set.member (term |> DisambiguatedTerm.toTerm |> Term.raw |> RawTerm.toString) relatedRawTermsAlreadyInForm)

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
