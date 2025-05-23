module RelatedTermSuggestions exposing (suggest)

import Data.GlossaryItem.DisambiguatedTerm as DisambiguatedTerm exposing (DisambiguatedTerm)
import Data.GlossaryItem.Tag as Tag exposing (Tag)
import Data.GlossaryItem.Term as Term
import Data.GlossaryItemOutline exposing (GlossaryItemOutline)
import Extras.Regex
import Regex
import Set exposing (Set)


suggest :
    List Tag
    -> Set String
    -> Set String
    -> List GlossaryItemOutline
    -> String
    -> List DisambiguatedTerm
suggest tags relatedRawTermsAlreadyInForm rawDisambiguatedPreferredTermsOfItemsListingThisItemAsRelated outlinesOfItemsOutside definitionFieldBody =
    let
        rawTagsSet : Set String
        rawTagsSet =
            tags
                |> List.map Tag.raw
                |> Set.fromList

        hasATagInCommonWithItemBeingDefined : GlossaryItemOutline -> Bool
        hasATagInCommonWithItemBeingDefined outlineOfItemOutside =
            List.any
                (\tag -> Set.member tag rawTagsSet)
                outlineOfItemOutside.allTags

        isNotAlreadyARelatedTerm : GlossaryItemOutline -> Bool
        isNotAlreadyARelatedTerm outlineOfItemOutside =
            not <| Set.member outlineOfItemOutside.disambiguatedPreferredTerm relatedRawTermsAlreadyInForm

        outlinesOfCandidateItems : List GlossaryItemOutline
        outlinesOfCandidateItems =
            outlinesOfItemsOutside
                |> List.filter
                    (\outlineOfItemOutside ->
                        isNotAlreadyARelatedTerm outlineOfItemOutside
                            && (Set.isEmpty rawTagsSet || hasATagInCommonWithItemBeingDefined outlineOfItemOutside)
                    )

        definitionFieldBodyLower : String
        definitionFieldBodyLower =
            String.toLower definitionFieldBody
    in
    outlinesOfCandidateItems
        |> List.filter
            (\outlineOfCandidateItem ->
                let
                    rawDisambiguatedPreferredTermOfCandidateItem : String
                    rawDisambiguatedPreferredTermOfCandidateItem =
                        outlineOfCandidateItem.disambiguatedPreferredTerm

                    candidateAsWord : Regex.Regex
                    candidateAsWord =
                        ("(\\b| )"
                            ++ Extras.Regex.escapeStringForUseInRegex
                                (String.toLower
                                    outlineOfCandidateItem.preferredTerm
                                )
                            ++ "(\\b| )"
                        )
                            |> Regex.fromString
                            |> Maybe.withDefault Regex.never
                in
                Set.member rawDisambiguatedPreferredTermOfCandidateItem rawDisambiguatedPreferredTermsOfItemsListingThisItemAsRelated
                    || Regex.contains candidateAsWord definitionFieldBodyLower
            )
        |> List.map
            (\outlineOfCandidateItem ->
                Term.fromMarkdown outlineOfCandidateItem.disambiguatedPreferredTerm False
                    |> DisambiguatedTerm.fromTerm
            )
