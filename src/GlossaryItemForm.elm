module GlossaryItemForm exposing
    ( GlossaryItemForm
    , RelatedTermField
    , addDetails
    , addRelatedTerm
    , addTerm
    , deleteDetails
    , deleteRelatedTerm
    , deleteTerm
    , detailsFields
    , empty
    , fromGlossaryItem
    , hasValidationErrors
    , relatedTermFields
    , selectRelatedTerm
    , suggestRelatedTerms
    , termBodyToId
    , termFields
    , toGlossaryItem
    , toggleAbbreviation
    , updateDetails
    , updateTerm
    )

import Array exposing (Array)
import Data.DetailsIndex as DetailsIndex exposing (DetailsIndex)
import Data.GlossaryItem exposing (GlossaryItem)
import Data.GlossaryItem.Details as Details
import Data.GlossaryItem.RelatedTerm as RelatedTerm
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Data.RelatedTermIndex as RelatedTermIndex exposing (RelatedTermIndex)
import Data.TermIndex as TermIndex exposing (TermIndex)
import Dict exposing (Dict)
import ElementIds
import Extras.Array
import Extras.Regex
import GlossaryItemForm.DetailsField as DetailsField exposing (DetailsField)
import GlossaryItemForm.TermField as TermField exposing (TermField)
import Regex
import Set exposing (Set)


type alias RelatedTermField =
    { idReference : Maybe String
    , validationError : Maybe String
    }


type GlossaryItemForm
    = GlossaryItemForm
        { termFields : Array TermField
        , detailsFields : Array DetailsField
        , relatedTermFields : Array RelatedTermField
        , termsOutside : List Term
        , primaryTermsOutside : List Term
        , itemsListingThisItemAsRelated : List GlossaryItem
        }


termFields : GlossaryItemForm -> Array TermField
termFields glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            form.termFields


detailsFields : GlossaryItemForm -> Array DetailsField
detailsFields glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            form.detailsFields


relatedTermFields : GlossaryItemForm -> Array RelatedTermField
relatedTermFields glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            form.relatedTermFields


termsOutside : GlossaryItemForm -> List Term
termsOutside glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            form.termsOutside


primaryTermsOutside : GlossaryItemForm -> List Term
primaryTermsOutside glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            form.primaryTermsOutside


itemsListingThisTermAsRelated : GlossaryItemForm -> List GlossaryItem
itemsListingThisTermAsRelated glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            form.itemsListingThisItemAsRelated


validate : GlossaryItemForm -> GlossaryItemForm
validate form =
    let
        cannotBeEmptyMessage =
            "This field can't be empty"

        termIdsOutsideSet =
            form |> termsOutside |> List.map Term.id |> Set.fromList

        termIdsInsideForm : Dict String Int
        termIdsInsideForm =
            form
                |> termFields
                |> Array.map (TermField.raw >> termBodyToId)
                |> Array.foldl
                    (\termId ->
                        Dict.update
                            termId
                            (Maybe.map ((+) 1) >> Maybe.withDefault 1 >> Just)
                    )
                    Dict.empty

        validatedTermFields =
            form
                |> termFields
                |> Array.map
                    (\termField ->
                        let
                            body =
                                TermField.raw termField

                            termId =
                                termBodyToId body
                        in
                        termField
                            |> TermField.setValidationError
                                (if String.isEmpty body then
                                    Just cannotBeEmptyMessage

                                 else if Set.member termId termIdsOutsideSet then
                                    Just "This term already exists elsewhere"

                                 else if (Dict.get termId termIdsInsideForm |> Maybe.withDefault 0) > 1 then
                                    Just "This term occurs multiple times"

                                 else if ElementIds.reserved termId then
                                    Just "This term is reserved"

                                 else
                                    Nothing
                                )
                    )

        validatedDetailsFields =
            form
                |> detailsFields
                |> Array.map
                    (\detailsField ->
                        let
                            raw =
                                DetailsField.raw detailsField
                        in
                        detailsField
                            |> DetailsField.setValidationError
                                (if String.isEmpty raw then
                                    Just cannotBeEmptyMessage

                                 else
                                    Nothing
                                )
                    )

        validatedRelatedTermFields =
            form
                |> relatedTermFields
                |> Array.map
                    (\relatedTermField ->
                        { relatedTermField
                            | validationError =
                                if relatedTermField.idReference == Nothing then
                                    Just "Please select an item"

                                else
                                    Nothing
                        }
                    )
    in
    GlossaryItemForm
        { termFields = validatedTermFields
        , detailsFields = validatedDetailsFields
        , relatedTermFields = validatedRelatedTermFields
        , termsOutside = termsOutside form
        , primaryTermsOutside = primaryTermsOutside form
        , itemsListingThisItemAsRelated = itemsListingThisTermAsRelated form
        }


hasValidationErrors : GlossaryItemForm -> Bool
hasValidationErrors form =
    let
        hasErrors f =
            Array.toList >> List.any (f >> (/=) Nothing)
    in
    (form |> termFields |> hasErrors TermField.validationError)
        || (form |> detailsFields |> hasErrors DetailsField.validationError)
        || (form |> relatedTermFields |> hasErrors .validationError)


empty : List Term -> List Term -> List GlossaryItem -> GlossaryItemForm
empty withTermsOutside withPrimaryTermsOutside withItemsListingThisTermAsRelated =
    GlossaryItemForm
        { termFields = Array.fromList [ TermField.emptyPlaintext ]
        , detailsFields = Array.empty
        , relatedTermFields = Array.empty
        , termsOutside = withTermsOutside
        , primaryTermsOutside = withPrimaryTermsOutside
        , itemsListingThisItemAsRelated = withItemsListingThisTermAsRelated
        }
        |> validate


emptyRelatedTermField : RelatedTermField
emptyRelatedTermField =
    { idReference = Nothing
    , validationError = Nothing
    }


fromGlossaryItem : List Term -> List Term -> List GlossaryItem -> GlossaryItem -> GlossaryItemForm
fromGlossaryItem existingTerms existingPrimaryTerms withItemsListingThisTermAsRelated item =
    let
        termFieldsForItem =
            List.map
                (\term ->
                    TermField.fromPlaintext (Term.raw term) (Term.isAbbreviation term)
                )
                item.terms

        termIdsForItem : Set String
        termIdsForItem =
            termFieldsForItem
                |> List.map (TermField.raw >> termBodyToId)
                |> Set.fromList

        termsOutside1 : List Term
        termsOutside1 =
            List.filter
                (\existingTerm ->
                    not <| Set.member (Term.id existingTerm) termIdsForItem
                )
                existingTerms

        primaryTermsOutside1 : List Term
        primaryTermsOutside1 =
            List.filter
                (\existingTerm ->
                    not <| Set.member (Term.id existingTerm) termIdsForItem
                )
                existingPrimaryTerms

        detailsFieldsList =
            List.map
                (\detailsElem ->
                    detailsElem
                        |> Details.raw
                        |> DetailsField.fromPlaintext
                )
                item.details
    in
    GlossaryItemForm
        { termFields = Array.fromList termFieldsForItem
        , detailsFields = Array.fromList detailsFieldsList
        , relatedTermFields =
            item.relatedTerms
                |> List.map (\term -> RelatedTermField (Just <| RelatedTerm.idReference term) Nothing)
                |> Array.fromList
        , termsOutside = termsOutside1
        , primaryTermsOutside = primaryTermsOutside1
        , itemsListingThisItemAsRelated = withItemsListingThisTermAsRelated
        }
        |> validate


termBodyToId : String -> String
termBodyToId body =
    String.replace " " "_" body


toGlossaryItem : GlossaryItems -> GlossaryItemForm -> GlossaryItem
toGlossaryItem glossaryItems form =
    let
        bodyByIdReference : Dict String String
        bodyByIdReference =
            glossaryItems
                |> GlossaryItems.orderedAlphabetically
                |> List.foldl
                    (\( _, glossaryItem ) result ->
                        List.foldl
                            (\term -> Dict.update (Term.id term) (always <| Just <| Term.raw term))
                            result
                            glossaryItem.terms
                    )
                    Dict.empty
    in
    { terms =
        form
            |> termFields
            |> Array.toList
            |> List.map
                (\termField ->
                    let
                        raw =
                            termField |> TermField.raw |> String.trim
                    in
                    Term.fromPlaintext raw <| TermField.isAbbreviation termField
                )
    , details =
        form
            |> detailsFields
            |> Array.toList
            |> List.map (DetailsField.raw >> String.trim >> Details.fromPlaintext)
    , relatedTerms =
        form
            |> relatedTermFields
            |> Array.toList
            |> List.filterMap
                (\relatedTermField ->
                    relatedTermField.idReference
                        |> Maybe.andThen
                            (\ref ->
                                Dict.get ref bodyByIdReference
                                    |> Maybe.map (RelatedTerm.fromPlaintext ref)
                            )
                )
    }


addTerm : GlossaryItemForm -> GlossaryItemForm
addTerm glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            GlossaryItemForm
                { form | termFields = Array.push TermField.emptyPlaintext form.termFields }
                |> validate


updateTerm : TermIndex -> GlossaryItemForm -> String -> GlossaryItemForm
updateTerm termIndex glossaryItemForm body =
    case glossaryItemForm of
        GlossaryItemForm form ->
            GlossaryItemForm
                { form
                    | termFields =
                        Extras.Array.update
                            (TermField.setBody body)
                            (TermIndex.toInt termIndex)
                            form.termFields
                }
                |> validate


deleteTerm : TermIndex -> GlossaryItemForm -> GlossaryItemForm
deleteTerm termIndex glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            GlossaryItemForm
                { form | termFields = Extras.Array.delete (TermIndex.toInt termIndex) form.termFields }
                |> validate


toggleAbbreviation : TermIndex -> GlossaryItemForm -> GlossaryItemForm
toggleAbbreviation termIndex glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            GlossaryItemForm
                { form
                    | termFields =
                        Extras.Array.update
                            (\termField ->
                                termField
                                    |> TermField.setIsAbbreviation (not <| TermField.isAbbreviation termField)
                                    |> TermField.setIsAbbreviationManuallyOverridden True
                            )
                            (TermIndex.toInt termIndex)
                            form.termFields
                }
                |> validate


addDetails : GlossaryItemForm -> GlossaryItemForm
addDetails glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            GlossaryItemForm
                { form | detailsFields = form.detailsFields |> Array.push DetailsField.emptyPlaintext }
                |> validate


updateDetails : DetailsIndex -> GlossaryItemForm -> String -> GlossaryItemForm
updateDetails detailsIndex glossaryItemForm body =
    case glossaryItemForm of
        GlossaryItemForm form ->
            GlossaryItemForm
                { form
                    | detailsFields =
                        Extras.Array.update
                            (always <| DetailsField.fromPlaintext body)
                            (DetailsIndex.toInt detailsIndex)
                            form.detailsFields
                }
                |> validate


deleteDetails : DetailsIndex -> GlossaryItemForm -> GlossaryItemForm
deleteDetails index glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            GlossaryItemForm
                { form | detailsFields = Extras.Array.delete (DetailsIndex.toInt index) form.detailsFields }
                |> validate


addRelatedTerm : Maybe String -> GlossaryItemForm -> GlossaryItemForm
addRelatedTerm maybeTermId glossaryItemForm =
    let
        relatedTermField =
            maybeTermId
                |> Maybe.map (\termId -> { idReference = Just termId, validationError = Nothing })
                |> Maybe.withDefault emptyRelatedTermField
    in
    case glossaryItemForm of
        GlossaryItemForm form ->
            GlossaryItemForm
                { form | relatedTermFields = Array.push relatedTermField form.relatedTermFields }
                |> validate


selectRelatedTerm : RelatedTermIndex -> GlossaryItemForm -> Maybe String -> GlossaryItemForm
selectRelatedTerm index glossaryItemForm relatedTermIdReference =
    case glossaryItemForm of
        GlossaryItemForm form ->
            GlossaryItemForm
                { form
                    | relatedTermFields =
                        Extras.Array.update
                            (always <| RelatedTermField relatedTermIdReference Nothing)
                            (RelatedTermIndex.toInt index)
                            form.relatedTermFields
                }
                |> validate


deleteRelatedTerm : RelatedTermIndex -> GlossaryItemForm -> GlossaryItemForm
deleteRelatedTerm index glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            GlossaryItemForm
                { form | relatedTermFields = Extras.Array.delete (RelatedTermIndex.toInt index) form.relatedTermFields }
                |> validate


suggestRelatedTerms : GlossaryItemForm -> List Term
suggestRelatedTerms glossaryItemForm =
    let
        relatedTermIdsAlreadyInForm : Set String
        relatedTermIdsAlreadyInForm =
            glossaryItemForm
                |> relatedTermFields
                |> Array.foldl
                    (\relatedTermField result ->
                        relatedTermField.idReference
                            |> Maybe.map (\idReference -> Set.insert idReference result)
                            |> Maybe.withDefault result
                    )
                    Set.empty

        candidateTerms : List Term
        candidateTerms =
            glossaryItemForm
                |> primaryTermsOutside
                |> List.filter
                    (\term -> not <| Set.member (Term.id term) relatedTermIdsAlreadyInForm)

        detailsFieldBodies =
            glossaryItemForm
                |> detailsFields
                |> Array.toList
                |> List.map (DetailsField.raw >> String.toLower)

        primaryTermIdsOfItemsListingThisItemAsRelated =
            glossaryItemForm
                |> itemsListingThisTermAsRelated
                |> List.map (.terms >> List.head)
                |> List.filterMap identity
                |> List.map Term.id
                |> Set.fromList

        relevantCandidateTerms : List Term
        relevantCandidateTerms =
            candidateTerms
                |> List.filter
                    (\candidateTerm ->
                        let
                            candidateTermAsWord =
                                ("\\b" ++ Extras.Regex.escapeStringForUseInRegex (String.toLower (Term.raw candidateTerm)) ++ "\\b")
                                    |> Regex.fromString
                                    |> Maybe.withDefault Regex.never
                        in
                        Set.member (Term.id candidateTerm) primaryTermIdsOfItemsListingThisItemAsRelated
                            || List.any
                                (\detailsFieldBody ->
                                    Regex.contains candidateTermAsWord detailsFieldBody
                                )
                                detailsFieldBodies
                    )
    in
    relevantCandidateTerms
