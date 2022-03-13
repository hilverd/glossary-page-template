module GlossaryItemForm exposing
    ( DetailsField
    , GlossaryItemForm
    , RelatedTermField
    , TermField
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
import Data.GlossaryItem as GlossaryItem exposing (GlossaryItem)
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Data.RelatedTermIndex as RelatedTermIndex exposing (RelatedTermIndex)
import Data.TermIndex as TermIndex exposing (TermIndex)
import Dict exposing (Dict)
import ElementIds
import Extras.Array
import Extras.Regex
import Html exposing (form)
import Regex
import Set exposing (Set)


type alias TermField =
    { body : String
    , isAbbreviation : Bool
    , isAbbreviationManuallyOverridden : Bool
    , validationError : Maybe String
    }


type alias DetailsField =
    { body : String
    , validationError : Maybe String
    }


type alias RelatedTermField =
    { idReference : Maybe String
    , validationError : Maybe String
    }


type GlossaryItemForm
    = GlossaryItemForm
        { termFields : Array TermField
        , detailsFields : Array DetailsField
        , relatedTermFields : Array RelatedTermField
        , termsOutside : List GlossaryItem.Term
        , primaryTermsOutside : List GlossaryItem.Term
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


termsOutside : GlossaryItemForm -> List GlossaryItem.Term
termsOutside glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            form.termsOutside


primaryTermsOutside : GlossaryItemForm -> List GlossaryItem.Term
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
        termIdsOutsideSet =
            form |> termsOutside |> List.map .id |> Set.fromList

        termIdsInsideForm : Dict String Int
        termIdsInsideForm =
            form
                |> termFields
                |> Array.map (.body >> termBodyToId)
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
                                String.trim termField.body

                            termId =
                                termBodyToId body
                        in
                        { termField
                            | validationError =
                                if String.isEmpty body then
                                    Just "This field can't be empty"

                                else if Set.member termId termIdsOutsideSet then
                                    Just "This term already exists elsewhere"

                                else if (Dict.get termId termIdsInsideForm |> Maybe.withDefault 0) > 1 then
                                    Just "This term occurs multiple times"

                                else if ElementIds.reserved termId then
                                    Just "This term is reserved"

                                else
                                    Nothing
                        }
                    )

        validatedDetailsFields =
            form
                |> detailsFields
                |> Array.map
                    (\detailsField ->
                        let
                            body =
                                String.trim detailsField.body
                        in
                        { detailsField
                            | validationError =
                                if String.isEmpty body then
                                    Just "This field can't be empty"

                                else
                                    Nothing
                        }
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
        hasErrors =
            Array.toList >> List.any (.validationError >> (/=) Nothing)
    in
    (form |> termFields |> hasErrors)
        || (form |> detailsFields |> hasErrors)
        || (form |> relatedTermFields |> hasErrors)


empty : List GlossaryItem.Term -> List GlossaryItem.Term -> List GlossaryItem -> GlossaryItemForm
empty withTermsOutside withPrimaryTermsOutside withItemsListingThisTermAsRelated =
    GlossaryItemForm
        { termFields = Array.fromList [ emptyTermField ]
        , detailsFields = Array.empty
        , relatedTermFields = Array.empty
        , termsOutside = withTermsOutside
        , primaryTermsOutside = withPrimaryTermsOutside
        , itemsListingThisItemAsRelated = withItemsListingThisTermAsRelated
        }
        |> validate


emptyTermField : TermField
emptyTermField =
    { body = ""
    , isAbbreviation = False
    , isAbbreviationManuallyOverridden = False
    , validationError = Nothing
    }


emptyDetailsField : DetailsField
emptyDetailsField =
    { body = ""
    , validationError = Nothing
    }


emptyRelatedTermField : RelatedTermField
emptyRelatedTermField =
    { idReference = Nothing
    , validationError = Nothing
    }


fromGlossaryItem : List GlossaryItem.Term -> List GlossaryItem.Term -> List GlossaryItem -> GlossaryItem -> GlossaryItemForm
fromGlossaryItem existingTerms existingPrimaryTerms withItemsListingThisTermAsRelated item =
    let
        termFieldsForItem =
            List.map (\term -> TermField term.body term.isAbbreviation True Nothing) item.terms

        termIdsForItem : Set String
        termIdsForItem =
            termFieldsForItem
                |> List.map (.body >> termBodyToId)
                |> Set.fromList

        termsOutside1 : List GlossaryItem.Term
        termsOutside1 =
            List.filter
                (\existingTerm ->
                    not <| Set.member existingTerm.id termIdsForItem
                )
                existingTerms

        primaryTermsOutside1 : List GlossaryItem.Term
        primaryTermsOutside1 =
            List.filter
                (\existingTerm ->
                    not <| Set.member existingTerm.id termIdsForItem
                )
                existingPrimaryTerms

        detailsFieldsList =
            List.map (\detailsElem -> DetailsField detailsElem Nothing) item.details
    in
    GlossaryItemForm
        { termFields = Array.fromList termFieldsForItem
        , detailsFields = Array.fromList detailsFieldsList
        , relatedTermFields =
            item.relatedTerms
                |> List.map (\term -> RelatedTermField (Just term.idReference) Nothing)
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
                            (\term -> Dict.update term.id (always <| Just term.body))
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
                    { id = termBodyToId termField.body
                    , isAbbreviation = termField.isAbbreviation
                    , body = termField.body |> String.trim
                    }
                )
    , details =
        form
            |> detailsFields
            |> Array.toList
            |> List.map (.body >> String.trim)
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
                                    |> Maybe.map (GlossaryItem.RelatedTerm ref)
                            )
                )
    }


addTerm : GlossaryItemForm -> GlossaryItemForm
addTerm glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            GlossaryItemForm
                { form | termFields = Array.push emptyTermField form.termFields }
                |> validate


updateTerm : TermIndex -> GlossaryItemForm -> String -> GlossaryItemForm
updateTerm termIndex glossaryItemForm body =
    case glossaryItemForm of
        GlossaryItemForm form ->
            GlossaryItemForm
                { form
                    | termFields =
                        Extras.Array.update
                            (\termField ->
                                if not termField.isAbbreviationManuallyOverridden then
                                    { termField
                                        | body = body
                                        , isAbbreviation = termBodyLooksLikeAnAbbreviation body
                                    }

                                else
                                    { termField
                                        | body = body
                                    }
                            )
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
                                { termField
                                    | isAbbreviation = not termField.isAbbreviation
                                    , isAbbreviationManuallyOverridden = True
                                }
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
                { form | detailsFields = form.detailsFields |> Array.push emptyDetailsField }
                |> validate


updateDetails : DetailsIndex -> GlossaryItemForm -> String -> GlossaryItemForm
updateDetails detailsIndex glossaryItemForm body =
    case glossaryItemForm of
        GlossaryItemForm form ->
            GlossaryItemForm
                { form
                    | detailsFields =
                        Extras.Array.update
                            (\detailsField -> { detailsField | body = body })
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


termBodyLooksLikeAnAbbreviation : String -> Bool
termBodyLooksLikeAnAbbreviation bodyRaw =
    let
        body =
            String.trim bodyRaw
    in
    (not <| String.isEmpty body) && body == String.toUpper body


suggestRelatedTerms : GlossaryItemForm -> List GlossaryItem.Term
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

        candidateTerms : List GlossaryItem.Term
        candidateTerms =
            glossaryItemForm
                |> primaryTermsOutside
                |> List.filter
                    (\term -> not <| Set.member term.id relatedTermIdsAlreadyInForm)

        detailsFieldBodies =
            glossaryItemForm
                |> detailsFields
                |> Array.toList
                |> List.map (.body >> String.toLower)

        primaryTermIdsOfItemsListingThisItemAsRelated =
            glossaryItemForm
                |> itemsListingThisTermAsRelated
                |> List.map (.terms >> List.head)
                |> List.filterMap identity
                |> List.map .id
                |> Set.fromList

        relevantCandidateTerms : List GlossaryItem.Term
        relevantCandidateTerms =
            candidateTerms
                |> List.filter
                    (\candidateTerm ->
                        let
                            candidateTermAsWord =
                                ("\\b" ++ Extras.Regex.escapeStringForUseInRegex (String.toLower candidateTerm.body) ++ "\\b")
                                    |> Regex.fromString
                                    |> Maybe.withDefault Regex.never
                        in
                        Set.member candidateTerm.id primaryTermIdsOfItemsListingThisItemAsRelated
                            || List.any
                                (\detailsFieldBody ->
                                    Regex.contains candidateTermAsWord detailsFieldBody
                                )
                                detailsFieldBodies
                    )
    in
    relevantCandidateTerms
