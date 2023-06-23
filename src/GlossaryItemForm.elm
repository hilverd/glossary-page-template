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
    , needsUpdating
    , relatedTermFields
    , selectRelatedTerm
    , suggestRelatedTerms
    , termBodyToId
    , termFields
    , toGlossaryItem
    , toggleAbbreviation
    , toggleNeedsUpdating
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
import Html exposing (details)
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
        , needsUpdating : Bool
        , lastUpdatedDate : String
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


needsUpdating : GlossaryItemForm -> Bool
needsUpdating glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            form.needsUpdating


lastUpdatedDate : GlossaryItemForm -> String
lastUpdatedDate glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            "2011-11-18T14:54:39.929Z"


validate : GlossaryItemForm -> GlossaryItemForm
validate form =
    let
        cannotBeEmptyMessage : String
        cannotBeEmptyMessage =
            "This field can't be empty"

        termIdsOutsideSet : Set String
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

        validatedTermFields : Array TermField
        validatedTermFields =
            form
                |> termFields
                |> Array.map
                    (\termField ->
                        let
                            body : String
                            body =
                                TermField.raw termField
                        in
                        termField
                            |> TermField.setValidationError
                                (if String.isEmpty body then
                                    Just cannotBeEmptyMessage

                                 else
                                    let
                                        termId : String
                                        termId =
                                            termBodyToId body
                                    in
                                    if Set.member termId termIdsOutsideSet then
                                        Just "This term already exists elsewhere"

                                    else if (Dict.get termId termIdsInsideForm |> Maybe.withDefault 0) > 1 then
                                        Just "This term occurs multiple times"

                                    else if ElementIds.reserved termId then
                                        Just "This term is reserved"

                                    else
                                        Nothing
                                )
                    )

        validatedDetailsFields : Array DetailsField
        validatedDetailsFields =
            form
                |> detailsFields
                |> Array.map
                    (\detailsField ->
                        let
                            raw : String
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

        validatedRelatedTermFields : Array RelatedTermField
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
        , needsUpdating = needsUpdating form
        , lastUpdatedDate = lastUpdatedDate form
        }


hasValidationErrors : GlossaryItemForm -> Bool
hasValidationErrors form =
    let
        hasErrors : (a -> Maybe b) -> Array a -> Bool
        hasErrors f =
            Array.toList >> List.any (f >> (/=) Nothing)
    in
    (form |> termFields |> hasErrors TermField.validationError)
        || (form |> detailsFields |> hasErrors DetailsField.validationError)
        || (form |> relatedTermFields |> hasErrors .validationError)


empty : List Term -> List Term -> List GlossaryItem -> GlossaryItemForm
empty withTermsOutside withPrimaryTermsOutside withItemsListingThisTermAsRelated =
    GlossaryItemForm
        { termFields = Array.fromList [ TermField.empty ]
        , detailsFields = Array.empty
        , relatedTermFields = Array.empty
        , termsOutside = withTermsOutside
        , primaryTermsOutside = withPrimaryTermsOutside
        , itemsListingThisItemAsRelated = withItemsListingThisTermAsRelated
        , needsUpdating = True
        , lastUpdatedDate = ""
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
        termFieldsForItem : List TermField
        termFieldsForItem =
            List.map
                (\term ->
                    TermField.fromString (Term.raw term) (Term.isAbbreviation term)
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

        detailsFieldsList : List DetailsField
        detailsFieldsList =
            List.map
                (\detailsElem ->
                    detailsElem
                        |> Details.raw
                        |> DetailsField.fromString
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
        , needsUpdating = item.needsUpdating
        , lastUpdatedDate = item.lastUpdatedDate |> Maybe.withDefault ""
        }
        |> validate


termBodyToId : String -> String
termBodyToId =
    String.replace " " "_"


toGlossaryItem : Bool -> GlossaryItems -> GlossaryItemForm -> Maybe String -> GlossaryItem
toGlossaryItem enableMarkdownBasedSyntax glossaryItems form dateTime =
    let
        bodyByIdReference : Dict String String
        bodyByIdReference =
            glossaryItems
                |> GlossaryItems.orderedAlphabetically
                |> Array.toList
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
                        raw : String
                        raw =
                            termField |> TermField.raw |> String.trim
                    in
                    (if enableMarkdownBasedSyntax then
                        Term.fromMarkdown

                     else
                        Term.fromPlaintext
                    )
                        raw
                    <|
                        TermField.isAbbreviation termField
                )
    , details =
        form
            |> detailsFields
            |> Array.toList
            |> List.map
                (DetailsField.raw
                    >> String.trim
                    >> (if enableMarkdownBasedSyntax then
                            Details.fromMarkdown

                        else
                            Details.fromPlaintext
                       )
                )
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
                                    |> Maybe.map
                                        ((if enableMarkdownBasedSyntax then
                                            RelatedTerm.fromMarkdown

                                          else
                                            RelatedTerm.fromPlaintext
                                         )
                                            ref
                                        )
                            )
                )
    , needsUpdating = needsUpdating form
    , lastUpdatedDate = dateTime
    }


addTerm : GlossaryItemForm -> GlossaryItemForm
addTerm glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            GlossaryItemForm
                { form | termFields = Array.push TermField.empty form.termFields }
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
            let
                needsUpdating1 : Bool
                needsUpdating1 =
                    if not (formHasDetailsOrRelatedTerms glossaryItemForm) && form.needsUpdating then
                        False

                    else
                        form.needsUpdating
            in
            GlossaryItemForm
                { form
                    | detailsFields = form.detailsFields |> Array.push DetailsField.empty
                    , needsUpdating = needsUpdating1
                }
                |> validate


updateDetails : DetailsIndex -> GlossaryItemForm -> String -> GlossaryItemForm
updateDetails detailsIndex glossaryItemForm body =
    case glossaryItemForm of
        GlossaryItemForm form ->
            GlossaryItemForm
                { form
                    | detailsFields =
                        Extras.Array.update
                            (always <| DetailsField.fromString body)
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


formHasDetailsOrRelatedTerms : GlossaryItemForm -> Bool
formHasDetailsOrRelatedTerms glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            form.detailsFields /= Array.empty || form.relatedTermFields /= Array.empty


addRelatedTerm : Maybe String -> GlossaryItemForm -> GlossaryItemForm
addRelatedTerm maybeTermId glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            let
                relatedTermField : RelatedTermField
                relatedTermField =
                    maybeTermId
                        |> Maybe.map (\termId -> { idReference = Just termId, validationError = Nothing })
                        |> Maybe.withDefault emptyRelatedTermField

                needsUpdating1 : Bool
                needsUpdating1 =
                    if not (formHasDetailsOrRelatedTerms glossaryItemForm) && form.needsUpdating then
                        False

                    else
                        form.needsUpdating
            in
            GlossaryItemForm
                { form
                    | relatedTermFields = Array.push relatedTermField form.relatedTermFields
                    , needsUpdating = needsUpdating1
                }
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

        detailsFieldBodies : List String
        detailsFieldBodies =
            glossaryItemForm
                |> detailsFields
                |> Array.toList
                |> List.map (DetailsField.raw >> String.toLower)

        primaryTermIdsOfItemsListingThisItemAsRelated : Set String
        primaryTermIdsOfItemsListingThisItemAsRelated =
            glossaryItemForm
                |> itemsListingThisTermAsRelated
                |> List.filterMap (.terms >> List.head)
                |> List.map Term.id
                |> Set.fromList
    in
    candidateTerms
        |> List.filter
            (\candidateTerm ->
                let
                    candidateTermAsWord : Regex.Regex
                    candidateTermAsWord =
                        ("(\\b| )" ++ Extras.Regex.escapeStringForUseInRegex (String.toLower (Term.raw candidateTerm)) ++ "(\\b| )")
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


toggleNeedsUpdating : GlossaryItemForm -> GlossaryItemForm
toggleNeedsUpdating glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            GlossaryItemForm
                { form
                    | needsUpdating = not form.needsUpdating
                }
                |> validate
