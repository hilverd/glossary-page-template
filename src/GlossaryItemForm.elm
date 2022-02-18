module GlossaryItemForm exposing
    ( Details
    , GlossaryItemForm
    , RelatedTerm
    , Term
    , addDetails
    , addRelatedTerm
    , addTerm
    , deleteDetails
    , deleteRelatedTerm
    , deleteTerm
    , detailsArray
    , empty
    , fromGlossaryItem
    , hasValidationErrors
    , relatedTerms
    , selectRelatedTerm
    , termBodyToId
    , terms
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
import Extras.Array
import Html.Attributes exposing (id)
import Set exposing (Set)


type alias Term =
    { body : String
    , isAbbreviation : Bool
    , isAbbreviationManuallyOverridden : Bool
    , validationError : Maybe String
    }


type alias Details =
    { body : String
    , validationError : Maybe String
    }


type alias RelatedTerm =
    { idReference : Maybe String
    , validationError : Maybe String
    }


type GlossaryItemForm
    = GlossaryItemForm
        { terms : Array Term
        , detailsArray : Array Details
        , relatedTerms : Array RelatedTerm
        , existingTermIds : Set String
        }


terms : GlossaryItemForm -> Array Term
terms glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            form.terms


detailsArray : GlossaryItemForm -> Array Details
detailsArray glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            form.detailsArray


relatedTerms : GlossaryItemForm -> Array RelatedTerm
relatedTerms glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            form.relatedTerms


existingTermIds : GlossaryItemForm -> Set String
existingTermIds glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            form.existingTermIds


validate : GlossaryItemForm -> GlossaryItemForm
validate form =
    let
        existingTermIdsSet =
            existingTermIds form

        _ =
            Debug.log "existingTermIdsSet" existingTermIdsSet

        validatedTerms =
            form
                |> terms
                |> Array.map
                    (\term ->
                        { term
                            | validationError =
                                if String.isEmpty term.body then
                                    Just "This field can't be empty"

                                else if Set.member (termBodyToId term.body) existingTermIdsSet then
                                    Just "This term already exists"

                                else
                                    Nothing
                        }
                    )

        validatedDetails =
            form
                |> detailsArray
                |> Array.map
                    (\details ->
                        { details
                            | validationError =
                                if String.isEmpty details.body then
                                    Just "This field can't be empty"

                                else
                                    Nothing
                        }
                    )

        validatedRelatedTerms =
            form
                |> relatedTerms
                |> Array.map
                    (\relatedTerm ->
                        { relatedTerm
                            | validationError =
                                if relatedTerm.idReference == Nothing then
                                    Just "Please select an item"

                                else
                                    Nothing
                        }
                    )
    in
    GlossaryItemForm
        { terms = validatedTerms
        , detailsArray = validatedDetails
        , relatedTerms = validatedRelatedTerms
        , existingTermIds = existingTermIds form
        }


hasValidationErrors : GlossaryItemForm -> Bool
hasValidationErrors form =
    (form |> terms |> Array.toList |> List.any (.validationError >> (/=) Nothing))
        || (form |> detailsArray |> Array.toList |> List.any (.validationError >> (/=) Nothing))
        || (form |> relatedTerms |> Array.toList |> List.any (.validationError >> (/=) Nothing))


empty : Set String -> GlossaryItemForm
empty withExistingTermIds =
    GlossaryItemForm
        { terms = Array.fromList [ emptyTerm ]
        , detailsArray = Array.empty
        , relatedTerms = Array.empty
        , existingTermIds = withExistingTermIds
        }
        |> validate


emptyTerm : Term
emptyTerm =
    { body = ""
    , isAbbreviation = False
    , isAbbreviationManuallyOverridden = False
    , validationError = Nothing
    }


emptyDetails : Details
emptyDetails =
    { body = ""
    , validationError = Nothing
    }


emptyRelatedTerm : RelatedTerm
emptyRelatedTerm =
    { idReference = Nothing
    , validationError = Nothing
    }


fromGlossaryItem : Set String -> GlossaryItem -> GlossaryItemForm
fromGlossaryItem withExistingTermIds item =
    let
        termsForItem =
            List.map (\term -> Term term.body term.isAbbreviation False Nothing) item.terms

        termIdsForItem =
            termsForItem
                |> List.map (.body >> termBodyToId)
                |> Set.fromList

        detailsList =
            List.map (\detailsElem -> Details detailsElem Nothing) item.details
    in
    GlossaryItemForm
        { terms = Array.fromList termsForItem
        , detailsArray = Array.fromList detailsList
        , relatedTerms =
            item.relatedTerms
                |> List.map (\term -> RelatedTerm (Just term.idReference) Nothing)
                |> Array.fromList
        , existingTermIds =
            Set.diff withExistingTermIds termIdsForItem
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
            |> terms
            |> Array.toList
            |> List.map
                (\formTerm ->
                    { id = termBodyToId formTerm.body
                    , isAbbreviation = formTerm.isAbbreviation
                    , body = formTerm.body
                    }
                )
    , details =
        form
            |> detailsArray
            |> Array.toList
            |> List.map .body
    , relatedTerms =
        form
            |> relatedTerms
            |> Array.toList
            |> List.filterMap
                (\formRelatedTerm ->
                    formRelatedTerm.idReference
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
                { form | terms = Array.push emptyTerm form.terms }
                |> validate


updateTerm : TermIndex -> GlossaryItemForm -> String -> GlossaryItemForm
updateTerm termIndex glossaryItemForm body =
    case glossaryItemForm of
        GlossaryItemForm form ->
            GlossaryItemForm
                { form
                    | terms =
                        Extras.Array.update
                            (\term -> { term | body = body })
                            (TermIndex.toInt termIndex)
                            form.terms
                }
                |> validate


deleteTerm : TermIndex -> GlossaryItemForm -> GlossaryItemForm
deleteTerm termIndex glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            GlossaryItemForm
                { form | terms = Extras.Array.delete (TermIndex.toInt termIndex) form.terms }
                |> validate


toggleAbbreviation : TermIndex -> GlossaryItemForm -> GlossaryItemForm
toggleAbbreviation termIndex glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            GlossaryItemForm
                { form
                    | terms =
                        Extras.Array.update
                            (\term ->
                                { term
                                    | isAbbreviation = not term.isAbbreviation
                                    , isAbbreviationManuallyOverridden = True
                                }
                            )
                            (TermIndex.toInt termIndex)
                            form.terms
                }
                |> validate


addDetails : GlossaryItemForm -> GlossaryItemForm
addDetails glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            GlossaryItemForm
                { form | detailsArray = form.detailsArray |> Array.push emptyDetails }
                |> validate


updateDetails : DetailsIndex -> GlossaryItemForm -> String -> GlossaryItemForm
updateDetails detailsIndex glossaryItemForm body =
    case glossaryItemForm of
        GlossaryItemForm form ->
            GlossaryItemForm
                { form
                    | detailsArray =
                        Extras.Array.update
                            (\detailsSingle -> { detailsSingle | body = body })
                            (DetailsIndex.toInt detailsIndex)
                            form.detailsArray
                }
                |> validate


deleteDetails : DetailsIndex -> GlossaryItemForm -> GlossaryItemForm
deleteDetails index glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            GlossaryItemForm
                { form | detailsArray = Extras.Array.delete (DetailsIndex.toInt index) form.detailsArray }
                |> validate


addRelatedTerm : GlossaryItemForm -> GlossaryItemForm
addRelatedTerm glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            GlossaryItemForm
                { form | relatedTerms = Array.push emptyRelatedTerm form.relatedTerms }
                |> validate


selectRelatedTerm : RelatedTermIndex -> GlossaryItemForm -> Maybe String -> GlossaryItemForm
selectRelatedTerm index glossaryItemForm relatedTermIdReference =
    case glossaryItemForm of
        GlossaryItemForm form ->
            GlossaryItemForm
                { form
                    | relatedTerms =
                        Extras.Array.update
                            (always <| RelatedTerm relatedTermIdReference Nothing)
                            (RelatedTermIndex.toInt index)
                            form.relatedTerms
                }
                |> validate


deleteRelatedTerm : RelatedTermIndex -> GlossaryItemForm -> GlossaryItemForm
deleteRelatedTerm index glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            GlossaryItemForm
                { form | relatedTerms = Extras.Array.delete (RelatedTermIndex.toInt index) form.relatedTerms }
                |> validate
