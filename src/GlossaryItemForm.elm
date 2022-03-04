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
        , termIdsOutside : Set String
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


termIdsOutside : GlossaryItemForm -> Set String
termIdsOutside glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            form.termIdsOutside


validate : GlossaryItemForm -> GlossaryItemForm
validate form =
    let
        termIdsOutsideSet =
            termIdsOutside form

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
        , termIdsOutside = termIdsOutside form
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


empty : Set String -> GlossaryItemForm
empty withTermIdsOutside =
    GlossaryItemForm
        { termFields = Array.fromList [ emptyTermField ]
        , detailsFields = Array.empty
        , relatedTermFields = Array.empty
        , termIdsOutside = withTermIdsOutside
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


fromGlossaryItem : Set String -> GlossaryItem -> GlossaryItemForm
fromGlossaryItem existingTermIds item =
    let
        termsForItem =
            List.map (\term -> TermField term.body term.isAbbreviation True Nothing) item.terms

        termIdsForItem =
            termsForItem
                |> List.map (.body >> termBodyToId)
                |> Set.fromList

        detailsFieldsList =
            List.map (\detailsElem -> DetailsField detailsElem Nothing) item.details
    in
    GlossaryItemForm
        { termFields = Array.fromList termsForItem
        , detailsFields = Array.fromList detailsFieldsList
        , relatedTermFields =
            item.relatedTerms
                |> List.map (\term -> RelatedTermField (Just term.idReference) Nothing)
                |> Array.fromList
        , termIdsOutside = Set.diff existingTermIds termIdsForItem
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


addRelatedTerm : GlossaryItemForm -> GlossaryItemForm
addRelatedTerm glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            GlossaryItemForm
                { form | relatedTermFields = Array.push emptyRelatedTermField form.relatedTermFields }
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
