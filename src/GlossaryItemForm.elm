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
    , empty
    , fromGlossaryItem
    , hasValidationErrors
    , selectRelatedTerm
    , termBodyToId
    , toGlossaryItem
    , toggleAbbreviation
    , updateDetails
    , updateTerm
    )

import Array exposing (Array)
import Data.GlossaryItem as GlossaryItem exposing (GlossaryItem)
import Data.TermIndex as TermIndex exposing (TermIndex)
import Dict exposing (Dict)
import Extras.Array


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


type alias GlossaryItemForm =
    { terms : Array Term
    , details : Array Details
    , relatedTerms : Array RelatedTerm
    }


validate : GlossaryItemForm -> GlossaryItemForm
validate form =
    let
        validatedTerms =
            form.terms
                |> Array.map
                    (\term ->
                        { term
                            | validationError =
                                if String.isEmpty term.body then
                                    Just "This field can't be empty"

                                else
                                    Nothing
                        }
                    )

        validatedDetails =
            form.details
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
            form.relatedTerms
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
    GlossaryItemForm validatedTerms validatedDetails validatedRelatedTerms


hasValidationErrors : GlossaryItemForm -> Bool
hasValidationErrors form =
    (form.terms |> Array.toList |> List.any (.validationError >> (/=) Nothing))
        || (form.details |> Array.toList |> List.any (.validationError >> (/=) Nothing))
        || (form.relatedTerms |> Array.toList |> List.any (.validationError >> (/=) Nothing))


empty : GlossaryItemForm
empty =
    validate
        { terms = Array.fromList [ emptyTerm ]
        , details = Array.empty
        , relatedTerms = Array.empty
        }


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


fromGlossaryItem : GlossaryItem -> GlossaryItemForm
fromGlossaryItem item =
    let
        validatedTerms =
            List.map (\term -> Term term.body term.isAbbreviation False Nothing) item.terms

        validatedDetails =
            List.map (\detailsElem -> Details detailsElem Nothing) item.details
    in
    validate
        { terms = Array.fromList validatedTerms
        , details = Array.fromList validatedDetails
        , relatedTerms =
            item.relatedTerms
                |> List.map (\term -> RelatedTerm (Just term.idReference) Nothing)
                |> Array.fromList
        }


termBodyToId : String -> String
termBodyToId body =
    String.replace " " "_" body


toGlossaryItem : List GlossaryItem -> GlossaryItemForm -> GlossaryItem
toGlossaryItem glossaryItems form =
    let
        bodyByIdReference : Dict String String
        bodyByIdReference =
            List.foldl
                (\glossaryItem result ->
                    List.foldl
                        (\term result1 ->
                            Dict.update
                                term.id
                                (always <| Just term.body)
                                result1
                        )
                        result
                        glossaryItem.terms
                )
                Dict.empty
                glossaryItems
    in
    { terms =
        form.terms
            |> Array.toList
            |> List.map
                (\formTerm ->
                    { id = termBodyToId formTerm.body
                    , isAbbreviation = formTerm.isAbbreviation
                    , body = formTerm.body
                    }
                )
    , details =
        form.details
            |> Array.toList
            |> List.map .body
    , relatedTerms =
        form.relatedTerms
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
addTerm form =
    validate { form | terms = Array.push emptyTerm form.terms }


updateTerm : TermIndex -> GlossaryItemForm -> String -> GlossaryItemForm
updateTerm termIndex form body =
    validate
        { form
            | terms =
                Extras.Array.update
                    (\term -> { term | body = body })
                    (TermIndex.toInt termIndex)
                    form.terms
        }


deleteTerm : TermIndex -> GlossaryItemForm -> GlossaryItemForm
deleteTerm termIndex form =
    validate { form | terms = Extras.Array.delete (TermIndex.toInt termIndex) form.terms }


toggleAbbreviation : TermIndex -> GlossaryItemForm -> GlossaryItemForm
toggleAbbreviation termIndex form =
    validate
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


addDetails : GlossaryItemForm -> GlossaryItemForm
addDetails form =
    validate { form | details = Array.push emptyDetails form.details }


updateDetails : Int -> GlossaryItemForm -> String -> GlossaryItemForm
updateDetails index form body =
    validate
        { form
            | details =
                Extras.Array.update
                    (\detailsSingle -> { detailsSingle | body = body })
                    index
                    form.details
        }


deleteDetails : Int -> GlossaryItemForm -> GlossaryItemForm
deleteDetails index form =
    validate { form | details = Extras.Array.delete index form.details }


addRelatedTerm : GlossaryItemForm -> GlossaryItemForm
addRelatedTerm form =
    validate
        { form
            | relatedTerms = Array.push emptyRelatedTerm form.relatedTerms
        }


selectRelatedTerm : Int -> GlossaryItemForm -> Maybe String -> GlossaryItemForm
selectRelatedTerm index form relatedTermIdReference =
    validate
        { form
            | relatedTerms =
                Extras.Array.update
                    (always <| RelatedTerm relatedTermIdReference Nothing)
                    index
                    form.relatedTerms
        }


deleteRelatedTerm : Int -> GlossaryItemForm -> GlossaryItemForm
deleteRelatedTerm index form =
    validate { form | relatedTerms = Extras.Array.delete index form.relatedTerms }
