module GlossaryItemForm exposing
    ( GlossaryItemForm
    , RelatedTermField
    , addRelatedTerm
    , addTerm
    , definitionField
    , deleteRelatedTerm
    , deleteTerm
    , disambiguationTagId
    , empty
    , fromGlossaryItemForUi
    , hasValidationErrors
    , moveRelatedTermDown
    , moveRelatedTermUp
    , moveTermDown
    , moveTermUp
    , needsUpdating
    , relatedTermFields
    , relocateRelatedTerm
    , relocateTerm
    , selectRelatedTerm
    , suggestRelatedTerms
    , tagCheckboxes
    , termFields
    , toGlossaryItem
    , toggleAbbreviation
    , toggleNeedsUpdating
    , toggleTagCheckbox
    , updateDefinition
    , updateDisambiguationTagId
    , updateRelatedTermComboboxInput
    , updateRelatedTermFieldCombobox
    , updateTerm
    )

import Array exposing (Array)
import Components.Combobox
import Data.GlossaryItem.Definition as Definition
import Data.GlossaryItem.DisambiguatedTerm as DisambiguatedTerm exposing (DisambiguatedTerm)
import Data.GlossaryItem.RawTerm as RawTerm exposing (RawTerm)
import Data.GlossaryItem.Tag exposing (Tag)
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItemForUi as GlossaryItemForUi exposing (GlossaryItemForUi)
import Data.GlossaryItemId exposing (GlossaryItemId)
import Data.GlossaryItemsForUi as GlossaryItems exposing (GlossaryItemsForUi)
import Data.RelatedTermIndex as RelatedTermIndex exposing (RelatedTermIndex)
import Data.TagId exposing (TagId)
import Data.TermIndex as TermIndex exposing (TermIndex)
import Dict exposing (Dict)
import ElementIds
import Extras.Array
import Extras.List
import Extras.Regex
import GlossaryItemForm.DefinitionField as DefinitionField exposing (DefinitionField)
import GlossaryItemForm.TermField as TermField exposing (TermField)
import Internationalisation as I18n
import Regex
import Set exposing (Set)


type alias RelatedTermField =
    { raw : Maybe RawTerm
    , validationError : Maybe String
    , combobox : Components.Combobox.Model
    , comboboxInput : String
    }


type GlossaryItemForm
    = GlossaryItemForm
        { preferredTermField : TermField
        , alternativeTermFields : Array TermField
        , tagCheckboxes : List ( ( TagId, Tag ), Bool )
        , disambiguationTagId : Maybe TagId
        , definitionField : DefinitionField
        , relatedTermFields : Array RelatedTermField
        , preferredTermsOutside : List DisambiguatedTerm
        , preferredTermsOfItemsListingThisItemAsRelated : List DisambiguatedTerm
        , needsUpdating : Bool
        , lastUpdatedDate : String
        }


preferredTermField : GlossaryItemForm -> TermField
preferredTermField glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            form.preferredTermField


alternativeTermFields : GlossaryItemForm -> Array TermField
alternativeTermFields glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            form.alternativeTermFields


termFields : GlossaryItemForm -> Array TermField
termFields form =
    form
        |> alternativeTermFields
        |> Array.toList
        |> (::) (preferredTermField form)
        |> Array.fromList


tagCheckboxes : GlossaryItemForm -> List ( ( TagId, Tag ), Bool )
tagCheckboxes glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            form.tagCheckboxes


disambiguationTagId : GlossaryItemForm -> Maybe TagId
disambiguationTagId glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            form.disambiguationTagId


definitionField : GlossaryItemForm -> DefinitionField
definitionField glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            form.definitionField


relatedTermFields : GlossaryItemForm -> Array RelatedTermField
relatedTermFields glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            form.relatedTermFields


disambiguatedPreferredTermsOutside : GlossaryItemForm -> List DisambiguatedTerm
disambiguatedPreferredTermsOutside glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            form.preferredTermsOutside


preferredTermsOfItemsListingThisItemAsRelated : GlossaryItemForm -> List DisambiguatedTerm
preferredTermsOfItemsListingThisItemAsRelated glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            form.preferredTermsOfItemsListingThisItemAsRelated


needsUpdating : GlossaryItemForm -> Bool
needsUpdating glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            form.needsUpdating


lastUpdatedDate : GlossaryItemForm -> String
lastUpdatedDate glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm _ ->
            "2011-11-18T14:54:39.929Z"


validate : GlossaryItemForm -> GlossaryItemForm
validate form =
    let
        cannotBeEmptyMessage : String
        cannotBeEmptyMessage =
            I18n.thisFieldCannotBeEmpty

        rawPreferredTermsOutsideSet : Set String
        rawPreferredTermsOutsideSet =
            form
                |> disambiguatedPreferredTermsOutside
                |> List.map (DisambiguatedTerm.toTerm >> Term.raw >> RawTerm.toString)
                |> Set.fromList

        rawTermsInsideForm : Dict String Int
        rawTermsInsideForm =
            form
                |> termFields
                |> Array.map TermField.raw
                |> Array.foldl
                    (\rawTerm ->
                        Dict.update
                            rawTerm
                            (Maybe.map ((+) 1) >> Maybe.withDefault 1 >> Just)
                    )
                    Dict.empty

        validateTermField : Bool -> TermField -> TermField
        validateTermField isPreferredTerm termField =
            let
                body : String
                body =
                    termField |> TermField.raw |> String.trim
            in
            termField
                |> TermField.setValidationError
                    (if String.isEmpty body then
                        Just cannotBeEmptyMessage

                     else
                        let
                            term : Term
                            term =
                                Term.fromMarkdown body False

                            disambiguatedTerm : DisambiguatedTerm
                            disambiguatedTerm =
                                if isPreferredTerm then
                                    form
                                        |> disambiguationTagId
                                        |> Maybe.map
                                            (\disambiguationTagId_ ->
                                                form
                                                    |> tagCheckboxes
                                                    |> List.filterMap
                                                        (\( ( tagId, tag ), _ ) ->
                                                            if tagId == disambiguationTagId_ then
                                                                Just tag

                                                            else
                                                                Nothing
                                                        )
                                                    |> List.head
                                                    |> Maybe.map (\disambiguationTag -> GlossaryItemForUi.disambiguatedTerm disambiguationTag term)
                                                    |> Maybe.withDefault (DisambiguatedTerm.fromTerm term)
                                            )
                                        |> Maybe.withDefault (DisambiguatedTerm.fromTerm term)

                                else
                                    DisambiguatedTerm.fromTerm term

                            rawTerm : String
                            rawTerm =
                                disambiguatedTerm
                                    |> DisambiguatedTerm.toTerm
                                    |> Term.raw
                                    |> RawTerm.toString
                        in
                        if isPreferredTerm && Set.member rawTerm rawPreferredTermsOutsideSet then
                            Just I18n.thisTermAlreadyExistsElsewhere

                        else if (Dict.get rawTerm rawTermsInsideForm |> Maybe.withDefault 0) > 1 then
                            Just I18n.thisTermOccursMultipleTimes

                        else if ElementIds.reserved rawTerm then
                            Just I18n.thisTermIsReserved

                        else
                            Nothing
                    )

        validatedPreferredTermField : TermField
        validatedPreferredTermField =
            form |> preferredTermField |> validateTermField True

        validatedAlternativeTermFields : Array TermField
        validatedAlternativeTermFields =
            form |> alternativeTermFields |> Array.map (validateTermField False)

        validatedDefinitionField : DefinitionField
        validatedDefinitionField =
            definitionField form

        validatedRelatedTermFields : Array RelatedTermField
        validatedRelatedTermFields =
            form
                |> relatedTermFields
                |> Array.map
                    (\relatedTermField ->
                        { relatedTermField
                            | validationError =
                                if relatedTermField.raw == Nothing then
                                    Just I18n.pleaseSelectAnItem

                                else
                                    Nothing
                        }
                    )
    in
    GlossaryItemForm
        { preferredTermField = validatedPreferredTermField
        , alternativeTermFields = validatedAlternativeTermFields
        , tagCheckboxes = tagCheckboxes form
        , disambiguationTagId = disambiguationTagId form
        , definitionField = validatedDefinitionField
        , relatedTermFields = validatedRelatedTermFields
        , preferredTermsOutside = disambiguatedPreferredTermsOutside form
        , preferredTermsOfItemsListingThisItemAsRelated = preferredTermsOfItemsListingThisItemAsRelated form
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
        || (form |> definitionField |> DefinitionField.validationError |> (/=) Nothing)
        || (form |> relatedTermFields |> hasErrors .validationError)


empty : GlossaryItemsForUi -> Maybe Tag -> GlossaryItemForm
empty items filterByTag =
    let
        tags : List ( TagId, Tag )
        tags =
            GlossaryItems.tagByIdList items

        existingDisambiguatedPreferredTerms : List DisambiguatedTerm
        existingDisambiguatedPreferredTerms =
            items
                |> GlossaryItems.disambiguatedPreferredTerms Nothing
                |> List.map Tuple.second

        filterByTagId : Maybe TagId
        filterByTagId =
            filterByTag
                |> Maybe.andThen (\tag -> GlossaryItems.tagIdFromTag tag items)
    in
    empty_
        existingDisambiguatedPreferredTerms
        tags
        filterByTagId


empty_ : List DisambiguatedTerm -> List ( TagId, Tag ) -> Maybe TagId -> GlossaryItemForm
empty_ withPreferredTermsOutside allTags filterByTag =
    GlossaryItemForm
        { preferredTermField = TermField.empty
        , alternativeTermFields = Array.empty
        , tagCheckboxes =
            List.map
                (\( tagId, tag ) ->
                    ( ( tagId, tag ), Just tagId == filterByTag )
                )
                allTags
        , disambiguationTagId = Nothing
        , definitionField = DefinitionField.empty
        , relatedTermFields = Array.empty
        , preferredTermsOutside = withPreferredTermsOutside
        , preferredTermsOfItemsListingThisItemAsRelated = []
        , needsUpdating = True
        , lastUpdatedDate = ""
        }
        |> validate


emptyRelatedTermField : RelatedTermField
emptyRelatedTermField =
    { raw = Nothing
    , validationError = Nothing
    , combobox = Components.Combobox.init
    , comboboxInput = ""
    }


fromGlossaryItemForUi : GlossaryItemsForUi -> GlossaryItemId -> GlossaryItemForUi -> GlossaryItemForm
fromGlossaryItemForUi items itemId item =
    let
        tags : List ( TagId, Tag )
        tags =
            GlossaryItems.tagByIdList items

        existingDisambiguatedPreferredTerms : List DisambiguatedTerm
        existingDisambiguatedPreferredTerms =
            items
                |> GlossaryItems.disambiguatedPreferredTerms Nothing
                |> List.map Tuple.second

        preferredTermsOfItemsListingThisItemAsRelated_ : List DisambiguatedTerm
        preferredTermsOfItemsListingThisItemAsRelated_ =
            GlossaryItems.preferredTermsOfItemsListingThisItemAsRelated itemId items

        disambiguationTagId_ : Maybe TagId
        disambiguationTagId_ =
            item
                |> GlossaryItemForUi.disambiguationTag
                |> Maybe.andThen (\tag -> GlossaryItems.tagIdFromTag tag items)
    in
    fromGlossaryItemForUi_ existingDisambiguatedPreferredTerms
        tags
        preferredTermsOfItemsListingThisItemAsRelated_
        (GlossaryItemForUi.relatedPreferredTerms item)
        disambiguationTagId_
        item


fromGlossaryItemForUi_ :
    List DisambiguatedTerm
    -> List ( TagId, Tag )
    -> List DisambiguatedTerm
    -> List DisambiguatedTerm
    -> Maybe TagId
    -> GlossaryItemForUi
    -> GlossaryItemForm
fromGlossaryItemForUi_ existingPreferredTerms allTags preferredTermsOfItemsListingThisItemAsRelated_ relatedTerms disambiguationTagId_ item =
    let
        normalTags : List Tag
        normalTags =
            GlossaryItemForUi.normalTags item

        preferredTermFieldForItem : TermField
        preferredTermFieldForItem =
            let
                preferredTerm : Term
                preferredTerm =
                    GlossaryItemForUi.nonDisambiguatedPreferredTerm item
            in
            TermField.fromString
                (preferredTerm |> Term.raw |> RawTerm.toString)
                (Term.isAbbreviation preferredTerm)

        alternativeTermFieldsForItem : List TermField
        alternativeTermFieldsForItem =
            item
                |> GlossaryItemForUi.alternativeTerms
                |> List.map
                    (\alternativeTerm ->
                        TermField.fromString (alternativeTerm |> Term.raw |> RawTerm.toString) (Term.isAbbreviation alternativeTerm)
                    )

        rawTermsForItem : Set String
        rawTermsForItem =
            item
                |> GlossaryItemForUi.allTerms
                |> List.map (Term.raw >> RawTerm.toString)
                |> Set.fromList

        preferredTermsOutside1 : List DisambiguatedTerm
        preferredTermsOutside1 =
            List.filter
                (\existingTerm ->
                    not <|
                        Set.member
                            (existingTerm
                                |> DisambiguatedTerm.toTerm
                                |> Term.raw
                                |> RawTerm.toString
                            )
                            rawTermsForItem
                )
                existingPreferredTerms

        definitionField_ : DefinitionField
        definitionField_ =
            item
                |> GlossaryItemForUi.definition
                |> Maybe.map
                    (\definitionElem ->
                        definitionElem
                            |> Definition.raw
                            |> DefinitionField.fromString
                    )
                |> Maybe.withDefault DefinitionField.empty
    in
    GlossaryItemForm
        { preferredTermField = preferredTermFieldForItem
        , alternativeTermFields = Array.fromList alternativeTermFieldsForItem
        , tagCheckboxes =
            List.map
                (\( tagId, tag ) ->
                    ( ( tagId, tag ), List.member tag normalTags || Just tagId == disambiguationTagId_ )
                )
                allTags
        , disambiguationTagId = disambiguationTagId_
        , definitionField = definitionField_
        , relatedTermFields =
            relatedTerms
                |> List.map
                    (\term ->
                        RelatedTermField
                            (Just <| Term.raw <| DisambiguatedTerm.toTerm term)
                            Nothing
                            Components.Combobox.init
                            (DisambiguatedTerm.toTerm term |> Term.raw |> RawTerm.toString)
                    )
                |> Array.fromList
        , preferredTermsOutside = preferredTermsOutside1
        , preferredTermsOfItemsListingThisItemAsRelated = preferredTermsOfItemsListingThisItemAsRelated_
        , needsUpdating = item |> GlossaryItemForUi.needsUpdating
        , lastUpdatedDate = item |> GlossaryItemForUi.lastUpdatedDateAsIso8601 |> Maybe.withDefault ""
        }
        |> validate


toGlossaryItem : GlossaryItemsForUi -> GlossaryItemForm -> GlossaryItemId -> Maybe String -> GlossaryItemForUi
toGlossaryItem glossaryItems form id dateTime =
    let
        termFieldToTerm : TermField -> Term
        termFieldToTerm termField =
            let
                raw : String
                raw =
                    termField |> TermField.raw |> String.trim

                isAbbreviation : Bool
                isAbbreviation =
                    TermField.isAbbreviation termField
            in
            Term.fromMarkdown raw isAbbreviation

        preferredTerm : Term
        preferredTerm =
            form
                |> preferredTermField
                |> termFieldToTerm

        alternativeTerms : List Term
        alternativeTerms =
            form
                |> alternativeTermFields
                |> Array.toList
                |> List.map termFieldToTerm

        normalTags : List Tag
        normalTags =
            form
                |> tagCheckboxes
                |> List.filterMap
                    (\( ( tagId, tag ), checked ) ->
                        if checked && Just tagId /= disambiguationTagId form then
                            Just tag

                        else
                            Nothing
                    )

        disambiguationTag : Maybe Tag
        disambiguationTag =
            form
                |> disambiguationTagId
                |> Maybe.andThen (\tagId -> GlossaryItems.tagFromId tagId glossaryItems)

        definition : Maybe Definition.Definition
        definition =
            form
                |> definitionField
                |> DefinitionField.raw
                |> String.trim
                |> (\definitionString ->
                        if String.isEmpty definitionString then
                            Nothing

                        else
                            Just definitionString
                   )
                |> Maybe.map Definition.fromMarkdown

        relatedTerms : List DisambiguatedTerm
        relatedTerms =
            form
                |> relatedTermFields
                |> Array.toList
                |> List.filterMap
                    (\relatedTermField ->
                        relatedTermField.raw
                            |> Maybe.andThen
                                (\rawTerm ->
                                    GlossaryItems.itemIdFromRawDisambiguatedPreferredTerm
                                        rawTerm
                                        glossaryItems
                                )
                            |> Maybe.andThen
                                (\itemId ->
                                    GlossaryItems.disambiguatedPreferredTerm
                                        itemId
                                        glossaryItems
                                )
                    )

        needsUpdating_ : Bool
        needsUpdating_ =
            needsUpdating form

        lastUpdatedDate_ : Maybe String
        lastUpdatedDate_ =
            dateTime
    in
    GlossaryItemForUi.create
        id
        preferredTerm
        alternativeTerms
        disambiguationTag
        normalTags
        definition
        relatedTerms
        needsUpdating_
        lastUpdatedDate_
        Nothing
        Nothing


addTerm : GlossaryItemForm -> GlossaryItemForm
addTerm glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            GlossaryItemForm
                { form | alternativeTermFields = Array.push TermField.empty form.alternativeTermFields }
                |> validate


updateTerm : TermIndex -> String -> GlossaryItemForm -> GlossaryItemForm
updateTerm termIndex body glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            (if TermIndex.toInt termIndex == 0 then
                GlossaryItemForm
                    { form | preferredTermField = TermField.setBody body form.preferredTermField }

             else
                GlossaryItemForm
                    { form
                        | alternativeTermFields =
                            Extras.Array.update
                                (TermField.setBody body)
                                (TermIndex.toInt termIndex - 1)
                                form.alternativeTermFields
                    }
            )
                |> validate


moveTermUp : TermIndex -> GlossaryItemForm -> GlossaryItemForm
moveTermUp index ((GlossaryItemForm form) as glossaryItemForm) =
    let
        termFields0 : Array TermField
        termFields0 =
            termFields glossaryItemForm

        indexInt : Int
        indexInt =
            TermIndex.toInt index

        maybeCurrentAtIndex : Maybe TermField
        maybeCurrentAtIndex =
            Array.get indexInt termFields0

        maybeCurrentAtPreviousIndex : Maybe TermField
        maybeCurrentAtPreviousIndex =
            Array.get (indexInt - 1) termFields0

        termFields1 : Array TermField
        termFields1 =
            Maybe.map2
                (\currentAtIndex currentAtPreviousIndex ->
                    termFields0
                        |> Array.set (indexInt - 1) currentAtIndex
                        |> Array.set indexInt currentAtPreviousIndex
                )
                maybeCurrentAtIndex
                maybeCurrentAtPreviousIndex
                |> Maybe.withDefault termFields0
    in
    GlossaryItemForm
        { form
            | preferredTermField =
                termFields1
                    |> Array.get 0
                    |> Maybe.withDefault TermField.empty
            , alternativeTermFields = Extras.Array.delete 0 termFields1
        }


moveTermDown : TermIndex -> GlossaryItemForm -> GlossaryItemForm
moveTermDown index ((GlossaryItemForm form) as glossaryItemForm) =
    let
        termFields0 : Array TermField
        termFields0 =
            termFields glossaryItemForm

        indexInt : Int
        indexInt =
            TermIndex.toInt index

        maybeCurrentAtIndex : Maybe TermField
        maybeCurrentAtIndex =
            Array.get indexInt termFields0

        maybeCurrentAtNextIndex : Maybe TermField
        maybeCurrentAtNextIndex =
            Array.get (indexInt + 1) termFields0

        termFields1 : Array TermField
        termFields1 =
            Maybe.map2
                (\currentAtIndex currentAtNextIndex ->
                    termFields0
                        |> Array.set (indexInt + 1) currentAtIndex
                        |> Array.set indexInt currentAtNextIndex
                )
                maybeCurrentAtIndex
                maybeCurrentAtNextIndex
                |> Maybe.withDefault termFields0
    in
    GlossaryItemForm
        { form
            | preferredTermField =
                termFields1
                    |> Array.get 0
                    |> Maybe.withDefault TermField.empty
            , alternativeTermFields = Extras.Array.delete 0 termFields1
        }


relocateTerm : TermIndex -> TermIndex -> GlossaryItemForm -> GlossaryItemForm
relocateTerm sourceIndex destinationIndex ((GlossaryItemForm form) as glossaryItemForm) =
    let
        termFields1 : Array TermField
        termFields1 =
            glossaryItemForm
                |> termFields
                |> Array.toList
                |> Extras.List.relocate (TermIndex.toInt sourceIndex) (TermIndex.toInt destinationIndex)
                |> Array.fromList
    in
    GlossaryItemForm
        { form
            | preferredTermField =
                termFields1
                    |> Array.get 0
                    |> Maybe.withDefault TermField.empty
            , alternativeTermFields = Extras.Array.delete 0 termFields1
        }


deleteTerm : TermIndex -> GlossaryItemForm -> GlossaryItemForm
deleteTerm termIndex glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            (if TermIndex.toInt termIndex == 0 then
                GlossaryItemForm
                    { form
                        | preferredTermField =
                            form.alternativeTermFields
                                |> Array.get 0
                                |> Maybe.withDefault TermField.empty
                        , alternativeTermFields = Extras.Array.delete 0 form.alternativeTermFields
                    }

             else
                GlossaryItemForm
                    { form | alternativeTermFields = Extras.Array.delete (TermIndex.toInt termIndex - 1) form.alternativeTermFields }
            )
                |> validate


toggleAbbreviation : TermIndex -> GlossaryItemForm -> GlossaryItemForm
toggleAbbreviation termIndex glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            GlossaryItemForm
                (if TermIndex.toInt termIndex == 0 then
                    { form
                        | preferredTermField =
                            form.preferredTermField
                                |> TermField.setIsAbbreviation (not <| TermField.isAbbreviation form.preferredTermField)
                                |> TermField.setIsAbbreviationManuallyOverridden True
                    }

                 else
                    { form
                        | alternativeTermFields =
                            Extras.Array.update
                                (\termField ->
                                    termField
                                        |> TermField.setIsAbbreviation (not <| TermField.isAbbreviation termField)
                                        |> TermField.setIsAbbreviationManuallyOverridden True
                                )
                                (TermIndex.toInt termIndex - 1)
                                form.alternativeTermFields
                    }
                )
                |> validate


toggleTagCheckbox : Tag -> GlossaryItemForm -> GlossaryItemForm
toggleTagCheckbox tag glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            let
                tagIdAndChecked : Maybe ( TagId, Bool )
                tagIdAndChecked =
                    form.tagCheckboxes
                        |> List.filterMap
                            (\( ( tagId, tag_ ), checked ) ->
                                if tag_ == tag then
                                    Just ( tagId, checked )

                                else
                                    Nothing
                            )
                        |> List.head

                tagCheckboxes_ : List ( ( TagId, Tag ), Bool )
                tagCheckboxes_ =
                    form.tagCheckboxes
                        |> List.map
                            (\( ( tagId, tag_ ), checked ) ->
                                if tag_ == tag then
                                    ( ( tagId, tag_ ), not checked )

                                else
                                    ( ( tagId, tag_ ), checked )
                            )

                disambiguationTagId_ : Maybe TagId
                disambiguationTagId_ =
                    tagIdAndChecked
                        |> Maybe.map
                            (\( tagId, checked ) ->
                                if Just tagId == form.disambiguationTagId && checked then
                                    Nothing

                                else
                                    form.disambiguationTagId
                            )
                        |> Maybe.withDefault form.disambiguationTagId
            in
            GlossaryItemForm
                { form
                    | tagCheckboxes = tagCheckboxes_
                    , disambiguationTagId = disambiguationTagId_
                }


updateDefinition : String -> GlossaryItemForm -> GlossaryItemForm
updateDefinition body glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            let
                needsUpdating1 : Bool
                needsUpdating1 =
                    if not (formHasDefinitionOrRelatedTerms glossaryItemForm) && form.needsUpdating then
                        False

                    else
                        form.needsUpdating
            in
            GlossaryItemForm
                { form
                    | definitionField = DefinitionField.fromString body
                    , needsUpdating = needsUpdating1
                }
                |> validate


updateDisambiguationTagId : Maybe TagId -> GlossaryItemForm -> GlossaryItemForm
updateDisambiguationTagId tagId glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            GlossaryItemForm
                { form | disambiguationTagId = tagId }
                |> validate


formHasDefinitionOrRelatedTerms : GlossaryItemForm -> Bool
formHasDefinitionOrRelatedTerms glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            form.definitionField /= DefinitionField.empty || form.relatedTermFields /= Array.empty


addRelatedTerm : Maybe RawTerm -> GlossaryItemForm -> GlossaryItemForm
addRelatedTerm maybeRawTerm glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            let
                relatedTermField : RelatedTermField
                relatedTermField =
                    maybeRawTerm
                        |> Maybe.map
                            (\rawTerm ->
                                { raw = Just rawTerm
                                , validationError = Nothing
                                , combobox = Components.Combobox.init
                                , comboboxInput = RawTerm.toString rawTerm
                                }
                            )
                        |> Maybe.withDefault emptyRelatedTermField

                needsUpdating1 : Bool
                needsUpdating1 =
                    if not (formHasDefinitionOrRelatedTerms glossaryItemForm) && form.needsUpdating then
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


updateRelatedTermFieldCombobox : RelatedTermIndex -> Components.Combobox.Model -> GlossaryItemForm -> GlossaryItemForm
updateRelatedTermFieldCombobox index combobox (GlossaryItemForm form) =
    GlossaryItemForm
        { form
            | relatedTermFields =
                Extras.Array.update
                    (\relatedTermField -> { relatedTermField | combobox = combobox })
                    (RelatedTermIndex.toInt index)
                    form.relatedTermFields
        }
        |> validate


updateRelatedTermComboboxInput : Bool -> RelatedTermIndex -> String -> GlossaryItemForm -> GlossaryItemForm
updateRelatedTermComboboxInput hideChoices index input (GlossaryItemForm form) =
    GlossaryItemForm
        { form
            | relatedTermFields =
                Extras.Array.update
                    (\relatedTermField ->
                        { relatedTermField
                            | combobox =
                                (if hideChoices then
                                    Components.Combobox.hideChoices

                                 else
                                    Components.Combobox.showChoices
                                )
                                    relatedTermField.combobox
                            , comboboxInput = input
                        }
                    )
                    (RelatedTermIndex.toInt index)
                    form.relatedTermFields
        }
        |> validate


selectRelatedTerm : RelatedTermIndex -> Maybe RawTerm -> GlossaryItemForm -> GlossaryItemForm
selectRelatedTerm index relatedRawTerm glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            GlossaryItemForm
                { form
                    | relatedTermFields =
                        Extras.Array.update
                            (always <|
                                RelatedTermField relatedRawTerm
                                    Nothing
                                    Components.Combobox.init
                                    (relatedRawTerm |> Maybe.map RawTerm.toString |> Maybe.withDefault "")
                            )
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


moveRelatedTermUp : RelatedTermIndex -> GlossaryItemForm -> GlossaryItemForm
moveRelatedTermUp index glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            let
                relatedTermFields0 : Array RelatedTermField
                relatedTermFields0 =
                    form.relatedTermFields

                indexInt : Int
                indexInt =
                    RelatedTermIndex.toInt index

                maybeCurrentAtIndex : Maybe RelatedTermField
                maybeCurrentAtIndex =
                    Array.get indexInt form.relatedTermFields

                maybeCurrentAtPreviousIndex : Maybe RelatedTermField
                maybeCurrentAtPreviousIndex =
                    Array.get (indexInt - 1) form.relatedTermFields
            in
            GlossaryItemForm
                { form
                    | relatedTermFields =
                        Maybe.map2
                            (\currentAtIndex currentAtPreviousIndex ->
                                relatedTermFields0
                                    |> Array.set (indexInt - 1) currentAtIndex
                                    |> Array.set indexInt currentAtPreviousIndex
                            )
                            maybeCurrentAtIndex
                            maybeCurrentAtPreviousIndex
                            |> Maybe.withDefault relatedTermFields0
                }
                |> validate


moveRelatedTermDown : RelatedTermIndex -> GlossaryItemForm -> GlossaryItemForm
moveRelatedTermDown index glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            let
                relatedTermFields0 : Array RelatedTermField
                relatedTermFields0 =
                    form.relatedTermFields

                indexInt : Int
                indexInt =
                    RelatedTermIndex.toInt index

                maybeCurrentAtIndex : Maybe RelatedTermField
                maybeCurrentAtIndex =
                    Array.get indexInt form.relatedTermFields

                maybeCurrentAtNextIndex : Maybe RelatedTermField
                maybeCurrentAtNextIndex =
                    Array.get (indexInt + 1) form.relatedTermFields
            in
            GlossaryItemForm
                { form
                    | relatedTermFields =
                        Maybe.map2
                            (\currentAtIndex currentAtNextIndex ->
                                relatedTermFields0
                                    |> Array.set (indexInt + 1) currentAtIndex
                                    |> Array.set indexInt currentAtNextIndex
                            )
                            maybeCurrentAtIndex
                            maybeCurrentAtNextIndex
                            |> Maybe.withDefault relatedTermFields0
                }
                |> validate


relocateRelatedTerm : RelatedTermIndex -> RelatedTermIndex -> GlossaryItemForm -> GlossaryItemForm
relocateRelatedTerm sourceIndex destinationIndex ((GlossaryItemForm form) as glossaryItemForm) =
    let
        relatedTermFields1 : Array RelatedTermField
        relatedTermFields1 =
            glossaryItemForm
                |> relatedTermFields
                |> Array.toList
                |> Extras.List.relocate (RelatedTermIndex.toInt sourceIndex) (RelatedTermIndex.toInt destinationIndex)
                |> Array.fromList
    in
    GlossaryItemForm { form | relatedTermFields = relatedTermFields1 }


suggestRelatedTerms : GlossaryItemForm -> List DisambiguatedTerm
suggestRelatedTerms glossaryItemForm =
    let
        relatedRawTermsAlreadyInForm : Set String
        relatedRawTermsAlreadyInForm =
            glossaryItemForm
                |> relatedTermFields
                |> Array.foldl
                    (\relatedTermField result ->
                        relatedTermField.raw
                            |> Maybe.map (\rawTerm -> Set.insert (RawTerm.toString rawTerm) result)
                            |> Maybe.withDefault result
                    )
                    Set.empty

        definitionFieldBody : String
        definitionFieldBody =
            glossaryItemForm
                |> definitionField
                |> DefinitionField.raw

        rawDisambiguatedPreferredTermsOfItemsListingThisItemAsRelated : Set String
        rawDisambiguatedPreferredTermsOfItemsListingThisItemAsRelated =
            glossaryItemForm
                |> preferredTermsOfItemsListingThisItemAsRelated
                |> List.map (DisambiguatedTerm.toTerm >> Term.raw >> RawTerm.toString)
                |> Set.fromList
    in
    suggestRelatedTerms_
        relatedRawTermsAlreadyInForm
        rawDisambiguatedPreferredTermsOfItemsListingThisItemAsRelated
        (disambiguatedPreferredTermsOutside glossaryItemForm)
        definitionFieldBody


toggleNeedsUpdating : GlossaryItemForm -> GlossaryItemForm
toggleNeedsUpdating glossaryItemForm =
    case glossaryItemForm of
        GlossaryItemForm form ->
            GlossaryItemForm
                { form
                    | needsUpdating = not form.needsUpdating
                }
                |> validate


suggestRelatedTerms_ :
    Set String
    -> Set String
    -> List DisambiguatedTerm
    -> String
    -> List DisambiguatedTerm
suggestRelatedTerms_ relatedRawTermsAlreadyInForm rawDisambiguatedPreferredTermsOfItemsListingThisItemAsRelated disambiguatedPreferredTermsOutside_ definitionFieldBody =
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
