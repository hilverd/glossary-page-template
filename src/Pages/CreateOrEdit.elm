port module Pages.CreateOrEdit exposing (InternalMsg, Model, Msg, init, subscriptions, update, view)

import Accessibility exposing (Html, article, div, dl, form, h1, h2, p, span, text)
import Accessibility.Aria
import Array exposing (Array)
import Browser exposing (Document)
import Browser.Dom as Dom
import CommonModel exposing (CommonModel)
import Components.Badge
import Components.Button
import Components.DragAndDrop
import Components.DropdownMenu
import Components.Form
import Components.GlossaryItemCard
import Components.SelectMenu
import Components.Spinner
import Data.Editability as Editability
import Data.GlossaryChange as GlossaryChange exposing (GlossaryChange)
import Data.GlossaryChangeWithChecksum exposing (GlossaryChangeWithChecksum)
import Data.GlossaryChangelist as GlossaryChangelist
import Data.GlossaryForUi as Glossary
import Data.GlossaryItem.DisambiguatedTerm as DisambiguatedTerm exposing (DisambiguatedTerm)
import Data.GlossaryItem.RawTerm as RawTerm exposing (RawTerm)
import Data.GlossaryItem.Tag as Tag exposing (Tag)
import Data.GlossaryItem.Term as Term
import Data.GlossaryItemForUi as GlossaryItemForUi exposing (GlossaryItemForUi)
import Data.GlossaryItemId as GlossaryItemId exposing (GlossaryItemId)
import Data.GlossaryItemsForUi as GlossaryItemsForUi exposing (GlossaryItemsForUi)
import Data.GlossaryTitle as GlossaryTitle
import Data.RelatedTermIndex as RelatedTermIndex exposing (RelatedTermIndex)
import Data.Saving exposing (Saving(..))
import Data.TagId as TagId exposing (TagId)
import Data.TermIndex as TermIndex exposing (TermIndex)
import Dict exposing (Dict)
import ElementIds
import Extras.Html
import Extras.HtmlAttribute
import Extras.HtmlEvents
import Extras.Http
import GlossaryItemForm as Form exposing (GlossaryItemForm)
import GlossaryItemForm.DefinitionField as DefinitionField exposing (DefinitionField)
import GlossaryItemForm.TermField as TermField exposing (TermField)
import Html
import Html.Attributes exposing (class, id, required, style)
import Html.Events
import Http
import Icons
import Internationalisation as I18n
import Json.Decode
import PageMsg exposing (PageMsg)
import QueryParameters
import Save
import Set exposing (Set)
import Svg.Attributes
import Task



-- MODEL


type TentativeDragAndDropChangesToShow
    = NoDragAndDropInProgress
    | TermBeingRelocated TermIndex (Maybe TermIndex)
    | RelatedTermBeingRelocated RelatedTermIndex (Maybe RelatedTermIndex)


type alias Model =
    { common : CommonModel
    , itemBeingEdited : Maybe GlossaryItemId
    , form : GlossaryItemForm
    , tentativeDragAndDropChangesToShow : TentativeDragAndDropChangesToShow
    , triedToSaveWhenFormInvalid : Bool
    , saving : Saving
    , dropdownMenusWithMoreOptionsForTerms : Dict Int Components.DropdownMenu.Model
    , dragAndDropTerms : Components.DragAndDrop.Model TermIndex TermIndex
    , dropdownMenusWithMoreOptionsForRelatedTerms : Dict Int Components.DropdownMenu.Model
    , dragAndDropRelatedTerms : Components.DragAndDrop.Model RelatedTermIndex RelatedTermIndex
    }


updateForm : (GlossaryItemForm -> GlossaryItemForm) -> Model -> Model
updateForm f model =
    { model
        | form = f model.form
        , saving =
            case model.saving of
                SavingNotAttempted _ ->
                    NotCurrentlySaving

                _ ->
                    model.saving
    }


type InternalMsg
    = NoOp
    | AddTerm
    | DeleteTerm TermIndex
    | UpdateTerm TermIndex String
    | DragAndDropTermsMsg (Components.DragAndDrop.Msg TermIndex TermIndex)
    | MoveTermUp Bool TermIndex
    | MoveTermDown Bool Int TermIndex
    | ToggleAbbreviation TermIndex
    | ToggleTagCheckbox Tag
    | UpdateDefinition String
    | SelectDisambiguationTag String
    | AddRelatedTerm (Maybe RawTerm)
    | SelectRelatedTerm RelatedTermIndex String
    | DeleteRelatedTerm RelatedTermIndex
    | DropdownMenuWithMoreOptionsForTermMsg Int Components.DropdownMenu.Msg
    | DropdownMenuWithMoreOptionsForRelatedTermMsg Int Components.DropdownMenu.Msg
    | MoveRelatedTermUp Bool RelatedTermIndex
    | MoveRelatedTermDown Bool Int RelatedTermIndex
    | DragAndDropRelatedTermsMsg (Components.DragAndDrop.Msg RelatedTermIndex RelatedTermIndex)
    | ToggleNeedsUpdating
    | Save
    | ReceiveCurrentDateTimeAndNewIdForSaving ( String, String )
    | FailedToSave Http.Error


type alias Msg =
    PageMsg InternalMsg


init : CommonModel -> Maybe GlossaryItemId -> ( Model, Cmd Msg )
init commonModel itemBeingEdited =
    case commonModel.glossaryForUi of
        Ok glossaryForUi ->
            let
                items : GlossaryItemsForUi
                items =
                    Glossary.items glossaryForUi

                emptyForm : GlossaryItemForm
                emptyForm =
                    Form.empty
                        items
                        (QueryParameters.filterByTag commonModel.queryParameters)

                form : GlossaryItemForm
                form =
                    Maybe.andThen
                        (\id ->
                            items
                                |> GlossaryItemsForUi.get id
                                |> Maybe.map
                                    (\itemForUi ->
                                        Form.fromGlossaryItemForUi
                                            items
                                            id
                                            itemForUi
                                    )
                        )
                        itemBeingEdited
                        |> Maybe.withDefault emptyForm
            in
            ( { itemBeingEdited = itemBeingEdited
              , common = commonModel
              , form = form
              , tentativeDragAndDropChangesToShow = NoDragAndDropInProgress
              , triedToSaveWhenFormInvalid = False
              , saving = NotCurrentlySaving
              , dragAndDropTerms = Components.DragAndDrop.init
              , dropdownMenusWithMoreOptionsForTerms =
                    dropdownMenusWithMoreOptionsForTermsForForm form
              , dropdownMenusWithMoreOptionsForRelatedTerms =
                    dropdownMenusWithMoreOptionsForRelatedTermsForForm form
              , dragAndDropRelatedTerms = Components.DragAndDrop.init
              }
            , if itemBeingEdited == Nothing then
                0 |> TermIndex.fromInt |> giveFocusToTermInputField

              else
                Cmd.none
            )

        Err _ ->
            ( { itemBeingEdited = itemBeingEdited
              , common = commonModel
              , form = Form.empty GlossaryItemsForUi.empty Nothing
              , tentativeDragAndDropChangesToShow = NoDragAndDropInProgress
              , triedToSaveWhenFormInvalid = False
              , saving = NotCurrentlySaving
              , dragAndDropTerms = Components.DragAndDrop.init
              , dropdownMenusWithMoreOptionsForTerms = Dict.empty
              , dropdownMenusWithMoreOptionsForRelatedTerms = Dict.empty
              , dragAndDropRelatedTerms = Components.DragAndDrop.init
              }
            , Cmd.none
            )


dropdownMenusWithMoreOptionsForRelatedTermsForForm : GlossaryItemForm -> Dict Int Components.DropdownMenu.Model
dropdownMenusWithMoreOptionsForRelatedTermsForForm form =
    form
        |> Form.relatedTermFields
        |> Array.indexedMap (\index _ -> index)
        |> Array.foldl
            (\index result ->
                Dict.insert
                    index
                    (Components.DropdownMenu.init
                        [ Components.DropdownMenu.id <| ElementIds.moreOptionsForRelatedTermDropdownMenu index
                        , Components.DropdownMenu.originTopLeft
                        ]
                    )
                    result
            )
            Dict.empty


dropdownMenusWithMoreOptionsForTermsForForm : GlossaryItemForm -> Dict Int Components.DropdownMenu.Model
dropdownMenusWithMoreOptionsForTermsForForm form =
    form
        |> Form.termFields
        |> Array.indexedMap (\index _ -> index)
        |> Array.foldl
            (\index result ->
                Dict.insert
                    index
                    (Components.DropdownMenu.init
                        [ Components.DropdownMenu.id <| ElementIds.moreOptionsForTermDropdownMenu index
                        , Components.DropdownMenu.originTopLeft
                        ]
                    )
                    result
            )
            Dict.empty



-- PORTS


port getCurrentDateTimeAndNewIdForSaving : () -> Cmd msg


port receiveCurrentDateTimeAndNewIdForSaving : (( String, String ) -> msg) -> Sub msg


port dragStart : Json.Decode.Value -> Cmd msg



-- UPDATE


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        AddTerm ->
            let
                form : GlossaryItemForm
                form =
                    Form.addTerm model.form

                latestTermIndex : TermIndex
                latestTermIndex =
                    Array.length (Form.termFields form) - 1 |> TermIndex.fromInt
            in
            ( updateForm (always form) model
            , giveFocusToTermInputField latestTermIndex
            )

        DeleteTerm termIndex ->
            ( updateForm (Form.deleteTerm termIndex) model
            , Task.attempt
                (\_ -> NoOp)
                (Dom.blur <| ElementIds.deleteTermButton <| TermIndex.toInt termIndex)
                |> Cmd.map PageMsg.Internal
            )

        UpdateTerm termIndex body ->
            ( updateForm (Form.updateTerm termIndex body) model
            , Cmd.none
            )

        DragAndDropTermsMsg msg_ ->
            let
                ( dragAndDropTerms_, result ) =
                    Components.DragAndDrop.update msg_ model.dragAndDropTerms

                dragId : Maybe TermIndex
                dragId =
                    Components.DragAndDrop.getDragId dragAndDropTerms_

                dropId : Maybe TermIndex
                dropId =
                    Components.DragAndDrop.getDropId dragAndDropTerms_

                tentativeDragAndDropChangesToShow : TentativeDragAndDropChangesToShow
                tentativeDragAndDropChangesToShow =
                    case ( dragId, dropId ) of
                        ( Just dragId_, Nothing ) ->
                            if model.tentativeDragAndDropChangesToShow == NoDragAndDropInProgress then
                                TermBeingRelocated dragId_ dropId

                            else
                                model.tentativeDragAndDropChangesToShow

                        ( Just dragId_, Just _ ) ->
                            TermBeingRelocated dragId_ dropId

                        _ ->
                            NoDragAndDropInProgress
            in
            ( { model
                | tentativeDragAndDropChangesToShow = tentativeDragAndDropChangesToShow
                , dragAndDropTerms = dragAndDropTerms_
              }
                |> (case result of
                        Nothing ->
                            identity

                        Just ( oldTermIndex_, newTermIndex_, _ ) ->
                            updateForm (updatedFormWithTermBeingRelocated oldTermIndex_ newTermIndex_)
                   )
            , Cmd.batch
                [ Components.DragAndDrop.getDragstartEvent msg_
                    |> Maybe.map (.event >> dragStart)
                    |> Maybe.withDefault Cmd.none
                , case result of
                    Nothing ->
                        Cmd.none

                    Just ( _, newTermIndex_, _ ) ->
                        Task.attempt
                            (\_ -> NoOp)
                            (Dom.focus <| ElementIds.dragTermButton <| TermIndex.toInt newTermIndex_)
                            |> Cmd.map PageMsg.Internal
                ]
            )

        MoveTermUp fromDragButton termIndex ->
            let
                form : GlossaryItemForm
                form =
                    Form.moveTermUp termIndex model.form
            in
            ( { model
                | dropdownMenusWithMoreOptionsForTerms = dropdownMenusWithMoreOptionsForTermsForForm form
              }
                |> updateForm (always form)
            , moveFocusAfterMovingTermUp fromDragButton termIndex
                |> Cmd.map PageMsg.Internal
            )

        MoveTermDown fromDragButton numberOfTerms termIndex ->
            let
                form : GlossaryItemForm
                form =
                    Form.moveTermDown termIndex model.form
            in
            ( { model
                | dropdownMenusWithMoreOptionsForTerms = dropdownMenusWithMoreOptionsForTermsForForm form
              }
                |> updateForm (always form)
            , moveFocusAfterMovingTermDown fromDragButton numberOfTerms termIndex
                |> Cmd.map PageMsg.Internal
            )

        ToggleAbbreviation termIndex ->
            ( updateForm (Form.toggleAbbreviation termIndex) model, Cmd.none )

        ToggleTagCheckbox tag ->
            ( updateForm (Form.toggleTagCheckbox tag) model, Cmd.none )

        UpdateDefinition body ->
            ( updateForm (Form.updateDefinition body) model, Cmd.none )

        SelectDisambiguationTag rawTag ->
            let
                disambiguationTagId : Maybe TagId
                disambiguationTagId =
                    model.form
                        |> Form.tagCheckboxes
                        |> List.filterMap
                            (\( ( tagId, tag ), _ ) ->
                                if Tag.raw tag == rawTag then
                                    Just tagId

                                else
                                    Nothing
                            )
                        |> List.head
            in
            ( updateForm (Form.updateDisambiguationTagId disambiguationTagId) model
            , Cmd.none
            )

        AddRelatedTerm maybeRawTerm ->
            let
                form : GlossaryItemForm
                form =
                    Form.addRelatedTerm maybeRawTerm model.form

                latestRelatedTermIndex : RelatedTermIndex
                latestRelatedTermIndex =
                    Array.length (Form.relatedTermFields form) - 1 |> RelatedTermIndex.fromInt
            in
            ( { model
                | dropdownMenusWithMoreOptionsForRelatedTerms = dropdownMenusWithMoreOptionsForRelatedTermsForForm form
              }
                |> updateForm (always form)
            , giveFocusToSeeAlsoSelect latestRelatedTermIndex
            )

        SelectRelatedTerm relatedTermIndex selection ->
            let
                relatedRawTerm : Maybe RawTerm
                relatedRawTerm =
                    if selection == "" then
                        Nothing

                    else
                        Just <| RawTerm.fromString selection
            in
            ( updateForm (Form.selectRelatedTerm relatedTermIndex relatedRawTerm) model
            , Cmd.none
            )

        DeleteRelatedTerm relatedTermIndex ->
            let
                form : GlossaryItemForm
                form =
                    Form.deleteRelatedTerm relatedTermIndex model.form
            in
            ( { model
                | dropdownMenusWithMoreOptionsForRelatedTerms = dropdownMenusWithMoreOptionsForRelatedTermsForForm form
              }
                |> updateForm (always form)
            , Task.attempt
                (\_ -> NoOp)
                (Dom.blur <| ElementIds.deleteRelatedTermButton <| RelatedTermIndex.toInt relatedTermIndex)
                |> Cmd.map PageMsg.Internal
            )

        DropdownMenuWithMoreOptionsForTermMsg termIndexInt msg_ ->
            model.dropdownMenusWithMoreOptionsForTerms
                |> Dict.get termIndexInt
                |> Maybe.map
                    (\dropdownMenu ->
                        Components.DropdownMenu.update
                            (\x ->
                                let
                                    dropdownMenusWithMoreOptionsForTerms1 : Dict Int Components.DropdownMenu.Model
                                    dropdownMenusWithMoreOptionsForTerms1 =
                                        Dict.map
                                            (\termIndex_ dropdownMenu_ ->
                                                if termIndex_ == termIndexInt then
                                                    x

                                                else
                                                    Components.DropdownMenu.hidden dropdownMenu_
                                            )
                                            model.dropdownMenusWithMoreOptionsForTerms
                                in
                                { model
                                    | dropdownMenusWithMoreOptionsForTerms =
                                        dropdownMenusWithMoreOptionsForTerms1
                                }
                            )
                            (PageMsg.Internal << DropdownMenuWithMoreOptionsForTermMsg termIndexInt)
                            msg_
                            dropdownMenu
                    )
                |> Maybe.withDefault ( model, Cmd.none )

        DropdownMenuWithMoreOptionsForRelatedTermMsg relatedTermIndexInt msg_ ->
            model.dropdownMenusWithMoreOptionsForRelatedTerms
                |> Dict.get relatedTermIndexInt
                |> Maybe.map
                    (\dropdownMenu ->
                        Components.DropdownMenu.update
                            (\x ->
                                let
                                    dropdownMenusWithMoreOptionsForRelatedTerms1 : Dict Int Components.DropdownMenu.Model
                                    dropdownMenusWithMoreOptionsForRelatedTerms1 =
                                        Dict.map
                                            (\relatedTermIndex_ dropdownMenu_ ->
                                                if relatedTermIndex_ == relatedTermIndexInt then
                                                    x

                                                else
                                                    Components.DropdownMenu.hidden dropdownMenu_
                                            )
                                            model.dropdownMenusWithMoreOptionsForRelatedTerms
                                in
                                { model
                                    | dropdownMenusWithMoreOptionsForRelatedTerms =
                                        dropdownMenusWithMoreOptionsForRelatedTerms1
                                }
                            )
                            (PageMsg.Internal << DropdownMenuWithMoreOptionsForRelatedTermMsg relatedTermIndexInt)
                            msg_
                            dropdownMenu
                    )
                |> Maybe.withDefault ( model, Cmd.none )

        MoveRelatedTermUp fromDragButton relatedTermIndex ->
            let
                form : GlossaryItemForm
                form =
                    Form.moveRelatedTermUp relatedTermIndex model.form
            in
            ( { model
                | dropdownMenusWithMoreOptionsForRelatedTerms = dropdownMenusWithMoreOptionsForRelatedTermsForForm form
              }
                |> updateForm (always form)
            , moveFocusAfterMovingRelatedTermUp fromDragButton relatedTermIndex
                |> Cmd.map PageMsg.Internal
            )

        MoveRelatedTermDown fromDragButton numberOfRelatedTerms relatedTermIndex ->
            let
                form : GlossaryItemForm
                form =
                    Form.moveRelatedTermDown relatedTermIndex model.form
            in
            ( { model
                | dropdownMenusWithMoreOptionsForRelatedTerms = dropdownMenusWithMoreOptionsForRelatedTermsForForm form
              }
                |> updateForm (always form)
            , moveFocusAfterMovingRelatedTermDown fromDragButton numberOfRelatedTerms relatedTermIndex
                |> Cmd.map PageMsg.Internal
            )

        DragAndDropRelatedTermsMsg msg_ ->
            let
                ( dragAndDropRelatedTerms_, result ) =
                    Components.DragAndDrop.update msg_ model.dragAndDropRelatedTerms

                dragId : Maybe RelatedTermIndex
                dragId =
                    Components.DragAndDrop.getDragId dragAndDropRelatedTerms_

                dropId : Maybe RelatedTermIndex
                dropId =
                    Components.DragAndDrop.getDropId dragAndDropRelatedTerms_

                tentativeDragAndDropChangesToShow : TentativeDragAndDropChangesToShow
                tentativeDragAndDropChangesToShow =
                    case ( dragId, dropId ) of
                        ( Just dragId_, Nothing ) ->
                            if model.tentativeDragAndDropChangesToShow == NoDragAndDropInProgress then
                                RelatedTermBeingRelocated dragId_ dropId

                            else
                                model.tentativeDragAndDropChangesToShow

                        ( Just dragId_, Just _ ) ->
                            RelatedTermBeingRelocated dragId_ dropId

                        _ ->
                            NoDragAndDropInProgress
            in
            ( { model
                | tentativeDragAndDropChangesToShow = tentativeDragAndDropChangesToShow
                , dragAndDropRelatedTerms = dragAndDropRelatedTerms_
              }
                |> (case result of
                        Nothing ->
                            identity

                        Just ( oldRelatedTermIndex_, newRelatedTermIndex_, _ ) ->
                            updateForm (updatedFormWithRelatedTermBeingRelocated oldRelatedTermIndex_ newRelatedTermIndex_)
                   )
            , Cmd.batch
                [ Components.DragAndDrop.getDragstartEvent msg_
                    |> Maybe.map (.event >> dragStart)
                    |> Maybe.withDefault Cmd.none
                , case result of
                    Nothing ->
                        Cmd.none

                    Just ( _, newRelatedTermIndex_, _ ) ->
                        Task.attempt
                            (\_ -> NoOp)
                            (Dom.focus <| ElementIds.dragRelatedTermButton <| RelatedTermIndex.toInt newRelatedTermIndex_)
                            |> Cmd.map PageMsg.Internal
                ]
            )

        ToggleNeedsUpdating ->
            ( updateForm Form.toggleNeedsUpdating model
            , Cmd.none
            )

        Save ->
            ( model, getCurrentDateTimeAndNewIdForSaving () )

        ReceiveCurrentDateTimeAndNewIdForSaving ( dateTime, newGlossaryItemIdString ) ->
            case model.common.glossaryForUi of
                Ok glossaryForUi ->
                    if Form.hasValidationErrors model.form then
                        ( { model
                            | triedToSaveWhenFormInvalid = True
                            , saving = NotCurrentlySaving
                          }
                        , Cmd.none
                        )

                    else
                        let
                            newGlossaryItemId : GlossaryItemId
                            newGlossaryItemId =
                                GlossaryItemId.create newGlossaryItemIdString

                            newOrUpdatedGlossaryItem : GlossaryItemForUi
                            newOrUpdatedGlossaryItem =
                                Form.toGlossaryItem
                                    (Glossary.items glossaryForUi)
                                    model.form
                                    (model.itemBeingEdited |> Maybe.withDefault newGlossaryItemId)
                                    (Just dateTime)

                            glossaryChange : GlossaryChange
                            glossaryChange =
                                case model.itemBeingEdited of
                                    Just _ ->
                                        GlossaryChange.Update (GlossaryItemForUi.toGlossaryItemFromDom newOrUpdatedGlossaryItem)

                                    Nothing ->
                                        GlossaryChange.Insert (GlossaryItemForUi.toGlossaryItemFromDom newOrUpdatedGlossaryItem)

                            glossaryChangeWithChecksum : GlossaryChangeWithChecksum
                            glossaryChangeWithChecksum =
                                { glossaryChange = glossaryChange
                                , checksum = Glossary.checksumForChange glossaryForUi glossaryChange
                                }

                            changelist : GlossaryChangelist.GlossaryChangelist
                            changelist =
                                GlossaryChangelist.create
                                    (Glossary.versionNumber glossaryForUi)
                                    [ glossaryChangeWithChecksum ]

                            ( saving, cmd ) =
                                Save.changeAndSave model.common.editability
                                    glossaryForUi
                                    changelist
                                    (PageMsg.Internal << FailedToSave)
                                    (\( itemToGiveFocus, updatedGlossaryForUi ) ->
                                        let
                                            common0 : CommonModel
                                            common0 =
                                                model.common
                                        in
                                        PageMsg.NavigateToListAll
                                            { common0 | glossaryForUi = Ok updatedGlossaryForUi }
                                            itemToGiveFocus
                                    )
                        in
                        ( { model | saving = saving }
                        , cmd
                        )

                _ ->
                    ( model, Cmd.none )

        FailedToSave error ->
            ( { model | saving = SavingFailed <| Extras.Http.httpErrorDescriptionAskingToReloadOnUnauthorisedOrConflict <| error }
            , Cmd.none
            )


moveFocusAfterMovingTermUp : Bool -> TermIndex -> Cmd InternalMsg
moveFocusAfterMovingTermUp fromDragButton termIndex =
    let
        termIndexInt : Int
        termIndexInt =
            TermIndex.toInt termIndex

        previousTermIndexInt : Int
        previousTermIndexInt =
            termIndexInt - 1
    in
    if fromDragButton then
        Task.attempt
            (\_ -> NoOp)
            (Dom.focus <| ElementIds.dragTermButton previousTermIndexInt)

    else if previousTermIndexInt == 0 then
        Task.attempt
            (\_ -> NoOp)
            (Dom.blur <| ElementIds.moveTermUpButton termIndexInt)

    else
        Task.attempt
            (\_ -> NoOp)
            (Dom.focus <| ElementIds.moveTermUpButton previousTermIndexInt)


moveFocusAfterMovingTermDown : Bool -> Int -> TermIndex -> Cmd InternalMsg
moveFocusAfterMovingTermDown fromDragButton numberOfTerms termIndex =
    let
        termIndexInt : Int
        termIndexInt =
            TermIndex.toInt termIndex

        nextTermIndexInt : Int
        nextTermIndexInt =
            termIndexInt + 1
    in
    if fromDragButton then
        Task.attempt
            (\_ -> NoOp)
            (Dom.focus <| ElementIds.dragTermButton nextTermIndexInt)

    else if nextTermIndexInt == numberOfTerms - 1 then
        Task.attempt
            (\_ -> NoOp)
            (Dom.blur <| ElementIds.moveTermDownButton termIndexInt)

    else
        Task.attempt
            (\_ -> NoOp)
            (Dom.focus <| ElementIds.moveTermDownButton nextTermIndexInt)


moveFocusAfterMovingRelatedTermUp : Bool -> RelatedTermIndex -> Cmd InternalMsg
moveFocusAfterMovingRelatedTermUp fromDragButton relatedTermIndex =
    let
        relatedTermIndexInt : Int
        relatedTermIndexInt =
            RelatedTermIndex.toInt relatedTermIndex

        previousRelatedTermIndexInt : Int
        previousRelatedTermIndexInt =
            relatedTermIndexInt - 1
    in
    if fromDragButton then
        Task.attempt
            (\_ -> NoOp)
            (Dom.focus <| ElementIds.dragRelatedTermButton previousRelatedTermIndexInt)

    else if previousRelatedTermIndexInt == 0 then
        Task.attempt
            (\_ -> NoOp)
            (Dom.blur <| ElementIds.moveRelatedTermUpButton relatedTermIndexInt)

    else
        Task.attempt
            (\_ -> NoOp)
            (Dom.focus <| ElementIds.moveRelatedTermUpButton previousRelatedTermIndexInt)


moveFocusAfterMovingRelatedTermDown : Bool -> Int -> RelatedTermIndex -> Cmd InternalMsg
moveFocusAfterMovingRelatedTermDown fromDragButton numberOfRelatedTerms relatedTermIndex =
    let
        relatedTermIndexInt : Int
        relatedTermIndexInt =
            RelatedTermIndex.toInt relatedTermIndex

        nextRelatedTermIndexInt : Int
        nextRelatedTermIndexInt =
            relatedTermIndexInt + 1
    in
    if fromDragButton then
        Task.attempt
            (\_ -> NoOp)
            (Dom.focus <| ElementIds.dragRelatedTermButton nextRelatedTermIndexInt)

    else if nextRelatedTermIndexInt == numberOfRelatedTerms - 1 then
        Task.attempt
            (\_ -> NoOp)
            (Dom.blur <| ElementIds.moveRelatedTermDownButton relatedTermIndexInt)

    else
        Task.attempt
            (\_ -> NoOp)
            (Dom.focus <| ElementIds.moveRelatedTermDownButton nextRelatedTermIndexInt)



-- VIEW


giveFocusToTermInputField : TermIndex -> Cmd Msg
giveFocusToTermInputField termIndex =
    Task.attempt (always <| PageMsg.Internal NoOp) (Dom.focus <| ElementIds.termInputField termIndex)


giveFocusToSeeAlsoSelect : RelatedTermIndex -> Cmd Msg
giveFocusToSeeAlsoSelect index =
    Task.attempt (always <| PageMsg.Internal NoOp) (Dom.focus <| ElementIds.seeAlsoSelect index)


viewCreateTerm : DivDragAndDropStatus -> Bool -> Bool -> Int -> Int -> TermField -> Dict Int Components.DropdownMenu.Model -> Html Msg
viewCreateTerm dragAndDropStatus mathSupportEnabled showValidationErrors numberOfTerms index term dropdownMenusWithMoreOptionsForTerms =
    viewCreateTermInternal
        dragAndDropStatus
        False
        mathSupportEnabled
        showValidationErrors
        numberOfTerms
        (Dict.get index dropdownMenusWithMoreOptionsForTerms)
        (TermIndex.fromInt index)
        term


viewMoveTermUpOrDownButtons : Int -> TermIndex -> Html (PageMsg InternalMsg)
viewMoveTermUpOrDownButtons numberOfTerms termIndex =
    div
        [ class "hidden sm:flex sm:mr-2 items-center" ]
        [ Components.Button.rounded (TermIndex.toInt termIndex > 0)
            [ Accessibility.Aria.label I18n.moveUp
            , id <| ElementIds.moveTermUpButton <| TermIndex.toInt termIndex
            , Html.Events.onClick <| PageMsg.Internal <| MoveTermUp False termIndex
            ]
            [ Icons.arrowUp
                [ Svg.Attributes.class "h-5 w-5" ]
            ]
        , Components.Button.rounded (TermIndex.toInt termIndex + 1 < numberOfTerms)
            [ Accessibility.Aria.label I18n.moveDown
            , id <| ElementIds.moveTermDownButton <| TermIndex.toInt termIndex
            , Html.Events.onClick <| PageMsg.Internal <| MoveTermDown False numberOfTerms termIndex
            ]
            [ Icons.arrowDown
                [ Svg.Attributes.class "h-5 w-5" ]
            ]
        ]


type DivDragAndDropStatus
    = CannotBeDraggedAndDropped
    | CanBeDraggedAndDropped
    | BeingDragged


viewCreateTermInternal : DivDragAndDropStatus -> Bool -> Bool -> Bool -> Int -> Maybe Components.DropdownMenu.Model -> TermIndex -> TermField -> Html Msg
viewCreateTermInternal dragAndDropStatus showMarkdownBasedSyntaxEnabled mathSupportEnabled showValidationErrors numberOfTerms maybeDropdownMenuWithMoreOptions termIndex termField =
    let
        abbreviationLabelId : String
        abbreviationLabelId =
            ElementIds.abbreviationLabel termIndex
    in
    Html.div
        (if dragAndDropStatus /= CannotBeDraggedAndDropped then
            Components.DragAndDrop.draggable (PageMsg.Internal << DragAndDropTermsMsg) termIndex
                ++ Components.DragAndDrop.droppable (PageMsg.Internal << DragAndDropTermsMsg) termIndex
                ++ [ class "hidden lg:block"
                   , Extras.HtmlAttribute.showIf (dragAndDropStatus == BeingDragged) <|
                        class "opacity-25"
                   ]

         else
            [ class "lg:hidden" ]
        )
        [ div
            [ class "flex flex-row items-center flex-auto max-w-2xl" ]
            [ div
                [ class "flex items-center w-full" ]
                [ Extras.Html.showIf (dragAndDropStatus /= CannotBeDraggedAndDropped) <|
                    Components.Button.roundedWithoutBorder True
                        [ Extras.HtmlAttribute.showIf showMarkdownBasedSyntaxEnabled <| class "sm:mt-6"
                        , class "cursor-grab mr-2"
                        , id <| ElementIds.dragTermButton (TermIndex.toInt termIndex)
                        , Html.Events.preventDefaultOn "keydown"
                            (Extras.HtmlEvents.preventDefaultOnDecoder
                                (\event ->
                                    if Extras.HtmlEvents.isUpArrow event then
                                        Just <| ( PageMsg.Internal <| MoveTermUp True termIndex, True )

                                    else if Extras.HtmlEvents.isDownArrow event then
                                        Just <| ( PageMsg.Internal <| MoveTermDown True numberOfTerms termIndex, True )

                                    else
                                        Nothing
                                )
                            )
                        ]
                        [ span
                            [ class "sr-only" ]
                            [ text I18n.dragOrUseUpAndDownArrowsToMoveTerm
                            ]
                        , Icons.gripVertical
                            [ Svg.Attributes.class "h-6 w-6 text-gray-500 dark:text-gray-400" ]
                        ]
                , Extras.Html.showIf (dragAndDropStatus == CannotBeDraggedAndDropped) <| viewMoveTermUpOrDownButtons numberOfTerms termIndex
                , Extras.Html.showIf (numberOfTerms > 1) <|
                    Extras.Html.showMaybe
                        (\dropdownMenuWithMoreOptions ->
                            span
                                [ class "sm:hidden mr-2 flex items-center" ]
                                [ viewMoreOptionsForTermDropdownButton numberOfTerms termIndex dropdownMenuWithMoreOptions ]
                        )
                        maybeDropdownMenuWithMoreOptions
                , Html.div
                    [ class "flex flex-wrap sm:flex-nowrap w-full min-w-0" ]
                    [ Components.Form.inputText
                        (TermField.raw termField)
                        showMarkdownBasedSyntaxEnabled
                        mathSupportEnabled
                        showValidationErrors
                        (TermField.validationError termField)
                        [ id <|
                            (if dragAndDropStatus == CannotBeDraggedAndDropped then
                                ElementIds.termInputField

                             else
                                ElementIds.draggableTermInputField
                            )
                                termIndex
                        , required True
                        , Html.Attributes.autocomplete False
                        , Html.Attributes.placeholder <|
                            if TermIndex.toInt termIndex == 0 then
                                I18n.preferredTerm

                            else
                                I18n.alternativeTerm
                        , Accessibility.Aria.label I18n.term
                        , Accessibility.Aria.required True
                        , Html.Events.onInput (PageMsg.Internal << UpdateTerm termIndex)
                        , Extras.HtmlEvents.onEnter <| PageMsg.Internal NoOp
                        ]
                    , div
                        [ class "sm:flex flex-auto mt-2 relative items-baseline"
                        , Extras.HtmlAttribute.showIf showMarkdownBasedSyntaxEnabled <| class "sm:pt-6"
                        ]
                        [ div
                            [ class "mt-1 sm:mt-0 sm:ml-5" ]
                            [ Components.Button.toggle
                                (TermField.isAbbreviation termField)
                                abbreviationLabelId
                                [ Html.Events.onClick <| PageMsg.Internal <| ToggleAbbreviation termIndex ]
                                [ span
                                    [ class "font-medium text-gray-900 dark:text-gray-300" ]
                                    [ text I18n.abbreviation ]
                                ]
                            ]
                        ]
                    ]
                ]
            , span
                [ class "inline-flex items-center"
                , Extras.HtmlAttribute.showIf showMarkdownBasedSyntaxEnabled <| class "pt-6"
                ]
                [ Components.Button.rounded (numberOfTerms > 1)
                    [ Accessibility.Aria.label I18n.delete
                    , id <| ElementIds.deleteTermButton <| TermIndex.toInt termIndex
                    , Html.Events.onClick <| PageMsg.Internal <| DeleteTerm termIndex
                    , class "ml-4"
                    ]
                    [ Icons.trash
                        [ Svg.Attributes.class "h-5 w-5" ]
                    ]
                ]
            ]
        , Extras.Html.showMaybe
            (\validationError ->
                p
                    [ class "mt-2 text-red-600 dark:text-red-400" ]
                    [ text validationError ]
            )
            (if showValidationErrors then
                TermField.validationError termField

             else
                Nothing
            )
        ]


viewMoreOptionsForTermDropdownButton : Int -> TermIndex -> Components.DropdownMenu.Model -> Html Msg
viewMoreOptionsForTermDropdownButton numberOfTerms index dropdownMenuWithMoreOptionsForTerm =
    let
        indexInt : Int
        indexInt =
            TermIndex.toInt index
    in
    Components.DropdownMenu.view
        (PageMsg.Internal << DropdownMenuWithMoreOptionsForTermMsg indexInt)
        dropdownMenuWithMoreOptionsForTerm
        True
        Components.DropdownMenu.Ellipsis
        (List.filterMap identity
            [ if indexInt > 0 then
                Just <|
                    Components.DropdownMenu.choice
                        [ span
                            [ class "inline-flex items-center" ]
                            [ Icons.arrowUp
                                [ Svg.Attributes.class "h-5 w-5 text-gray-500 dark:text-gray-400 mr-2" ]
                            , text I18n.moveUp
                            ]
                        ]
                        (PageMsg.Internal <| MoveTermUp False index)

              else
                Nothing
            , if indexInt + 1 < numberOfTerms then
                Just <|
                    Components.DropdownMenu.choice
                        [ span
                            [ class "inline-flex items-center" ]
                            [ Icons.arrowDown
                                [ Svg.Attributes.class "h-5 w-5 text-gray-500 dark:text-gray-400 mr-2" ]
                            , text I18n.moveDown
                            ]
                        ]
                        (PageMsg.Internal <| MoveTermDown False numberOfTerms index)

              else
                Nothing
            ]
        )


viewCreateTerms : Bool -> Bool -> Maybe Int -> Array TermField -> Dict Int Components.DropdownMenu.Model -> Html Msg
viewCreateTerms mathSupportEnabled showValidationErrors idOfTermBeingDragged termsArray dropdownMenusWithMoreOptionsForTerms =
    let
        terms : List TermField
        terms =
            Array.toList termsArray
    in
    div []
        [ div []
            [ h2
                [ class "text-lg leading-6 font-medium text-gray-900 dark:text-gray-100" ]
                [ text I18n.terms ]
            , p
                [ class "mt-1 max-w-2xl text-sm text-gray-600 dark:text-gray-400" ]
                I18n.listTheGroupOfTermsBeingDefined
            ]
        , div
            [ class "mt-6 sm:mt-5" ]
            [ div
                [ class "mb-1" ]
                [ Components.Form.markdownSupportedMessage mathSupportEnabled ]
            , div
                [ class "space-y-6 sm:space-y-5"
                , Html.Attributes.attribute "data-contains-draggable-elements" "true"
                ]
                ((List.indexedMap
                    (\index termField ->
                        [ viewCreateTerm
                            (if Just index == idOfTermBeingDragged then
                                BeingDragged

                             else
                                CanBeDraggedAndDropped
                            )
                            mathSupportEnabled
                            showValidationErrors
                            (List.length terms)
                            index
                            termField
                            dropdownMenusWithMoreOptionsForTerms
                        , viewCreateTerm
                            CannotBeDraggedAndDropped
                            mathSupportEnabled
                            showValidationErrors
                            (List.length terms)
                            index
                            termField
                            dropdownMenusWithMoreOptionsForTerms
                        ]
                    )
                    terms
                    |> List.concat
                 )
                    ++ [ div []
                            [ Components.Button.secondary
                                [ Html.Events.onClick <| PageMsg.Internal AddTerm ]
                                [ Icons.plus [ Svg.Attributes.class "mx-auto -ml-1 mr-2 h-5 w-5" ]
                                , text I18n.addTermButton
                                ]
                            ]
                       ]
                )
            ]
        ]


viewDefinitionSingle : Bool -> Bool -> DefinitionField -> Html Msg
viewDefinitionSingle mathSupportEnabled showValidationErrors definitionSingle =
    viewDefinitionSingle1 mathSupportEnabled showValidationErrors definitionSingle


viewDefinitionSingle1 : Bool -> Bool -> DefinitionField -> Html Msg
viewDefinitionSingle1 mathSupportEnabled showValidationErrors definitionSingle =
    let
        raw : String
        raw =
            DefinitionField.raw definitionSingle

        validationError : Maybe String
        validationError =
            DefinitionField.validationError definitionSingle
    in
    div []
        [ div
            [ class "flex-auto max-w-3xl flex" ]
            [ div
                [ class "relative block min-w-0 w-full" ]
                [ Components.Form.textarea
                    raw
                    True
                    mathSupportEnabled
                    showValidationErrors
                    validationError
                    [ required True
                    , Accessibility.Aria.label I18n.definition
                    , Accessibility.Aria.required True
                    , id ElementIds.definition
                    , Html.Events.onInput (PageMsg.Internal << UpdateDefinition)
                    ]
                ]
            ]
        , Extras.Html.showMaybe
            (\validationError0 ->
                p
                    [ class "mt-2 text-red-600 dark:text-red-400" ]
                    [ text validationError0 ]
            )
            (if showValidationErrors then
                validationError

             else
                Nothing
            )
        ]


viewDefinition : Bool -> Bool -> DefinitionField -> Html Msg
viewDefinition mathSupportEnabled showValidationErrors definitionField =
    div
        [ class "pt-8 space-y-6 sm:pt-10 sm:space-y-5" ]
        [ div []
            [ h2
                [ class "text-lg leading-6 font-medium text-gray-900 dark:text-gray-100" ]
                [ text I18n.definition ]
            , p
                [ class "mt-1 max-w-2xl text-sm text-gray-600 dark:text-gray-400" ]
                [ text I18n.provideADefinitionForThisGroupOfTerms ]
            ]
        , div
            [ class "space-y-6 sm:space-y-5" ]
            [ viewDefinitionSingle mathSupportEnabled showValidationErrors definitionField ]
        ]


viewTags : Bool -> List ( ( TagId, Tag ), Bool ) -> Html Msg
viewTags enableMathSupport tagCheckboxes =
    div
        [ class "pt-8 space-y-6 sm:pt-10 sm:space-y-5" ]
        [ div []
            [ h2
                [ class "text-lg leading-6 font-medium text-gray-900 dark:text-gray-100" ]
                [ text I18n.tags ]
            , p
                [ class "mt-1 max-w-2xl text-sm text-gray-600 dark:text-gray-400" ]
                [ text I18n.selectAllTagsThatApplyToThisItem ]
            ]
        , div
            []
            (tagCheckboxes
                |> List.map
                    (\( ( tagId, tag ), checked ) ->
                        Components.Badge.indigoWithCheckbox
                            { tabbable = True, checked = checked }
                            (tagId |> TagId.toString |> (++) "tag-")
                            (PageMsg.Internal <| ToggleTagCheckbox tag)
                            [ class "mr-2 mb-2" ]
                            [ Tag.view enableMathSupport [] tag ]
                    )
            )
        ]


viewDisambiguationTag : Maybe TagId -> List ( TagId, Tag ) -> Html Msg
viewDisambiguationTag disambiguationTagId tags =
    div
        [ class "pt-8 space-y-6 sm:pt-10 sm:space-y-5" ]
        [ div []
            [ h2
                [ class "text-lg leading-6 font-medium text-gray-900 dark:text-gray-100" ]
                [ text I18n.disambiguationTagOptional ]
            , p
                [ class "mt-1 max-w-2xl text-sm text-gray-500 dark:text-gray-400" ]
                [ text I18n.chooseWhichTagShouldBeUsedToDistinguishThisItem ]
            ]
        , div
            [ class "max-w-md" ]
            [ Components.SelectMenu.view
                [ Components.SelectMenu.id <| ElementIds.disambiguationTagSelect
                , Components.SelectMenu.ariaLabel I18n.disambiguationTag
                , Components.SelectMenu.onChange (PageMsg.Internal << SelectDisambiguationTag)
                ]
                (Components.SelectMenu.Choice
                    ""
                    [ text I18n.none ]
                    (disambiguationTagId == Nothing)
                    :: (tags
                            |> List.map
                                (\( tagId, tag ) ->
                                    Components.SelectMenu.Choice
                                        (Tag.raw tag)
                                        [ text <| Tag.inlineText tag ]
                                        (disambiguationTagId == Just tagId)
                                )
                       )
                )
            ]
        ]


viewCreateSeeAlsoSingle :
    DivDragAndDropStatus
    -> Bool
    -> Set String
    -> Int
    -> List DisambiguatedTerm
    -> Dict Int Components.DropdownMenu.Model
    -> Int
    -> Form.RelatedTermField
    -> Html Msg
viewCreateSeeAlsoSingle dragAndDropStatus showValidationErrors relatedRawTerms numberOfRelatedTerms allTerms dropdownMenusWithMoreOptionsForRelatedTerms index relatedTerm =
    viewCreateSeeAlsoSingle1
        dragAndDropStatus
        showValidationErrors
        relatedRawTerms
        numberOfRelatedTerms
        allTerms
        (Dict.get index dropdownMenusWithMoreOptionsForRelatedTerms)
        (RelatedTermIndex.fromInt index)
        relatedTerm


viewMoveRelatedTermUpOrDownButtons : Int -> RelatedTermIndex -> Html (PageMsg InternalMsg)
viewMoveRelatedTermUpOrDownButtons numberOfRelatedTerms relatedTermIndex =
    div
        [ class "hidden sm:flex sm:mr-2 items-center" ]
        [ Components.Button.rounded (RelatedTermIndex.toInt relatedTermIndex > 0)
            [ Accessibility.Aria.label I18n.moveUp
            , id <| ElementIds.moveRelatedTermUpButton <| RelatedTermIndex.toInt relatedTermIndex
            , Html.Events.onClick <| PageMsg.Internal <| MoveRelatedTermUp False relatedTermIndex
            ]
            [ Icons.arrowUp
                [ Svg.Attributes.class "h-5 w-5" ]
            ]
        , Components.Button.rounded (RelatedTermIndex.toInt relatedTermIndex + 1 < numberOfRelatedTerms)
            [ Accessibility.Aria.label I18n.moveDown
            , id <| ElementIds.moveRelatedTermDownButton <| RelatedTermIndex.toInt relatedTermIndex
            , Html.Events.onClick <| PageMsg.Internal <| MoveRelatedTermDown False numberOfRelatedTerms relatedTermIndex
            ]
            [ Icons.arrowDown
                [ Svg.Attributes.class "h-5 w-5" ]
            ]
        ]


viewCreateSeeAlsoSingle1 :
    DivDragAndDropStatus
    -> Bool
    -> Set String
    -> Int
    -> List DisambiguatedTerm
    -> Maybe Components.DropdownMenu.Model
    -> RelatedTermIndex
    -> Form.RelatedTermField
    -> Html Msg
viewCreateSeeAlsoSingle1 dragAndDropStatus showValidationErrors relatedRawTerms numberOfRelatedTerms allTerms maybeDropdownMenuWithMoreOptions relatedTermIndex relatedTerm =
    Html.div
        (if dragAndDropStatus /= CannotBeDraggedAndDropped then
            Components.DragAndDrop.draggable (PageMsg.Internal << DragAndDropRelatedTermsMsg) relatedTermIndex
                ++ Components.DragAndDrop.droppable (PageMsg.Internal << DragAndDropRelatedTermsMsg) relatedTermIndex
                ++ [ class "hidden lg:block"
                   , Extras.HtmlAttribute.showIf (dragAndDropStatus == BeingDragged) <|
                        class "opacity-25"
                   ]

         else
            [ class "lg:hidden" ]
        )
        [ div
            [ class "flex-auto max-w-2xl flex items-center" ]
            [ Extras.Html.showIf (dragAndDropStatus /= CannotBeDraggedAndDropped) <|
                Components.Button.roundedWithoutBorder True
                    [ class "cursor-grab mr-2"
                    , id <| ElementIds.dragRelatedTermButton (RelatedTermIndex.toInt relatedTermIndex)
                    , Html.Events.preventDefaultOn "keydown"
                        (Extras.HtmlEvents.preventDefaultOnDecoder
                            (\event ->
                                if Extras.HtmlEvents.isUpArrow event then
                                    Just <| ( PageMsg.Internal <| MoveRelatedTermUp True relatedTermIndex, True )

                                else if Extras.HtmlEvents.isDownArrow event then
                                    Just <| ( PageMsg.Internal <| MoveRelatedTermDown True numberOfRelatedTerms relatedTermIndex, True )

                                else
                                    Nothing
                            )
                        )
                    ]
                    [ span
                        [ class "sr-only" ]
                        [ text I18n.dragOrUseUpAndDownArrowsToMoveTerm
                        ]
                    , Icons.gripVertical
                        [ Svg.Attributes.class "h-6 w-6 text-gray-500 dark:text-gray-400" ]
                    ]
            , Extras.Html.showIf (dragAndDropStatus == CannotBeDraggedAndDropped) <| viewMoveRelatedTermUpOrDownButtons numberOfRelatedTerms relatedTermIndex
            , Extras.Html.showIf (numberOfRelatedTerms > 1) <|
                Extras.Html.showMaybe
                    (\dropdownMenuWithMoreOptions ->
                        span
                            [ class "sm:hidden mr-2 flex items-center" ]
                            [ viewMoreOptionsForRelatedTermDropdownButton numberOfRelatedTerms relatedTermIndex dropdownMenuWithMoreOptions ]
                    )
                    maybeDropdownMenuWithMoreOptions
            , Components.SelectMenu.view
                [ Components.SelectMenu.id <|
                    (if dragAndDropStatus == CannotBeDraggedAndDropped then
                        ElementIds.seeAlsoSelect

                     else
                        ElementIds.draggableSeeAlsoSelect
                    )
                        relatedTermIndex
                , Components.SelectMenu.ariaLabel I18n.relatedItem
                , Components.SelectMenu.validationError relatedTerm.validationError
                , Components.SelectMenu.showValidationErrors showValidationErrors
                , Components.SelectMenu.onChange (PageMsg.Internal << SelectRelatedTerm relatedTermIndex)
                ]
                (allTerms
                    |> List.filter
                        (\term ->
                            (not <| Set.member (term |> DisambiguatedTerm.toTerm |> Term.raw |> RawTerm.toString) relatedRawTerms)
                                || (Just (term |> DisambiguatedTerm.toTerm |> Term.raw) == relatedTerm.raw)
                        )
                    |> List.map
                        (\term ->
                            Components.SelectMenu.Choice
                                (term |> DisambiguatedTerm.toTerm |> Term.raw |> RawTerm.toString)
                                [ text <| Term.inlineText <| DisambiguatedTerm.toTerm term ]
                                (Just (Term.raw <| DisambiguatedTerm.toTerm term) == relatedTerm.raw)
                        )
                )
            , span
                [ class "inline-flex items-center" ]
                [ Components.Button.rounded True
                    [ Accessibility.Aria.label I18n.delete
                    , id <| ElementIds.deleteRelatedTermButton <| RelatedTermIndex.toInt relatedTermIndex
                    , Html.Events.onClick <| PageMsg.Internal <| DeleteRelatedTerm relatedTermIndex
                    , class "ml-2"
                    ]
                    [ Icons.trash
                        [ Svg.Attributes.class "h-5 w-5" ]
                    ]
                ]
            ]
        ]


viewMoreOptionsForRelatedTermDropdownButton : Int -> RelatedTermIndex -> Components.DropdownMenu.Model -> Html Msg
viewMoreOptionsForRelatedTermDropdownButton numberOfRelatedTerms index dropdownMenuWithMoreOptionsForRelatedTerm =
    let
        indexInt : Int
        indexInt =
            RelatedTermIndex.toInt index
    in
    Components.DropdownMenu.view
        (PageMsg.Internal << DropdownMenuWithMoreOptionsForRelatedTermMsg indexInt)
        dropdownMenuWithMoreOptionsForRelatedTerm
        True
        Components.DropdownMenu.Ellipsis
        (List.filterMap identity
            [ if indexInt > 0 then
                Just <|
                    Components.DropdownMenu.choice
                        [ span
                            [ class "inline-flex items-center" ]
                            [ Icons.arrowUp
                                [ Svg.Attributes.class "h-5 w-5 text-gray-500 dark:text-gray-400 mr-2" ]
                            , text I18n.moveUp
                            ]
                        ]
                        (PageMsg.Internal <| MoveRelatedTermUp False index)

              else
                Nothing
            , if indexInt + 1 < numberOfRelatedTerms then
                Just <|
                    Components.DropdownMenu.choice
                        [ span
                            [ class "inline-flex items-center" ]
                            [ Icons.arrowDown
                                [ Svg.Attributes.class "h-5 w-5 text-gray-500 dark:text-gray-400 mr-2" ]
                            , text I18n.moveDown
                            ]
                        ]
                        (PageMsg.Internal <| MoveRelatedTermDown False numberOfRelatedTerms index)

              else
                Nothing
            ]
        )


viewAddRelatedTermButton : Html Msg
viewAddRelatedTermButton =
    Components.Button.secondary
        [ Html.Events.onClick <| PageMsg.Internal <| AddRelatedTerm Nothing ]
        [ Icons.plus [ Svg.Attributes.class "mx-auto -ml-1 mr-2 h-5 w-5" ]
        , text I18n.addRelatedItem
        ]


viewAddRelatedTermButtonForEmptyState : Html Msg
viewAddRelatedTermButtonForEmptyState =
    Components.Button.emptyState
        [ Html.Events.onClick <| PageMsg.Internal <| AddRelatedTerm Nothing ]
        [ Icons.plus [ Svg.Attributes.class "mx-auto h-12 w-12 text-gray-400" ]
        , span
            [ class "mt-2 block font-medium text-gray-900 dark:text-gray-200" ]
            [ text I18n.addRelatedItem ]
        ]


viewCreateSeeAlso :
    Bool
    -> Bool
    -> Maybe Int
    -> GlossaryItemsForUi
    -> Array TermField
    -> Array Form.RelatedTermField
    -> Dict Int Components.DropdownMenu.Model
    -> List DisambiguatedTerm
    -> Html Msg
viewCreateSeeAlso enableMathSupport showValidationErrors idOfRelatedTermBeingDragged glossaryItemsForUi terms relatedTermsArray dropdownMenusWithMoreOptionsForRelatedTerms suggestedRelatedTerms =
    let
        rawTermsSet : Set String
        rawTermsSet =
            terms |> Array.toList |> List.map TermField.raw |> Set.fromList

        relatedTermsList : List Form.RelatedTermField
        relatedTermsList =
            Array.toList relatedTermsArray

        allPreferredTerms : List DisambiguatedTerm
        allPreferredTerms =
            glossaryItemsForUi
                |> GlossaryItemsForUi.orderedAlphabetically Nothing
                |> List.map (Tuple.second >> GlossaryItemForUi.disambiguatedPreferredTerm)
    in
    div
        [ class "pt-8 space-y-6 sm:pt-10 sm:space-y-5" ]
        [ div []
            [ h2
                [ class "text-lg leading-6 font-medium text-gray-900 dark:text-gray-100" ]
                [ text I18n.relatedItems ]
            , p
                [ class "mt-1 max-w-2xl text-sm text-gray-600 dark:text-gray-400" ]
                [ text I18n.pointToAnyRelatedItems ]
            ]
        , div
            [ class "mt-6 sm:mt-5 space-y-6 sm:space-y-5"
            , Html.Attributes.attribute "data-contains-draggable-elements" "true"
            ]
            (List.indexedMap
                (\index relatedTerm ->
                    [ viewCreateSeeAlsoSingle
                        (if Just index == idOfRelatedTermBeingDragged then
                            BeingDragged

                         else
                            CanBeDraggedAndDropped
                        )
                        showValidationErrors
                        (relatedTermsList
                            |> List.filterMap (.raw >> Maybe.map RawTerm.toString)
                            |> Set.fromList
                        )
                        (List.length relatedTermsList)
                        (List.filter
                            (\term ->
                                not <|
                                    Set.member
                                        (term
                                            |> DisambiguatedTerm.toTerm
                                            |> Term.raw
                                            |> RawTerm.toString
                                        )
                                        rawTermsSet
                            )
                            allPreferredTerms
                        )
                        dropdownMenusWithMoreOptionsForRelatedTerms
                        index
                        relatedTerm
                    , viewCreateSeeAlsoSingle
                        CannotBeDraggedAndDropped
                        showValidationErrors
                        (relatedTermsList
                            |> List.filterMap (.raw >> Maybe.map RawTerm.toString)
                            |> Set.fromList
                        )
                        (List.length relatedTermsList)
                        (List.filter
                            (\term ->
                                not <|
                                    Set.member
                                        (term
                                            |> DisambiguatedTerm.toTerm
                                            |> Term.raw
                                            |> RawTerm.toString
                                        )
                                        rawTermsSet
                            )
                            allPreferredTerms
                        )
                        dropdownMenusWithMoreOptionsForRelatedTerms
                        index
                        relatedTerm
                    ]
                )
                relatedTermsList
                |> List.concat
            )
        , div
            []
            [ if List.isEmpty relatedTermsList then
                viewAddRelatedTermButtonForEmptyState

              else
                viewAddRelatedTermButton
            ]
        , viewAddSuggestedSeeAlso enableMathSupport suggestedRelatedTerms |> Extras.Html.showIf (not <| List.isEmpty suggestedRelatedTerms)
        ]


viewAddSuggestedSeeAlso : Bool -> List DisambiguatedTerm -> Html Msg
viewAddSuggestedSeeAlso enableMathSupport suggestedRelatedTerms =
    div
        []
        [ p
            [ class "mb-2 max-w-2xl text-sm text-gray-500 dark:text-gray-400" ]
            [ text I18n.suggestions ]
        , div
            [ class "flow-root" ]
            [ div
                [ class "-m-1 flex flex-wrap" ]
                (suggestedRelatedTerms
                    |> List.map
                        (\suggestedRelatedTerm ->
                            Components.Button.white True
                                [ class "m-1 text-sm"
                                , Html.Events.onClick <|
                                    PageMsg.Internal
                                        (AddRelatedTerm <|
                                            Just <|
                                                Term.raw <|
                                                    DisambiguatedTerm.toTerm suggestedRelatedTerm
                                        )
                                ]
                                [ Icons.plus
                                    [ Svg.Attributes.class "shrink-0 -ml-1 mr-2 h-4 w-4" ]
                                , Term.view enableMathSupport [ class "truncate" ] (DisambiguatedTerm.toTerm suggestedRelatedTerm)
                                ]
                        )
                )
            ]
        ]


viewMiscellaneous : Bool -> Html Msg
viewMiscellaneous on =
    div
        [ class "pt-8 space-y-6 sm:pt-10 sm:space-y-5" ]
        [ div []
            [ h2
                [ class "text-lg leading-6 font-medium text-gray-900 dark:text-gray-100" ]
                [ text I18n.miscellaneous ]
            ]
        , Components.Button.toggle
            on
            ElementIds.needsUpdatingToggleLabel
            [ Html.Events.onClick <| PageMsg.Internal <| ToggleNeedsUpdating ]
            [ span
                [ class "font-medium text-gray-900 dark:text-gray-300" ]
                [ text I18n.needsUpdating ]
            ]
        ]


viewCreateFormFooter : Model -> Html Msg
viewCreateFormFooter model =
    let
        saving : Saving
        saving =
            model.saving

        errorDiv : String -> Html msg
        errorDiv message =
            div
                [ class "flex justify-end mb-4" ]
                [ p
                    [ class "text-red-600 dark:text-red-400" ]
                    [ text message ]
                ]

        common : CommonModel
        common =
            model.common
    in
    div
        [ class "pt-5 lg:border-t dark:border-gray-700 flex flex-col items-center" ]
        [ errorDiv I18n.thereAreErrorsOnThisFormSeeAbove
            |> Extras.Html.showIf (model.triedToSaveWhenFormInvalid && Form.hasValidationErrors model.form)
        , Extras.Html.showMaybe
            (\glossaryItemsError ->
                errorDiv <| I18n.unableToSaveAsItWouldResultInTheFollowing ++ ": " ++ glossaryItemsError ++ "."
            )
            (case model.saving of
                SavingNotAttempted error ->
                    Just error

                _ ->
                    Nothing
            )
        , Extras.Html.showIf (common.editability == Editability.EditingInMemory) <|
            div
                [ class "mt-2 mb-2 text-sm text-gray-600 dark:text-gray-400 sm:text-right" ]
                [ text I18n.savingChangesInMemoryMessage ]
        , div
            [ class "flex items-center mb-2" ]
            [ Components.Button.white
                (saving /= SavingInProgress)
                [ Html.Events.onClick <|
                    PageMsg.NavigateToListAll common model.itemBeingEdited
                ]
                [ text I18n.cancel ]
            , Components.Button.primary
                (saving /= SavingInProgress && not (model.triedToSaveWhenFormInvalid && Form.hasValidationErrors model.form))
                [ class "ml-3"
                , Html.Events.onClick <| PageMsg.Internal Save
                ]
                [ text I18n.save ]
            , Components.Spinner.view
                [ Svg.Attributes.class "ml-3 w-8 h-8" ]
                (saving == SavingInProgress)
            ]
        , case saving of
            SavingFailed errorMessage ->
                errorDiv <| I18n.failedToSave ++ " — " ++ errorMessage ++ "."

            _ ->
                Extras.Html.nothing
        ]


updatedFormWithTermBeingRelocated : TermIndex -> TermIndex -> GlossaryItemForm -> GlossaryItemForm
updatedFormWithTermBeingRelocated sourceTermIndex destinationTermIndex =
    let
        sourceTermIndexInt : Int
        sourceTermIndexInt =
            TermIndex.toInt sourceTermIndex

        destinationTermIndexInt : Int
        destinationTermIndexInt =
            TermIndex.toInt destinationTermIndex

        ( sourceTermIndex_, destinationTermIndex_ ) =
            if sourceTermIndexInt < destinationTermIndexInt then
                ( sourceTermIndex, destinationTermIndexInt + 1 |> TermIndex.fromInt )

            else
                ( sourceTermIndex, destinationTermIndex )
    in
    Form.relocateTerm sourceTermIndex_ destinationTermIndex_


updatedFormWithRelatedTermBeingRelocated : RelatedTermIndex -> RelatedTermIndex -> GlossaryItemForm -> GlossaryItemForm
updatedFormWithRelatedTermBeingRelocated sourceRelatedTermIndex destinationRelatedTermIndex =
    let
        sourceRelatedTermIndexInt : Int
        sourceRelatedTermIndexInt =
            RelatedTermIndex.toInt sourceRelatedTermIndex

        destinationRelatedTermIndexInt : Int
        destinationRelatedTermIndexInt =
            RelatedTermIndex.toInt destinationRelatedTermIndex

        ( sourceRelatedTermIndex_, destinationRelatedTermIndex_ ) =
            if sourceRelatedTermIndexInt < destinationRelatedTermIndexInt then
                ( sourceRelatedTermIndex, destinationRelatedTermIndexInt + 1 |> RelatedTermIndex.fromInt )

            else
                ( sourceRelatedTermIndex, destinationRelatedTermIndex )
    in
    Form.relocateRelatedTerm sourceRelatedTermIndex_ destinationRelatedTermIndex_


view : Model -> Document Msg
view model =
    case model.common.glossaryForUi of
        Ok glossaryForUi ->
            let
                terms : Array TermField
                terms =
                    Form.termFields
                        (case model.tentativeDragAndDropChangesToShow of
                            TermBeingRelocated sourceIndex (Just destinationIndex) ->
                                updatedFormWithTermBeingRelocated sourceIndex destinationIndex model.form

                            _ ->
                                model.form
                        )

                definitionArray : DefinitionField
                definitionArray =
                    Form.definitionField model.form

                disambiguationTagId : Maybe TagId
                disambiguationTagId =
                    Form.disambiguationTagId model.form

                relatedTerms : Array Form.RelatedTermField
                relatedTerms =
                    Form.relatedTermFields
                        (case model.tentativeDragAndDropChangesToShow of
                            RelatedTermBeingRelocated sourceIndex (Just destinationIndex) ->
                                updatedFormWithRelatedTermBeingRelocated sourceIndex destinationIndex model.form

                            _ ->
                                model.form
                        )

                suggestedRelatedTerms : List DisambiguatedTerm
                suggestedRelatedTerms =
                    Form.suggestRelatedTerms model.form

                items : GlossaryItemsForUi
                items =
                    Glossary.items glossaryForUi

                newOrUpdatedGlossaryItem : GlossaryItemForUi
                newOrUpdatedGlossaryItem =
                    Form.toGlossaryItem items model.form (GlossaryItemId.create "") Nothing

                idOfTermBeingDragged : Maybe Int
                idOfTermBeingDragged =
                    case
                        model.tentativeDragAndDropChangesToShow
                    of
                        TermBeingRelocated sourceIndex Nothing ->
                            Just <| TermIndex.toInt sourceIndex

                        TermBeingRelocated _ (Just destinationIndex) ->
                            Just <| TermIndex.toInt destinationIndex

                        _ ->
                            Nothing

                idOfRelatedTermBeingDragged : Maybe Int
                idOfRelatedTermBeingDragged =
                    case
                        model.tentativeDragAndDropChangesToShow
                    of
                        RelatedTermBeingRelocated sourceIndex Nothing ->
                            Just <| RelatedTermIndex.toInt sourceIndex

                        RelatedTermBeingRelocated _ (Just destinationIndex) ->
                            Just <| RelatedTermIndex.toInt destinationIndex

                        _ ->
                            Nothing
            in
            { title = glossaryForUi |> Glossary.title |> GlossaryTitle.inlineText
            , body =
                [ div
                    [ class "container mx-auto px-6 pb-16 lg:px-8 max-w-4xl lg:max-w-(--breakpoint-2xl)" ]
                    [ Html.main_
                        []
                        [ h1
                            [ class "text-3xl font-bold leading-tight text-gray-900 dark:text-gray-100 print:text-black pt-6" ]
                            [ text <|
                                if model.itemBeingEdited == Nothing then
                                    I18n.createANewGlossaryItemCapitalised

                                else
                                    I18n.editGlossaryItemCapitalised
                            ]
                        , form
                            [ class "pt-7" ]
                            [ div
                                [ class "lg:flex lg:space-x-8" ]
                                [ div
                                    [ class "lg:w-1/2" ]
                                    [ viewCreateTerms
                                        model.common.enableMathSupport
                                        model.triedToSaveWhenFormInvalid
                                        idOfTermBeingDragged
                                        terms
                                        model.dropdownMenusWithMoreOptionsForTerms
                                    , viewDefinition
                                        model.common.enableMathSupport
                                        model.triedToSaveWhenFormInvalid
                                        definitionArray
                                    , model.form
                                        |> Form.tagCheckboxes
                                        |> viewTags model.common.enableMathSupport
                                        |> Extras.Html.showIf (not <| List.isEmpty <| Form.tagCheckboxes model.form)
                                    , model.form
                                        |> Form.tagCheckboxes
                                        |> List.filterMap
                                            (\( tagWithId, checked ) ->
                                                if checked then
                                                    Just tagWithId

                                                else
                                                    Nothing
                                            )
                                        |> (\tags ->
                                                Extras.Html.showIf (not <| List.isEmpty tags) <|
                                                    viewDisambiguationTag disambiguationTagId tags
                                           )
                                    , viewCreateSeeAlso
                                        model.common.enableMathSupport
                                        model.triedToSaveWhenFormInvalid
                                        idOfRelatedTermBeingDragged
                                        items
                                        terms
                                        relatedTerms
                                        model.dropdownMenusWithMoreOptionsForRelatedTerms
                                        suggestedRelatedTerms
                                    , viewMiscellaneous <| Form.needsUpdating model.form
                                    ]
                                , div
                                    [ class "mt-8 lg:w-1/2 lg:mt-0 max-w-4xl text-gray-900 dark:text-gray-100" ]
                                    [ Html.fieldset
                                        [ class "pt-4 lg:sticky lg:top-5" ]
                                        [ Html.legend
                                            [ class "text-xl text-center text-gray-800 dark:text-gray-300 px-1 select-none" ]
                                            [ text I18n.preview ]
                                        , article
                                            [ id ElementIds.items ]
                                            [ dl
                                                [ style "display" "block" ]
                                                [ Components.GlossaryItemCard.view
                                                    { enableMathSupport = model.common.enableMathSupport, enableLastUpdatedDates = False }
                                                    Components.GlossaryItemCard.Preview
                                                    Nothing
                                                    Nothing
                                                    { previous = Nothing
                                                    , item = Just newOrUpdatedGlossaryItem
                                                    , next = Nothing
                                                    }
                                                ]
                                            ]
                                        ]
                                    ]
                                ]
                            , div
                                [ class "mt-4 lg:mt-8" ]
                                [ viewCreateFormFooter model ]
                            ]
                        ]
                    ]
                ]
            }

        Err _ ->
            { title = I18n.glossaryCapitalised
            , body = [ text I18n.somethingWentWrong ]
            }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ ReceiveCurrentDateTimeAndNewIdForSaving
            |> receiveCurrentDateTimeAndNewIdForSaving
            |> Sub.map PageMsg.Internal
        , model.dropdownMenusWithMoreOptionsForTerms
            |> Dict.toList
            |> List.map
                (\( termIndex, dropdownModel ) ->
                    dropdownModel
                        |> Components.DropdownMenu.subscriptions
                        |> Sub.map (DropdownMenuWithMoreOptionsForTermMsg termIndex >> PageMsg.Internal)
                )
            |> Sub.batch
        , model.dropdownMenusWithMoreOptionsForRelatedTerms
            |> Dict.toList
            |> List.map
                (\( relatedTermIndex, dropdownModel ) ->
                    dropdownModel
                        |> Components.DropdownMenu.subscriptions
                        |> Sub.map (DropdownMenuWithMoreOptionsForRelatedTermMsg relatedTermIndex >> PageMsg.Internal)
                )
            |> Sub.batch
        ]
