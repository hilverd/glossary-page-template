port module Pages.ManageTags exposing (InternalMsg, Model, Msg, init, subscriptions, update, view)

import Accessibility exposing (Html, div, form, h1, main_, p, span, text)
import Accessibility.Aria
import Array exposing (Array)
import Browser exposing (Document)
import Browser.Dom as Dom
import CommonModel exposing (CommonModel)
import Components.Button
import Components.Form
import Components.Spinner
import Data.Editability as Editability
import Data.GlossaryChange as GlossaryChange
import Data.GlossaryChangeWithChecksum exposing (GlossaryChangeWithChecksum)
import Data.GlossaryChangelist as GlossaryChangelist
import Data.GlossaryForUi as Glossary
import Data.GlossaryItem.Tag as Tag
import Data.GlossaryItemsForUi as GlossaryItems
import Data.Notification exposing (Notification)
import Data.Saving exposing (Saving(..))
import Data.TagDescription as TagDescription
import Data.TagId as TagId
import ElementIds
import Extras.Html
import Extras.HtmlAttribute
import Extras.HtmlEvents
import Extras.Http
import Html
import Html.Attributes exposing (class, id)
import Html.Events
import Http
import Icons
import Internationalisation as I18n
import PageMsg exposing (PageMsg)
import Save
import Svg.Attributes
import TagsForm as Form exposing (Row, TagsForm)
import TagsForm.TagDescriptionField as TagDescriptionField exposing (TagDescriptionField)
import TagsForm.TagField as TagField exposing (TagField)
import Task



-- MODEL


type alias Model =
    { common : CommonModel
    , form : TagsForm
    , triedToSaveWhenFormInvalid : Bool
    , saving : Saving
    }


updateForm : (TagsForm -> TagsForm) -> Model -> Model
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
    | AddRow
    | ReceiveUuidForAddingRow String
    | DeleteRow Int
    | UpdateTag Int String
    | UpdateTagDescription Int String
    | Save
    | FailedToSave Http.Error


type alias Msg =
    PageMsg InternalMsg


init : CommonModel -> ( Model, Cmd Msg )
init common =
    case common.glossaryForUi of
        Ok glossaryForUi ->
            ( { common = common
              , form =
                    glossaryForUi
                        |> Glossary.items
                        |> GlossaryItems.describedTags
                        |> Form.create
              , triedToSaveWhenFormInvalid = False
              , saving = NotCurrentlySaving
              }
            , Cmd.none
            )

        _ ->
            ( { common = common
              , form = Form.create []
              , triedToSaveWhenFormInvalid = False
              , saving = NotCurrentlySaving
              }
            , Cmd.none
            )



-- PORTS


port generateUuid : () -> Cmd msg


port receiveUuidForAddingRow : (String -> msg) -> Sub msg



-- UPDATE


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        AddRow ->
            ( model, generateUuid () )

        ReceiveUuidForAddingRow uuid ->
            let
                form : TagsForm
                form =
                    Form.addRow (TagId.create uuid) model.form

                latestIndex : Int
                latestIndex =
                    Array.length (Form.rows form) - 1
            in
            ( updateForm (always form) model
            , giveFocusToTagField latestIndex
            )

        DeleteRow index ->
            ( updateForm (Form.deleteRow index) model, Cmd.none )

        UpdateTag index body ->
            ( updateForm (Form.updateTag index body) model, Cmd.none )

        UpdateTagDescription index body ->
            ( updateForm (Form.updateTagDescription index body) model, Cmd.none )

        Save ->
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
                            glossaryChange =
                                GlossaryChange.ChangeTags <| Form.changes model.form

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
                                    (\( _, updatedGlossaryForUi ) ->
                                        let
                                            common0 : CommonModel
                                            common0 =
                                                model.common
                                        in
                                        PageMsg.NavigateToListAll
                                            { common0 | glossaryForUi = Ok updatedGlossaryForUi }
                                            Nothing
                                            (Just yourChangesHaveBeenSavedNotification)
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


yourChangesHaveBeenSavedNotification : Notification
yourChangesHaveBeenSavedNotification =
    { title = text I18n.saved
    , body = text I18n.yourChangesHaveBeenSaved
    }



-- VIEW


giveFocusToTagField : Int -> Cmd Msg
giveFocusToTagField index =
    Task.attempt (always <| PageMsg.Internal NoOp) (Dom.focus <| ElementIds.tagInputField index)


viewEditTag :
    { enableMathSupport : Bool
    , showValidationErrors : Bool
    }
    -> Int
    -> TagField
    -> TagDescriptionField
    -> Html Msg
viewEditTag { enableMathSupport, showValidationErrors } index tagField tagDescriptionField =
    div
        [ class "flex items-start" ]
        [ span
            [ class "inline-flex items-center mr-1 mt-4" ]
            [ Components.Button.rounded True
                [ Accessibility.Aria.label I18n.delete
                , Html.Events.onClick <| PageMsg.Internal <| DeleteRow index
                ]
                [ Icons.trash
                    [ Svg.Attributes.class "h-5 w-5" ]
                ]
            ]
        , div
            [ class "lg:flex lg:flex-row lg:space-x-6 lg:items-start w-full border-l-4 pl-4 border-gray-300 dark:border-gray-700" ]
            [ div
                [ class "lg:w-1/2 space-y-2" ]
                [ div
                    [ class "block w-full min-w-0"
                    , Extras.HtmlAttribute.showIf (index /= 0) <| class "mt-3"
                    ]
                    [ Components.Form.inputText
                        (TagField.raw tagField)
                        (index == 0)
                        enableMathSupport
                        showValidationErrors
                        (TagField.validationError tagField)
                        [ id <| ElementIds.tagInputField index
                        , Html.Attributes.required True
                        , Html.Attributes.autocomplete False
                        , Html.Attributes.placeholder I18n.tag
                        , Accessibility.Aria.label I18n.tag
                        , Accessibility.Aria.required True
                        , Html.Events.onInput (PageMsg.Internal << UpdateTag index)
                        , Extras.HtmlEvents.onEnter <| PageMsg.Internal NoOp
                        ]
                    ]
                , Extras.Html.showMaybe
                    (\validationError ->
                        p
                            [ class "mt-2 text-red-600 dark:text-red-400" ]
                            [ text validationError ]
                    )
                    (if showValidationErrors then
                        TagField.validationError tagField

                     else
                        Nothing
                    )
                , div
                    [ class "block w-full min-w-0" ]
                    [ div
                        [ class "relative block min-w-0 w-full" ]
                        [ Components.Form.textarea
                            (TagDescriptionField.raw tagDescriptionField)
                            (index == 0)
                            enableMathSupport
                            showValidationErrors
                            (TagDescriptionField.validationError tagDescriptionField)
                            [ Html.Attributes.required True
                            , Html.Attributes.placeholder I18n.description
                            , Accessibility.Aria.label I18n.description
                            , Accessibility.Aria.required True
                            , id <| ElementIds.tagDescriptionInputField index
                            , Html.Events.onInput (PageMsg.Internal << UpdateTagDescription index)
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
                        TagDescriptionField.validationError tagDescriptionField

                     else
                        Nothing
                    )
                ]
            , Html.fieldset
                [ class "px-4 pt-2 pb-4 lg:w-1/2 mt-4 lg:mt-0 border border-gray-300 dark:border-gray-700 rounded-md" ]
                [ Html.legend
                    [ class "text-center text-gray-800 dark:text-gray-100 px-3 py-0.5 select-none" ]
                    [ text I18n.preview ]
                , Extras.Html.showIf (tagField |> TagField.raw |> String.trim |> String.isEmpty |> not) <|
                    div
                        [ class "pb-4" ]
                        [ Components.Button.soft
                            False
                            [ class "mr-2 mt-2"
                            ]
                            [ Tag.view enableMathSupport
                                [ class "text-gray-700 dark:text-gray-100" ]
                                (tagField
                                    |> TagField.raw
                                    |> Tag.fromMarkdown
                                )
                            ]
                        ]
                , div
                    []
                    [ TagDescription.view
                        { enableMathSupport = enableMathSupport }
                        [ class "text-gray-700 dark:text-white" ]
                        (tagDescriptionField
                            |> TagDescriptionField.raw
                            |> TagDescription.fromMarkdown
                        )
                    ]
                ]
            ]
        ]


viewAddTagButtonForEmptyState : Html Msg
viewAddTagButtonForEmptyState =
    Components.Button.emptyState
        [ Html.Events.onClick <| PageMsg.Internal AddRow
        ]
        [ Icons.plus
            [ Svg.Attributes.class "mx-auto h-12 w-12 text-gray-400" ]
        , span
            [ class "mt-2 block font-medium text-gray-900 dark:text-gray-200" ]
            [ text I18n.addTag ]
        ]


viewAddTagButton : Html Msg
viewAddTagButton =
    div []
        [ Components.Button.secondary
            [ Html.Events.onClick <| PageMsg.Internal AddRow
            ]
            [ Icons.plus
                [ Svg.Attributes.class "mx-auto -ml-1 mr-2 h-5 w-5" ]
            , text I18n.addTag
            ]
        ]


tagsFormRowsIsEmpty : List Row -> Bool
tagsFormRowsIsEmpty rows =
    rows
        |> List.filter
            (\row ->
                case row of
                    Form.Existing _ ->
                        True

                    Form.New _ ->
                        True

                    Form.Deleted _ ->
                        False
            )
        |> List.isEmpty


viewEditTags :
    { enableMathSupport : Bool
    , tabbable : Bool
    , showValidationErrors : Bool
    }
    -> Array Form.Row
    -> Html Msg
viewEditTags { enableMathSupport, showValidationErrors } tagsFormRowsArray =
    let
        tagsFormRows : List Row
        tagsFormRows =
            Array.toList tagsFormRowsArray
    in
    div
        [ class "space-y-6 sm:space-y-7" ]
        [ div
            [ class "mt-6 sm:mt-5 space-y-8 sm:space-y-7" ]
            (tagsFormRows
                |> List.indexedMap Tuple.pair
                |> List.filterMap
                    (\( index, row ) ->
                        case row of
                            Form.Existing { tagField, tagDescriptionField } ->
                                Just <|
                                    viewEditTag { enableMathSupport = enableMathSupport, showValidationErrors = showValidationErrors }
                                        index
                                        tagField
                                        tagDescriptionField

                            Form.New { tagField, tagDescriptionField } ->
                                Just <|
                                    viewEditTag { enableMathSupport = enableMathSupport, showValidationErrors = showValidationErrors }
                                        index
                                        tagField
                                        tagDescriptionField

                            Form.Deleted _ ->
                                Nothing
                    )
            )
        , if tagsFormRowsIsEmpty tagsFormRows then
            viewAddTagButtonForEmptyState

          else
            viewAddTagButton
        ]


viewFooter : Model -> Bool -> Html Msg
viewFooter model showValidationErrors =
    let
        form : TagsForm
        form =
            model.form

        saving : Saving
        saving =
            model.saving

        errorDiv : String -> Html msg
        errorDiv message =
            div
                [ class "flex justify-end mt-2" ]
                [ p
                    [ class "text-red-600 dark:text-red-400" ]
                    [ text message ]
                ]
    in
    div
        [ class "pt-5 lg:border-t dark:border-gray-700 flex flex-col items-center" ]
        [ errorDiv I18n.thereAreErrorsOnThisFormSeeAbove
            |> Extras.Html.showIf (showValidationErrors && Form.hasValidationErrors form)
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
        , Extras.Html.showIf (model.common.editability == Editability.EditingInMemory) <|
            div
                [ class "mt-2 mb-2 text-sm text-gray-500 dark:text-gray-400 sm:text-right" ]
                [ text I18n.savingChangesInMemoryMessage ]
        , div
            [ class "mt-4 flex items-center" ]
            [ Components.Button.white
                (saving /= SavingInProgress)
                [ Html.Events.onClick <|
                    PageMsg.NavigateToListAll model.common Nothing Nothing
                ]
                [ text I18n.cancel ]
            , Components.Button.primary
                (saving /= SavingInProgress && not (showValidationErrors && Form.hasValidationErrors form))
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


view : Model -> Document Msg
view model =
    { title = I18n.manageTagsTitle
    , body =
        [ div
            [ class "container mx-auto px-6 pb-12 lg:px-8 max-w-4xl lg:max-w-(--breakpoint-2xl)" ]
            [ main_
                []
                [ h1
                    [ class "text-3xl font-bold leading-tight text-gray-900 dark:text-gray-100 print:text-black pt-6" ]
                    [ text I18n.manageTagsTitle
                    ]
                , p
                    [ class "mt-6 max-w-prose text-gray-900 dark:text-gray-100" ]
                    [ text I18n.youCanUseTagsToAttachLabels ]
                , Html.details
                    []
                    [ Html.summary
                        [ class "mt-2 mb-1 items-center font-medium text-gray-900 dark:text-gray-100 select-none" ]
                        [ span
                            [ class "ml-2" ]
                            [ text I18n.readMore ]
                        ]
                    , div
                        [ class "mb-1 max-w-prose text-gray-900 dark:text-gray-100" ]
                        [ text I18n.whyTagsMayBeUseful ]
                    ]
                , form
                    [ class "pt-7" ]
                    [ model.form
                        |> Form.rows
                        |> viewEditTags
                            { enableMathSupport = model.common.enableMathSupport
                            , tabbable = True
                            , showValidationErrors = model.triedToSaveWhenFormInvalid
                            }
                    , div
                        [ class "mt-4 lg:mt-8" ]
                        [ viewFooter model model.triedToSaveWhenFormInvalid ]
                    ]
                ]
            ]
        ]
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    ReceiveUuidForAddingRow
        |> receiveUuidForAddingRow
        |> Sub.map PageMsg.Internal
