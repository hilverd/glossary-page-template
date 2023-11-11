module Pages.ManageTags exposing (InternalMsg, Model, Msg, init, subscriptions, update, view)

import Accessibility exposing (Html, div, form, h1, main_, p, span, text)
import Accessibility.Aria exposing (required)
import Array exposing (Array)
import Browser exposing (Document)
import Browser.Dom as Dom
import CommonModel exposing (CommonModel)
import Components.Button
import Components.Copy
import Components.Form
import Components.SelectMenu exposing (showValidationErrors)
import Components.Spinner
import Data.Glossary as Glossary exposing (Glossary)
import Data.GlossaryItem.Tag as Tag exposing (Tag)
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Data.Saving exposing (Saving(..))
import Data.TagDescription as TagDescription exposing (TagDescription)
import ElementIds
import Extras.Html
import Extras.HtmlAttribute
import Extras.HtmlEvents
import Extras.HtmlTree as HtmlTree
import Extras.Http
import Extras.Task
import Html exposing (h2)
import Html.Attributes exposing (class, id)
import Html.Events
import Http
import Icons
import Json.Decode as Decode
import PageMsg exposing (PageMsg)
import Platform exposing (Task)
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


type InternalMsg
    = NoOp
    | AddRow
    | DeleteRow Int
    | UpdateTag Int String
    | UpdateTagDescription Int String
    | Save
    | FailedToSave Http.Error


type alias Msg =
    PageMsg InternalMsg


init : CommonModel -> ( Model, Cmd Msg )
init common =
    case common.glossary of
        Ok { items } ->
            ( { common = common
              , form =
                    items
                        |> GlossaryItems.tagsWithIdsAndDescriptions
                        |> Form.create
              , triedToSaveWhenFormInvalid = False
              , saving = NotSaving
              }
            , Cmd.none
            )

        _ ->
            ( { common = common
              , form = Form.create []
              , triedToSaveWhenFormInvalid = False
              , saving = NotSaving
              }
            , Cmd.none
            )



-- UPDATE


applyChanges : List Form.Row -> Glossary -> Glossary
applyChanges rows glossary =
    let
        items0 : GlossaryItems
        items0 =
            glossary.items

        items1 : GlossaryItems
        items1 =
            List.foldl
                (\row items ->
                    case row of
                        Form.Existing { id, tagField, tagDescriptionField } ->
                            items
                                |> GlossaryItems.updateTag id
                                    (tagField |> TagField.raw |> Tag.fromMarkdown)
                                    (tagDescriptionField |> TagDescriptionField.raw |> String.trim |> TagDescription.fromMarkdown)

                        Form.Deleted tagId ->
                            GlossaryItems.removeTag tagId items

                        Form.New { tagField, tagDescriptionField } ->
                            items
                                |> GlossaryItems.insertTag
                                    (tagField |> TagField.raw |> Tag.fromMarkdown)
                                    (tagDescriptionField |> TagDescriptionField.raw |> String.trim |> TagDescription.fromMarkdown)
                )
                items0
                rows
    in
    { glossary | items = items1 }


patchHtmlFile : CommonModel -> GlossaryItems -> Cmd Msg
patchHtmlFile common glossaryItems =
    let
        msg : PageMsg a
        msg =
            PageMsg.NavigateToListAll common
    in
    if common.enableSavingChangesInMemory then
        Extras.Task.messageToCommand msg

    else
        case common.glossary of
            Ok glossary0 ->
                let
                    glossary : Glossary
                    glossary =
                        { glossary0 | items = glossaryItems }
                in
                Http.request
                    { method = "PATCH"
                    , headers = []
                    , url = "/"
                    , body =
                        glossary
                            |> Glossary.toHtmlTree common.enableExportMenu common.enableOrderItemsButtons common.enableHelpForMakingChanges
                            |> HtmlTree.toHtmlReplacementString
                            |> Http.stringBody "text/html"
                    , expect =
                        Http.expectWhatever
                            (\result ->
                                case result of
                                    Ok _ ->
                                        msg

                                    Err error ->
                                        PageMsg.Internal <| FailedToSave error
                            )
                    , timeout = Nothing
                    , tracker = Nothing
                    }

            _ ->
                Cmd.none


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        AddRow ->
            let
                form : TagsForm
                form =
                    Form.addRow model.form

                latestIndex : Int
                latestIndex =
                    Array.length (Form.rows form) - 1
            in
            ( { model | form = form }
            , giveFocusToTagField latestIndex
            )

        DeleteRow index ->
            ( { model | form = Form.deleteRow index model.form }, Cmd.none )

        UpdateTag index body ->
            ( { model | form = Form.updateTag index model.form body }, Cmd.none )

        UpdateTagDescription index body ->
            ( { model | form = Form.updateTagDescription index model.form body }, Cmd.none )

        Save ->
            case model.common.glossary of
                Ok glossary0 ->
                    if Form.hasValidationErrors model.form then
                        ( { model
                            | triedToSaveWhenFormInvalid = True
                            , saving = NotSaving
                          }
                        , Cmd.none
                        )

                    else
                        let
                            common0 : CommonModel
                            common0 =
                                model.common

                            common1 : CommonModel
                            common1 =
                                { common0
                                    | glossary =
                                        glossary0
                                            |> applyChanges (model.form |> Form.rows |> Array.toList)
                                            |> Ok
                                }

                            model1 : Model
                            model1 =
                                { model
                                    | common = common1
                                    , saving = SavingInProgress
                                }
                        in
                        ( model1
                        , patchHtmlFile model1.common glossary0.items
                        )

                _ ->
                    ( model, Cmd.none )

        FailedToSave error ->
            ( { model | saving = SavingFailed <| Extras.Http.errorToHumanReadable <| error }
            , Cmd.none
            )



-- VIEW


giveFocusToTagField : Int -> Cmd Msg
giveFocusToTagField index =
    Task.attempt (always <| PageMsg.Internal NoOp) (Dom.focus <| ElementIds.tagInputField index)


viewEditTag :
    { enableMathSupport : Bool
    , tabbable : Bool
    , showValidationErrors : Bool
    }
    -> Int
    -> TagField
    -> TagDescriptionField
    -> Html Msg
viewEditTag { enableMathSupport, tabbable, showValidationErrors } index tagField tagDescriptionField =
    div
        [ class "flex items-start" ]
        [ span
            [ class "inline-flex items-center mr-1 mt-4" ]
            [ Components.Button.rounded True
                [ Accessibility.Aria.label "Delete"
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
                        , Html.Attributes.placeholder "Tag"
                        , Accessibility.Aria.label "Tag"
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
                            , Html.Attributes.placeholder "Description"
                            , Accessibility.Aria.label "Description"
                            , Accessibility.Aria.required True

                            -- , Html.Attributes.id ElementIds.tagDescriptionInputField
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
                    [ text "Preview" ]
                , div
                    [ class "pb-4" ]
                    [ Components.Button.soft
                        tabbable
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
                        { enableMathSupport = enableMathSupport
                        , makeLinksTabbable = tabbable
                        }
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
            [ text "Add tag" ]
        ]


viewAddTagButton : Html Msg
viewAddTagButton =
    div []
        [ Components.Button.secondary
            [ Html.Events.onClick <| PageMsg.Internal AddRow
            ]
            [ Icons.plus
                [ Svg.Attributes.class "mx-auto -ml-1 mr-2 h-5 w-5" ]
            , text "Add tag"
            ]
        ]


viewEditTags :
    { enableMathSupport : Bool
    , tabbable : Bool
    , showValidationErrors : Bool
    }
    -> Array Form.Row
    -> Html Msg
viewEditTags { enableMathSupport, tabbable, showValidationErrors } tagsFormRowsArray =
    let
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
                                    viewEditTag { enableMathSupport = enableMathSupport, tabbable = tabbable, showValidationErrors = showValidationErrors }
                                        index
                                        tagField
                                        tagDescriptionField

                            Form.New { tagField, tagDescriptionField } ->
                                Just <|
                                    viewEditTag { enableMathSupport = enableMathSupport, tabbable = tabbable, showValidationErrors = showValidationErrors }
                                        index
                                        tagField
                                        tagDescriptionField

                            Form.Deleted _ ->
                                Nothing
                    )
            )
        , if List.isEmpty tagsFormRows then
            viewAddTagButtonForEmptyState

          else
            viewAddTagButton
        ]


viewFooter : Model -> Bool -> GlossaryItems -> Html Msg
viewFooter model showValidationErrors glossaryItems =
    let
        form =
            model.form

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

        common : CommonModel
        common =
            model.common

        updatedGlossary : Result Decode.Error Glossary
        updatedGlossary =
            case common.glossary of
                Ok glossary ->
                    Ok { glossary | items = glossaryItems }

                error ->
                    error
    in
    div
        [ class "pt-5 lg:border-t dark:border-gray-700 flex flex-col items-center" ]
        [ errorDiv "There are errors on this form — see above."
            |> Extras.Html.showIf (showValidationErrors && Form.hasValidationErrors form)
        , Extras.Html.showIf model.common.enableSavingChangesInMemory <|
            div
                [ class "mt-2 mb-2 text-sm text-gray-500 dark:text-gray-400 sm:text-right" ]
                [ text Components.Copy.sandboxModeMessage ]
        , div
            [ class "flex items-center" ]
            [ Components.Button.white
                (saving /= SavingInProgress)
                [ Html.Events.onClick <|
                    PageMsg.NavigateToListAll { common | glossary = updatedGlossary }
                ]
                [ text "Cancel" ]
            , Components.Button.primary
                (saving /= SavingInProgress && not (showValidationErrors && Form.hasValidationErrors form))
                [ class "ml-3"
                , Html.Events.onClick <| PageMsg.Internal Save
                ]
                [ text "Save" ]
            , Components.Spinner.view
                [ Svg.Attributes.class "ml-3 w-8 h-8" ]
                (saving == SavingInProgress)
            ]
        , case saving of
            SavingFailed errorMessage ->
                errorDiv <| "Failed to save — " ++ errorMessage ++ "."

            _ ->
                Extras.Html.nothing
        ]


view : Model -> Document Msg
view model =
    case model.common.glossary of
        Ok { enableMathSupport, items } ->
            { title = "Manage Tags"
            , body =
                [ div
                    [ class "container mx-auto px-6 pb-12 lg:px-8 max-w-4xl lg:max-w-screen-2xl" ]
                    [ main_
                        []
                        [ h1
                            [ class "text-3xl font-bold leading-tight text-gray-900 dark:text-gray-100 print:text-black pt-6" ]
                            [ text "Manage Tags"
                            ]
                        , form
                            [ class "pt-7" ]
                            [ model.form
                                |> Form.rows
                                |> viewEditTags
                                    { enableMathSupport = enableMathSupport
                                    , tabbable = True
                                    , showValidationErrors = model.triedToSaveWhenFormInvalid
                                    }
                            , div
                                [ class "mt-4 lg:mt-8" ]
                                [ viewFooter model model.triedToSaveWhenFormInvalid items ]
                            ]
                        ]
                    ]
                ]
            }

        Err _ ->
            { title = "Manage Tags"
            , body = [ text "Something went wrong." ]
            }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
