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
import Components.Spinner
import Data.Glossary exposing (Glossary)
import Data.GlossaryItem.Tag as Tag exposing (Tag)
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Data.Saving exposing (Saving(..))
import Data.TagDescription as TagDescription exposing (TagDescription)
import ElementIds
import Extras.Html
import Extras.HtmlEvents
import Html exposing (h2)
import Html.Attributes exposing (class, id)
import Html.Events
import Icons
import Json.Decode as Decode
import PageMsg exposing (PageMsg)
import Platform exposing (Task)
import Svg.Attributes
import TagsForm as Form exposing (TagsForm)
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
    | AddTagWithDescription
    | DeleteTagWithDescription Int
    | UpdateTag Int String
    | UpdateTagDescription Int String
    | Save


type alias Msg =
    PageMsg InternalMsg


init : CommonModel -> ( Model, Cmd Msg )
init common =
    case common.glossary of
        Ok { items } ->
            ( { common = common
              , form =
                    items
                        |> GlossaryItems.tagsWithDescriptions
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


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        AddTagWithDescription ->
            let
                form : TagsForm
                form =
                    Form.addTagWithDescription model.form

                latestIndex : Int
                latestIndex =
                    Array.length (Form.tagsWithDescriptionsFields form) - 1
            in
            ( { model | form = form }
            , giveFocusToTagField latestIndex
            )

        DeleteTagWithDescription index ->
            ( { model | form = Form.deleteTagWithDescription index model.form }, Cmd.none )

        UpdateTag index body ->
            ( { model | form = Form.updateTag index model.form body }, Cmd.none )

        UpdateTagDescription index body ->
            ( { model | form = Form.updateTagDescription index model.form body }, Cmd.none )

        Save ->
            ( model, Cmd.none )



-- VIEW


giveFocusToTagField : Int -> Cmd Msg
giveFocusToTagField index =
    Task.attempt (always <| PageMsg.Internal NoOp) (Dom.focus <| ElementIds.tagInputField index)


viewEditTag : { enableMathSupport : Bool, tabbable : Bool } -> Int -> Int -> ( TagField, TagDescriptionField ) -> Html Msg
viewEditTag { enableMathSupport, tabbable } _ index ( tagField, tagDescriptionField ) =
    div
        [ class "flex items-center" ]
        [ span
            [ class "inline-flex items-center mr-1" ]
            [ Components.Button.rounded True
                [ Accessibility.Aria.label "Delete"
                , Html.Events.onClick <| PageMsg.Internal <| DeleteTagWithDescription index
                ]
                [ Icons.trash
                    [ Svg.Attributes.class "h-5 w-5" ]
                ]
            ]
        , div
            [ class "lg:flex lg:flex-row lg:space-x-6 lg:items-center w-full border-l-4 pl-4 border-gray-300 dark:border-gray-700" ]
            [ div
                [ class "lg:w-1/2 space-y-2" ]
                [ div
                    [ class "block w-full min-w-0" ]
                    [ Components.Form.inputText
                        (TagField.raw tagField)
                        True
                        enableMathSupport
                        False
                        -- showValidationErrors
                        Nothing
                        -- (TagField.validationError tagField)
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
                , div
                    [ class "block w-full min-w-0" ]
                    [ div
                        [ class "relative block min-w-0 w-full" ]
                        [ Components.Form.textarea
                            (TagDescriptionField.raw tagDescriptionField)
                            False
                            enableMathSupport
                            False
                            Nothing
                            [ Html.Attributes.required True
                            , Html.Attributes.placeholder "Description"
                            , Accessibility.Aria.label "Description"
                            , Accessibility.Aria.required True

                            -- , Html.Attributes.id ElementIds.tagDescriptionInputField
                            , Html.Events.onInput (PageMsg.Internal << UpdateTagDescription index)
                            ]
                        ]
                    ]
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
        [ Html.Events.onClick <| PageMsg.Internal AddTagWithDescription
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
            [ Html.Events.onClick <| PageMsg.Internal AddTagWithDescription
            ]
            [ Icons.plus
                [ Svg.Attributes.class "mx-auto -ml-1 mr-2 h-5 w-5" ]
            , text "Add tag"
            ]
        ]


viewEditTags : { enableMathSupport : Bool, tabbable : Bool } -> Array ( TagField, TagDescriptionField ) -> Html Msg
viewEditTags { enableMathSupport, tabbable } tagsWithDescriptionsFieldsArray =
    let
        numberOfTags =
            Array.length tagsWithDescriptionsFieldsArray

        tagsAndDescriptions =
            Array.toList tagsWithDescriptionsFieldsArray
    in
    div
        [ class "space-y-6 sm:space-y-7" ]
        [ div
            [ class "mt-6 sm:mt-5 space-y-8 sm:space-y-7" ]
            (List.indexedMap
                (viewEditTag { enableMathSupport = enableMathSupport, tabbable = tabbable } numberOfTags)
                tagsAndDescriptions
            )
        , if List.isEmpty tagsAndDescriptions then
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
        [ Extras.Html.showIf model.common.enableSavingChangesInMemory <|
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
                                |> Form.tagsWithDescriptionsFields
                                |> viewEditTags { enableMathSupport = enableMathSupport, tabbable = True }
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
