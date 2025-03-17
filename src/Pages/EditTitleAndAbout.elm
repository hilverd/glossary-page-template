module Pages.EditTitleAndAbout exposing (InternalMsg, Model, Msg, init, subscriptions, update, view)

import Accessibility exposing (Html, div, form, h1, h2, label, main_, p, span, text)
import Accessibility.Aria
import Array exposing (Array)
import Browser exposing (Document)
import Browser.Dom as Dom
import CommonModel exposing (CommonModel)
import Components.AboutSection
import Components.Button
import Components.Form
import Components.Spinner
import Data.AboutLink as AboutLink
import Data.AboutLinkIndex as AboutLinkIndex exposing (AboutLinkIndex)
import Data.AboutParagraph as AboutParagraph
import Data.AboutSection exposing (AboutSection)
import Data.Checksum exposing (Checksum)
import Data.Editability as Editability
import Data.GlossaryChange as GlossaryChange exposing (GlossaryChange)
import Data.GlossaryChangeWithChecksum exposing (GlossaryChangeWithChecksum)
import Data.GlossaryChangelist as GlossaryChangelist exposing (GlossaryChangelist)
import Data.GlossaryForUi as Glossary
import Data.GlossaryTitle as GlossaryTitle
import Data.Saving exposing (Saving(..))
import ElementIds
import Extras.Html
import Extras.HtmlEvents
import Extras.Http
import Html
import Html.Attributes exposing (class, for, id, name, placeholder, required, spellcheck, type_)
import Html.Events
import Http
import Icons
import Internationalisation as I18n
import PageMsg exposing (PageMsg)
import Save
import Svg.Attributes
import Task
import TitleAndAboutForm as Form exposing (TitleAndAboutForm)



-- MODEL


type alias Model =
    { common : CommonModel
    , form : TitleAndAboutForm
    , triedToSaveWhenFormInvalid : Bool
    , saving : Saving
    }


type InternalMsg
    = NoOp
    | UpdateTitle String
    | UpdateAboutParagraph String
    | UpdateAboutLinkHref AboutLinkIndex String
    | UpdateAboutLinkBody AboutLinkIndex String
    | AddAboutLink
    | DeleteAboutLink AboutLinkIndex
    | Save
    | FailedToSave Http.Error


type alias Msg =
    PageMsg InternalMsg


init : CommonModel -> ( Model, Cmd Msg )
init common =
    case common.glossaryForUi of
        Ok glossaryForUi ->
            ( { common = common
              , form = Form.create (Glossary.title glossaryForUi) (Glossary.aboutSection glossaryForUi)
              , triedToSaveWhenFormInvalid = False
              , saving = NotCurrentlySaving
              }
            , Cmd.none
            )

        _ ->
            ( { common = common
              , form =
                    Form.create (GlossaryTitle.fromMarkdown "")
                        { paragraph = AboutParagraph.fromMarkdown ""
                        , links = []
                        }
              , triedToSaveWhenFormInvalid = False
              , saving = NotCurrentlySaving
              }
            , Cmd.none
            )



-- UPDATE


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateTitle body ->
            ( { model | form = Form.updateTitle body model.form }, Cmd.none )

        UpdateAboutParagraph body ->
            ( { model | form = Form.updateAboutParagraph body model.form }, Cmd.none )

        UpdateAboutLinkHref index href ->
            ( { model | form = Form.updateAboutLinkHref index href model.form }, Cmd.none )

        UpdateAboutLinkBody index body ->
            ( { model | form = Form.updateAboutLinkBody index body model.form }, Cmd.none )

        AddAboutLink ->
            let
                form : TitleAndAboutForm
                form =
                    Form.addAboutLink model.form

                latestAboutLinkIndex : AboutLinkIndex
                latestAboutLinkIndex =
                    form
                        |> Form.aboutLinkFields
                        |> Array.length
                        |> (+) -1
                        |> AboutLinkIndex.fromInt
            in
            ( { model | form = form }
            , giveFocusToAboutLinkHref latestAboutLinkIndex
            )

        DeleteAboutLink aboutLinkIndex ->
            ( { model | form = Form.deleteAboutLink aboutLinkIndex model.form }, Cmd.none )

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
                            setTitleChange : GlossaryChange
                            setTitleChange =
                                GlossaryChange.SetTitle <| titleFromForm model.form

                            setAboutSectionChange : GlossaryChange
                            setAboutSectionChange =
                                GlossaryChange.SetAboutSection <| aboutSectionFromForm model.form

                            glossaryChangesWithChecksums : List GlossaryChangeWithChecksum
                            glossaryChangesWithChecksums =
                                [ setTitleChange, setAboutSectionChange ]
                                    |> List.map
                                        (\glossaryChange ->
                                            { glossaryChange = glossaryChange
                                            , checksum =
                                                Glossary.checksumForChange glossaryForUi glossaryChange
                                            }
                                        )

                            changelist : GlossaryChangelist
                            changelist =
                                GlossaryChangelist.create
                                    (Glossary.versionNumber glossaryForUi)
                                    glossaryChangesWithChecksums

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


titleFromForm : Form.TitleAndAboutForm -> GlossaryTitle.GlossaryTitle
titleFromForm =
    Form.titleField
        >> .body
        >> GlossaryTitle.fromMarkdown


aboutSectionFromForm : Form.TitleAndAboutForm -> AboutSection
aboutSectionFromForm form =
    { paragraph =
        form
            |> Form.aboutParagraphField
            |> .body
            |> AboutParagraph.fromMarkdown
    , links =
        form
            |> Form.aboutLinkFields
            |> Array.toList
            |> List.map
                (\( href, body ) ->
                    AboutLink.create href.href body.body
                )
    }



-- VIEW


giveFocusToAboutLinkHref : AboutLinkIndex -> Cmd Msg
giveFocusToAboutLinkHref index =
    Task.attempt (always <| PageMsg.Internal NoOp) (Dom.focus <| ElementIds.aboutLinkHref index)


viewEditTitle : Bool -> Bool -> Form.TitleField -> Html Msg
viewEditTitle mathSupportEnabled showValidationErrors titleField =
    div []
        [ div []
            [ h2
                [ class "text-lg leading-6 font-medium text-gray-900 dark:text-gray-100" ]
                [ text I18n.title ]
            ]
        , div
            [ class "mt-4" ]
            [ div
                [ class "sm:flex sm:flex-row sm:items-center" ]
                [ div
                    [ class "flex-auto max-w-prose flex" ]
                    [ div
                        [ class "flex-auto" ]
                        [ div
                            [ class "sm:flex sm:flex-row sm:items-center" ]
                            [ Html.div
                                [ class "block w-full min-w-0" ]
                                [ Components.Form.inputText
                                    titleField.body
                                    True
                                    mathSupportEnabled
                                    showValidationErrors
                                    titleField.validationError
                                    [ required True
                                    , Html.Attributes.autocomplete False
                                    , Html.Attributes.id ElementIds.titleInputField
                                    , Accessibility.Aria.label I18n.title
                                    , Accessibility.Aria.required True
                                    , Html.Events.onInput (PageMsg.Internal << UpdateTitle)
                                    , Extras.HtmlEvents.onEnter <| PageMsg.Internal NoOp
                                    ]
                                ]
                            ]
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
                    titleField.validationError

                 else
                    Nothing
                )
            ]
        ]


viewEditAboutParagraph : Bool -> Bool -> Form.AboutParagraphField -> Html Msg
viewEditAboutParagraph mathSupportEnabled showValidationErrors aboutParagraphField =
    div []
        [ div []
            [ h2
                [ class "text-lg leading-6 font-medium text-gray-900 dark:text-gray-100" ]
                [ text I18n.about ]
            ]
        , div
            [ class "mt-4 max-w-2xl" ]
            [ div
                [ class "sm:flex sm:flex-row sm:items-center" ]
                [ div
                    [ class "relative block min-w-0 w-full" ]
                    [ Components.Form.textarea
                        aboutParagraphField.body
                        True
                        mathSupportEnabled
                        showValidationErrors
                        aboutParagraphField.validationError
                        [ required True
                        , Accessibility.Aria.label I18n.about
                        , Accessibility.Aria.required True
                        , Html.Attributes.id ElementIds.aboutParagraphInputField
                        , Html.Events.onInput (PageMsg.Internal << UpdateAboutParagraph)
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
                    aboutParagraphField.validationError

                 else
                    Nothing
                )
            ]
        ]


viewEditAboutLinks : Bool -> Array Form.AboutLinkField -> Html Msg
viewEditAboutLinks showValidationErrors aboutLinkFieldsArray =
    let
        aboutLinkFields : List Form.AboutLinkField
        aboutLinkFields =
            Array.toList aboutLinkFieldsArray
    in
    div
        [ class "space-y-5 sm:space-y-6" ]
        [ div []
            [ h2
                [ class "text-lg leading-6 font-medium text-gray-900 dark:text-gray-100" ]
                [ text I18n.links ]
            ]
        , div
            [ class "space-y-6 sm:space-y-5" ]
            (List.indexedMap
                (viewEditAboutLink showValidationErrors << AboutLinkIndex.fromInt)
                aboutLinkFields
                ++ [ if List.isEmpty aboutLinkFields then
                        viewAddAboutLinkButtonForEmptyState

                     else
                        viewAddAboutLinkButton
                   ]
            )
        ]


viewAddAboutLinkButtonForEmptyState : Html Msg
viewAddAboutLinkButtonForEmptyState =
    Components.Button.emptyState
        [ Html.Events.onClick <| PageMsg.Internal AddAboutLink ]
        [ Icons.plus
            [ Svg.Attributes.class "mx-auto h-12 w-12 text-gray-400" ]
        , span
            [ class "mt-2 block font-medium text-gray-900 dark:text-gray-200" ]
            [ text I18n.addLinkButton ]
        ]


viewAddAboutLinkButton : Html Msg
viewAddAboutLinkButton =
    div []
        [ Components.Button.secondary
            [ Html.Events.onClick <| PageMsg.Internal AddAboutLink ]
            [ Icons.plus
                [ Svg.Attributes.class "mx-auto -ml-1 mr-2 h-5 w-5" ]
            , text I18n.addLinkButton
            ]
        ]


viewEditAboutLink : Bool -> AboutLinkIndex -> ( Form.AboutLinkHref, Form.AboutLinkBody ) -> Html Msg
viewEditAboutLink showValidationErrors index ( aboutLinkHref, aboutLinkBody ) =
    div []
        [ div
            [ class "flex flex-auto max-w-2xl" ]
            [ span
                [ class "flex-none inline-flex items-center" ]
                [ Components.Button.rounded True
                    [ Accessibility.Aria.label I18n.delete
                    , Html.Events.onClick <| PageMsg.Internal <| DeleteAboutLink index
                    ]
                    [ Icons.trash
                        [ Svg.Attributes.class "h-5 w-5" ]
                    ]
                ]
            , div
                [ class "flex-1 isolate -space-y-px rounded-md shadow-xs bg-white dark:bg-gray-700" ]
                [ div
                    [ if not showValidationErrors || aboutLinkHref.validationError == Nothing then
                        class "relative border border-gray-300 dark:border-gray-500 rounded-md rounded-b-none px-3 py-2 focus-within:z-10 focus-within:ring-1 focus-within:ring-indigo-600 focus-within:border-indigo-600"

                      else
                        class "relative border border-red-300 dark:border-red-700 rounded-md rounded-b-none px-3 py-2 focus-within:z-10 focus-within:ring-1 focus-within:ring-red-500 focus-within:border-red-500"
                    ]
                    [ label
                        [ class "block text-sm font-semibold text-gray-500 dark:text-gray-300"
                        , for <| ElementIds.aboutLinkHref index
                        ]
                        [ text I18n.url ]
                    , Components.Form.input
                        aboutLinkHref.href
                        showValidationErrors
                        aboutLinkHref.validationError
                        [ id <| ElementIds.aboutLinkHref index
                        , name <| ElementIds.aboutLinkHref index
                        , placeholder "https://example.com"
                        , type_ "url"
                        , spellcheck False
                        , required True
                        , Html.Attributes.autocomplete False
                        , Accessibility.Aria.required True
                        , Html.Events.onInput (PageMsg.Internal << UpdateAboutLinkHref index)
                        , Extras.HtmlEvents.onEnter <| PageMsg.Internal NoOp
                        ]
                    ]
                , div
                    [ if not showValidationErrors || aboutLinkBody.validationError == Nothing then
                        class "border border-gray-300 dark:border-gray-500 rounded-md rounded-t-none px-3 py-2 focus-within:z-10 focus-within:ring-1 focus-within:ring-indigo-600 focus-within:border-indigo-600"

                      else
                        class "border border-red-300 dark:border-red-700 rounded-md rounded-t-none px-3 py-2 focus-within:z-10 focus-within:ring-1 focus-within:ring-red-500 focus-within:border-red-500"
                    ]
                    [ label
                        [ class "block text-sm font-semibold text-gray-500 dark:text-gray-300"
                        , for <| ElementIds.aboutLinkBody index
                        ]
                        [ text I18n.textLabel ]
                    , Html.div
                        [ class "block w-full min-w-0" ]
                        [ Html.div
                            [ class "block w-full min-w-0" ]
                            [ Components.Form.inputText
                                aboutLinkBody.body
                                False
                                False
                                showValidationErrors
                                aboutLinkBody.validationError
                                [ class "block w-full border-0 p-0 focus:ring-0 dark:bg-gray-700 dark:text-white"
                                , id <| ElementIds.aboutLinkBody index
                                , name <| ElementIds.aboutLinkBody index
                                , placeholder I18n.example
                                , type_ "text"
                                , required True
                                , Html.Attributes.autocomplete False
                                , Accessibility.Aria.required True
                                , Accessibility.Aria.invalid <| aboutLinkHref.validationError /= Nothing
                                , Html.Events.onInput (PageMsg.Internal << UpdateAboutLinkBody index)
                                , Extras.HtmlEvents.onEnter <| PageMsg.Internal NoOp
                                ]
                            ]
                        ]
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
                orMaybe aboutLinkHref.validationError aboutLinkBody.validationError

             else
                Nothing
            )
        ]


orMaybe : Maybe a -> Maybe a -> Maybe a
orMaybe first second =
    case first of
        Just _ ->
            first

        _ ->
            second


viewCreateFormFooter : Model -> Bool -> Html Msg
viewCreateFormFooter model showValidationErrors =
    let
        form : TitleAndAboutForm
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
        , Extras.Html.showIf (model.common.editability == Editability.EditingInMemory) <|
            div
                [ class "mt-2 mb-2 text-sm text-gray-500 dark:text-gray-400 sm:text-right" ]
                [ text I18n.savingChangesInMemoryMessage ]
        , div
            [ class "flex items-center" ]
            [ Components.Button.white
                (saving /= SavingInProgress)
                [ Html.Events.onClick <|
                    PageMsg.NavigateToListAll model.common Nothing
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
                errorDiv <| "Failed to save — " ++ errorMessage ++ "."

            _ ->
                Extras.Html.nothing
        ]


view : Model -> Document Msg
view model =
    let
        title1 : GlossaryTitle.GlossaryTitle
        title1 =
            titleFromForm model.form

        aboutSection : AboutSection
        aboutSection =
            aboutSectionFromForm model.form
    in
    { title = GlossaryTitle.inlineText title1
    , body =
        [ div
            [ class "container mx-auto px-6 pb-12 lg:px-8 max-w-4xl lg:max-w-(--breakpoint-2xl)" ]
            [ main_
                []
                [ h1
                    [ class "text-3xl font-bold leading-tight text-gray-900 dark:text-gray-100 print:text-black pt-6" ]
                    [ text I18n.editTitleAndAboutSectionHeading
                    ]
                , form
                    [ class "pt-7" ]
                    [ div
                        [ class "lg:flex lg:space-x-8" ]
                        [ div
                            [ class "lg:w-1/2 space-y-7 lg:space-y-8" ]
                            [ viewEditTitle model.common.enableMathSupport model.triedToSaveWhenFormInvalid <| Form.titleField model.form
                            , viewEditAboutParagraph model.common.enableMathSupport model.triedToSaveWhenFormInvalid <| Form.aboutParagraphField model.form
                            , viewEditAboutLinks model.triedToSaveWhenFormInvalid <| Form.aboutLinkFields model.form
                            ]
                        , div
                            [ class "mt-8 lg:w-1/2 lg:mt-0 text-gray-900 dark:text-gray-100" ]
                            [ Html.fieldset
                                [ class "border border-solid rounded-md border-gray-300 dark:border-gray-700 p-4" ]
                                [ Html.legend
                                    [ class "text-xl text-center text-gray-800 dark:text-gray-300 px-3 py-0.5 select-none" ]
                                    [ text I18n.preview ]
                                , h2
                                    [ class "pb-4" ]
                                    [ GlossaryTitle.view model.common.enableMathSupport
                                        [ class "text-2xl font-bold leading-tight text-gray-700 dark:text-gray-300" ]
                                        title1
                                    ]
                                , Components.AboutSection.view
                                    { enableMathSupport = model.common.enableMathSupport }
                                    aboutSection
                                ]
                            ]
                        ]
                    , div
                        [ class "mt-4 lg:mt-8" ]
                        [ viewCreateFormFooter model model.triedToSaveWhenFormInvalid ]
                    ]
                ]
            ]
        ]
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
