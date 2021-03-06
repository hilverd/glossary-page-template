module Pages.EditTitleAndAbout exposing (Model, Msg, init, subscriptions, update, view)

import Accessibility exposing (..)
import Accessibility.Aria
import Array exposing (Array)
import Browser exposing (Document)
import Browser.Dom as Dom
import CommonModel exposing (CommonModel, OrderItemsBy(..))
import Components.Button
import Components.Copy
import Components.Form
import Data.AboutLink as AboutLink
import Data.AboutLinkIndex as AboutLinkIndex exposing (AboutLinkIndex)
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Data.GlossaryTitle as GlossaryTitle
import ElementIds
import Extras.Html
import Extras.HtmlEvents
import Extras.HtmlTree as HtmlTree exposing (HtmlTree(..))
import Extras.Http
import Extras.Task
import Html
import Html.Attributes exposing (class, for, id, name, placeholder, required, spellcheck, type_)
import Html.Events
import Http
import Icons
import PageMsg exposing (PageMsg)
import Svg.Attributes
import Task
import TitleAndAboutForm as Form exposing (TitleAndAboutForm)



-- MODEL


type alias Model =
    { common : CommonModel
    , form : TitleAndAboutForm
    , triedToSaveWhenFormInvalid : Bool
    , errorMessageWhileSaving : Maybe String
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
    ( { common = common
      , form = Form.create common.title common.aboutParagraph common.aboutLinks
      , triedToSaveWhenFormInvalid = False
      , errorMessageWhileSaving = Nothing
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
                form =
                    Form.addAboutLink model.form

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
            case model.common.loadedGlossaryItems of
                Ok glossaryItems ->
                    if Form.hasValidationErrors model.form then
                        ( { model
                            | triedToSaveWhenFormInvalid = True
                            , errorMessageWhileSaving = Nothing
                          }
                        , Cmd.none
                        )

                    else
                        let
                            common0 =
                                model.common

                            common1 =
                                { common0
                                    | title = model.form |> Form.titleField |> .body |> GlossaryTitle.fromString
                                    , aboutParagraph = model.form |> Form.aboutParagraphField |> .body
                                    , aboutLinks =
                                        model.form
                                            |> Form.aboutLinkFields
                                            |> Array.toList
                                            |> List.map
                                                (\( href, body ) ->
                                                    AboutLink.create href.href body.body
                                                )
                                }

                            model1 =
                                { model | common = common1 }
                        in
                        ( model1
                        , patchHtmlFile model1.common glossaryItems
                        )

                _ ->
                    ( model, Cmd.none )

        FailedToSave error ->
            ( { model | errorMessageWhileSaving = error |> Extras.Http.errorToHumanReadable |> Just }
            , Cmd.none
            )


patchHtmlFile : CommonModel -> GlossaryItems -> Cmd Msg
patchHtmlFile common glossaryItems =
    let
        msg =
            PageMsg.NavigateToListAll common
    in
    if common.enableSavingChangesInMemory then
        Extras.Task.messageToCommand msg

    else
        Http.request
            { method = "PATCH"
            , headers = []
            , url = "/"
            , body =
                glossaryItems
                    |> GlossaryItems.toHtmlTree
                        common.enableHelpForMakingChanges
                        (GlossaryTitle.toString common.title)
                        common.aboutParagraph
                        common.aboutLinks
                    |> HtmlTree.toHtml
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



-- VIEW


giveFocusToAboutLinkHref : AboutLinkIndex -> Cmd Msg
giveFocusToAboutLinkHref index =
    Task.attempt (always <| PageMsg.Internal NoOp) (Dom.focus <| ElementIds.aboutLinkHref index)


viewEditTitle : Bool -> Form.TitleField -> Html Msg
viewEditTitle showValidationErrors titleField =
    div []
        [ div []
            [ h2
                [ class "text-lg leading-6 font-medium text-gray-900 dark:text-gray-100" ]
                [ text "Title" ]
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
                                [ class "relative block w-full min-w-0" ]
                                [ Components.Form.inputText
                                    titleField.body
                                    showValidationErrors
                                    titleField.validationError
                                    [ required True
                                    , Html.Attributes.autocomplete False
                                    , Accessibility.Aria.label "Title"
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


viewEditAboutParagraph : Bool -> Form.AboutParagraphField -> Html Msg
viewEditAboutParagraph showValidationErrors aboutParagraphField =
    div []
        [ div []
            [ h2
                [ class "text-lg leading-6 font-medium text-gray-900 dark:text-gray-100" ]
                [ text "About" ]
            ]
        , div
            [ class "mt-4 max-w-prose" ]
            [ div
                [ class "sm:flex sm:flex-row sm:items-center" ]
                [ div
                    [ class "relative block min-w-0 w-full" ]
                    [ Components.Form.textarea
                        aboutParagraphField.body
                        showValidationErrors
                        aboutParagraphField.validationError
                        [ required True
                        , Accessibility.Aria.label "About"
                        , Accessibility.Aria.required True
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
            , Extras.Html.showIf (String.trim aboutParagraphField.body |> String.contains "\n") <|
                p
                    [ class "mt-2 text-red-800 dark:text-red-200" ]
                    [ text "This will be turned into a single paragraph ??? line breaks are automatically converted to spaces" ]
            ]
        ]


viewEditAboutLinks : Bool -> Array Form.AboutLinkField -> Html Msg
viewEditAboutLinks showValidationErrors aboutLinkFieldsArray =
    let
        aboutLinkFields =
            Array.toList aboutLinkFieldsArray
    in
    div
        [ class "space-y-5 sm:space-y-6" ]
        [ div []
            [ h2
                [ class "text-lg leading-6 font-medium text-gray-900 dark:text-gray-100" ]
                [ text "Links" ]
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
            [ text "Add link" ]
        ]


viewAddAboutLinkButton : Html Msg
viewAddAboutLinkButton =
    div []
        [ Components.Button.secondary
            [ Html.Events.onClick <| PageMsg.Internal AddAboutLink ]
            [ Icons.plus
                [ Svg.Attributes.class "mx-auto -ml-1 mr-2 h-5 w-5" ]
            , text "Add link"
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
                    [ Accessibility.Aria.label "Delete"
                    , Html.Events.onClick <| PageMsg.Internal <| DeleteAboutLink index
                    ]
                    [ Icons.trash
                        [ Svg.Attributes.class "h-5 w-5" ]
                    ]
                ]
            , div
                [ class "flex-1 isolate -space-y-px rounded-md shadow-sm bg-white dark:bg-gray-700" ]
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
                        [ text "URL" ]
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
                        class "relative border border-gray-300 dark:border-gray-500 rounded-md rounded-t-none px-3 py-2 focus-within:z-10 focus-within:ring-1 focus-within:ring-indigo-600 focus-within:border-indigo-600"

                      else
                        class "relative border border-red-300 dark:border-red-700 rounded-md rounded-t-none px-3 py-2 focus-within:z-10 focus-within:ring-1 focus-within:ring-red-500 focus-within:border-red-500"
                    ]
                    [ label
                        [ class "block text-sm font-semibold text-gray-500 dark:text-gray-300"
                        , for <| ElementIds.aboutLinkBody index
                        ]
                        [ text "Text" ]
                    , Html.div
                        [ class "block w-full min-w-0" ]
                        [ Html.div
                            [ class "block w-full min-w-0" ]
                            [ Components.Form.inputText
                                aboutLinkBody.body
                                showValidationErrors
                                aboutLinkBody.validationError
                                [ class "block w-full border-0 p-0 focus:ring-0 dark:bg-gray-700 dark:text-white"
                                , id <| ElementIds.aboutLinkBody index
                                , name <| ElementIds.aboutLinkBody index
                                , placeholder "Example"
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


viewCreateFormFooter : Model -> Bool -> Maybe String -> GlossaryItems -> TitleAndAboutForm -> Html Msg
viewCreateFormFooter model showValidationErrors errorMessageWhileSaving glossaryItems form =
    let
        errorDiv message =
            div
                [ class "flex justify-end mb-2" ]
                [ p
                    [ class "text-red-600 dark:text-red-400" ]
                    [ text message ]
                ]

        common =
            model.common
    in
    div
        [ class "pt-5 border-t dark:border-gray-700" ]
        [ errorDiv "There are errors on this form ??? see above."
            |> Extras.Html.showIf (showValidationErrors && Form.hasValidationErrors form)
        , errorMessageWhileSaving
            |> Extras.Html.showMaybe (\errorMessage -> errorDiv <| "Failed to save ??? " ++ errorMessage ++ ".")
        , Extras.Html.showIf model.common.enableSavingChangesInMemory <|
            div
                [ class "mt-5 sm:mt-4 mb-2 text-sm text-gray-500 dark:text-gray-400 sm:text-right" ]
                [ text Components.Copy.sandboxModeMessage ]
        , div
            [ class "flex justify-end" ]
            [ Components.Button.white True
                [ Html.Events.onClick <|
                    PageMsg.NavigateToListAll { common | loadedGlossaryItems = Ok glossaryItems }
                ]
                [ text "Cancel" ]
            , Components.Button.primary True
                [ class "ml-3"
                , Html.Events.onClick <| PageMsg.Internal Save
                ]
                [ text "Save" ]
            ]
        ]


view : Model -> Document Msg
view model =
    case model.common.loadedGlossaryItems of
        Ok glossaryItems ->
            { title = GlossaryTitle.toString model.common.title
            , body =
                [ div
                    [ class "container mx-auto px-6 pb-10 lg:px-8 max-w-4xl" ]
                    [ main_
                        []
                        [ h1
                            [ class "text-3xl font-bold leading-tight text-gray-900 dark:text-gray-100 print:text-black pt-6" ]
                            [ text "Edit Title and About Section"
                            ]
                        , form
                            [ class "pt-7" ]
                            [ div
                                [ class "space-y-7 sm:space-y-8" ]
                                [ viewEditTitle model.triedToSaveWhenFormInvalid <| Form.titleField model.form
                                , viewEditAboutParagraph model.triedToSaveWhenFormInvalid <| Form.aboutParagraphField model.form
                                , viewEditAboutLinks model.triedToSaveWhenFormInvalid <| Form.aboutLinkFields model.form
                                , viewCreateFormFooter model model.triedToSaveWhenFormInvalid model.errorMessageWhileSaving glossaryItems model.form
                                ]
                            ]
                        ]
                    ]
                ]
            }

        Err _ ->
            { title = "Glossary"
            , body = [ text "Something went wrong." ]
            }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
