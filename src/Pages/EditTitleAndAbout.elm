module Pages.EditTitleAndAbout exposing (Model, Msg, init, update, view)

import Accessibility exposing (..)
import Accessibility.Aria
import Array exposing (Array)
import Browser exposing (Document)
import Browser.Dom as Dom
import CommonModel exposing (CommonModel, OrderItemsBy(..))
import Components.Button
import Data.AboutLink as AboutLink
import Data.AboutLinkIndex as AboutLinkIndex exposing (AboutLinkIndex)
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Data.GlossaryTitle as GlossaryTitle
import ElementIds
import Extras.Html
import Extras.HtmlEvents
import Extras.HtmlTree as HtmlTree exposing (HtmlTree(..))
import Extras.Http
import Html
import Html.Attributes exposing (attribute, class, for, id, name, placeholder, required, spellcheck, type_, value)
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
                            PageMsg.NavigateToListAll common

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
                            [ div
                                [ class "relative block w-full min-w-0" ]
                                [ inputText titleField.body
                                    [ if not showValidationErrors || titleField.validationError == Nothing then
                                        class "w-full min-w-0 rounded-md focus:ring-indigo-500 focus:border-indigo-500 border-gray-300 dark:border-gray-500 dark:bg-gray-700 dark:text-white"

                                      else
                                        class "w-full min-w-0 rounded-md border-red-300 dark:bg-gray-700 text-red-900 dark:text-red-300 placeholder-red-300 dark:placeholder-red-700 focus:outline-none focus:ring-red-500 focus:border-red-500"
                                    , type_ "text"
                                    , required True
                                    , Html.Attributes.autocomplete False
                                    , Accessibility.Aria.label "Title"
                                    , Accessibility.Aria.required True
                                    , Accessibility.Aria.invalid <| titleField.validationError /= Nothing
                                    , Html.Events.onInput (PageMsg.Internal << UpdateTitle)
                                    , Extras.HtmlEvents.onEnter <| PageMsg.Internal NoOp
                                    ]
                                , Extras.Html.showIf (showValidationErrors && titleField.validationError /= Nothing) <|
                                    div [ class "absolute inset-y-0 right-0 pr-3 flex items-center pointer-events-none" ]
                                        [ Icons.exclamationCircle
                                            [ Svg.Attributes.class "h-5 w-5 text-red-500 dark:text-red-400" ]
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
                    [ div
                        [ class "grow-wrap"
                        , attribute "data-replicated-value" <| aboutParagraphField.body ++ "\n"
                        ]
                        [ textarea
                            [ if not showValidationErrors || aboutParagraphField.validationError == Nothing then
                                class "shadow-sm w-full rounded-md border border-gray-300 dark:border-gray-500 focus:ring-indigo-500 focus:border-indigo-500 dark:bg-gray-700 dark:text-white"

                              else
                                class "shadow-sm w-full rounded-md border-red-300 text-red-900 dark:text-red-300 placeholder-red-300 dark:placeholder-red-700 focus:outline-none focus:ring-red-500 focus:border-red-500 dark:bg-gray-700"
                            , required True
                            , Accessibility.Aria.label "About"
                            , Accessibility.Aria.required True
                            , Accessibility.Aria.invalid <| aboutParagraphField.validationError /= Nothing
                            , Html.Events.onInput (PageMsg.Internal << UpdateAboutParagraph)
                            ]
                            [ text aboutParagraphField.body ]
                        ]
                    , Extras.Html.showIf (showValidationErrors && aboutParagraphField.validationError /= Nothing) <|
                        div
                            [ class "absolute inset-y-0 right-0 pr-3 flex items-center pointer-events-none" ]
                            [ Icons.exclamationCircle
                                [ Svg.Attributes.class "h-5 w-5 text-red-500 dark:text-red-400" ]
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
                    [ text "This will be turned into a single paragraph — line breaks are automatically converted to spaces" ]
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
    button
        [ Html.Attributes.type_ "button"
        , class "relative block max-w-lg border-2 border-gray-300 border-dashed rounded-lg p-5 text-center hover:border-gray-400 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
        , Html.Events.onClick <| PageMsg.Internal AddAboutLink
        ]
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
                    , Html.input
                        [ class "block w-full border-0 p-0 focus:ring-0 dark:bg-gray-700 dark:text-white"
                        , if not showValidationErrors || aboutLinkBody.validationError == Nothing then
                            class "text-gray-900 placeholder-gray-500 dark:placeholder-gray-400"

                          else
                            class "text-red-900 dark:text-red-300 placeholder-red-300 dark:placeholder-orange-700"
                        , id <| ElementIds.aboutLinkHref index
                        , name <| ElementIds.aboutLinkHref index
                        , placeholder "https://example.com"
                        , type_ "url"
                        , spellcheck False
                        , value aboutLinkHref.href
                        , required True
                        , Html.Attributes.autocomplete False
                        , Accessibility.Aria.required True
                        , Accessibility.Aria.invalid <| aboutLinkHref.validationError /= Nothing
                        , Html.Events.onInput (PageMsg.Internal << UpdateAboutLinkHref index)
                        , Extras.HtmlEvents.onEnter <| PageMsg.Internal NoOp
                        ]
                        []
                    , Extras.Html.showIf (showValidationErrors && aboutLinkHref.validationError /= Nothing) <|
                        div [ class "absolute inset-y-0 right-0 pr-3 flex items-center pointer-events-none" ]
                            [ Icons.exclamationCircle
                                [ Svg.Attributes.class "h-5 w-5 text-red-500 dark:text-red-400" ]
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
                    , div []
                        [ inputText aboutLinkBody.body
                            [ class "block w-full border-0 p-0 focus:ring-0 dark:bg-gray-700 dark:text-white"
                            , if not showValidationErrors || aboutLinkBody.validationError == Nothing then
                                class "text-gray-900 placeholder-gray-500 dark:placeholder-gray-400"

                              else
                                class "text-red-900 dark:text-red-300 placeholder-red-300 dark:placeholder-orange-700"
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
                        , Extras.Html.showIf (showValidationErrors && aboutLinkBody.validationError /= Nothing) <|
                            div [ class "absolute inset-y-0 right-0 pr-3 flex items-center pointer-events-none" ]
                                [ Icons.exclamationCircle
                                    [ Svg.Attributes.class "h-5 w-5 text-red-500 dark:text-red-400" ]
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
        [ errorDiv "There are errors on this form — see above."
            |> Extras.Html.showIf (showValidationErrors && Form.hasValidationErrors form)
        , errorMessageWhileSaving
            |> Extras.Html.showMaybe (\errorMessage -> errorDiv <| "Failed to save — " ++ errorMessage ++ ".")
        , div
            [ class "flex justify-end" ]
            [ Components.Button.white
                [ Html.Events.onClick <|
                    PageMsg.NavigateToListAll { common | loadedGlossaryItems = Ok glossaryItems }
                ]
                [ text "Cancel" ]
            , Components.Button.primary
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
