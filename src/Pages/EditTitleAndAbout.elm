module Pages.EditTitleAndAbout exposing (..)

import Array exposing (Array)
import Browser exposing (Document)
import Browser.Dom as Dom
import CommonModel exposing (CommonModel, OrderItemsBy(..))
import Data.AboutLink as AboutLink exposing (AboutLink)
import Data.AboutLinkIndex as AboutLinkIndex exposing (AboutLinkIndex)
import Data.DetailsIndex as DetailsIndex exposing (DetailsIndex)
import Data.GlossaryItem as GlossaryItem
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Data.RelatedTermIndex as RelatedTermIndex exposing (RelatedTermIndex)
import Data.TermIndex as TermIndex exposing (TermIndex)
import ElementIds
import Extras.Html
import Extras.HtmlAttribute
import Extras.HtmlTree as HtmlTree exposing (HtmlTree(..))
import Extras.Http
import Html exposing (Html, a, button, div, form, h1, h3, input, label, option, p, select, span, text, textarea)
import Html.Attributes exposing (attribute, class, disabled, for, id, name, placeholder, required, selected, spellcheck, type_, value)
import Html.Events
import Http
import Icons
import PageMsg exposing (PageMsg)
import Set exposing (Set)
import Svg exposing (path, svg)
import Svg.Attributes exposing (d, fill, stroke, viewBox)
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
                                    | title = model.form |> Form.titleField |> .body
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
                    common.title
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
            [ h3
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
                                [ input
                                    [ if not showValidationErrors || titleField.validationError == Nothing then
                                        class "w-full min-w-0 rounded-md focus:ring-indigo-500 focus:border-indigo-500 border-gray-300 dark:border-gray-500 dark:bg-gray-700 dark:text-white"

                                      else
                                        class "w-full min-w-0 rounded-md border-red-300 dark:bg-gray-700 text-red-900 dark:text-red-300 placeholder-red-300 dark:placeholder-red-700 focus:outline-none focus:ring-red-500 focus:border-red-500"
                                    , type_ "text"
                                    , value titleField.body
                                    , required True
                                    , Html.Attributes.autocomplete False
                                    , attribute "aria-required" "true"
                                    , attribute "aria-invalid" "true" |> Extras.HtmlAttribute.showIf (titleField.validationError /= Nothing)
                                    , Html.Events.onInput (PageMsg.Internal << UpdateTitle)
                                    ]
                                    []
                                , Extras.Html.showIf (showValidationErrors && titleField.validationError /= Nothing) <|
                                    div [ class "absolute inset-y-0 right-0 pr-3 flex items-center pointer-events-none" ]
                                        [ Icons.exclamationSolidRed ]
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
            [ h3
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
                            , attribute "aria-required" "true"
                            , attribute "aria-invalid" "true" |> Extras.HtmlAttribute.showIf (aboutParagraphField.validationError /= Nothing)
                            , Html.Events.onInput (PageMsg.Internal << UpdateAboutParagraph)
                            ]
                            [ text aboutParagraphField.body ]
                        ]
                    , Extras.Html.showIf (showValidationErrors && aboutParagraphField.validationError /= Nothing) <|
                        div
                            [ class "absolute inset-y-0 right-0 pr-3 flex items-center pointer-events-none" ]
                            [ Icons.exclamationSolidRed ]
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
        [ class "space-y-6 sm:space-y-5" ]
        [ div []
            [ h3
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
        [ svg
            [ Svg.Attributes.class "mx-auto h-12 w-12 text-gray-400"
            , stroke "none"
            , fill "currentColor"
            , viewBox "0 0 20 20"
            ]
            [ path
                [ d "M10 3a1 1 0 011 1v5h5a1 1 0 110 2h-5v5a1 1 0 11-2 0v-5H4a1 1 0 110-2h5V4a1 1 0 011-1z" ]
                []
            ]
        , span
            [ class "mt-2 block font-medium text-gray-900 dark:text-gray-200" ]
            [ text "Add link" ]
        ]


viewAddAboutLinkButton : Html Msg
viewAddAboutLinkButton =
    div []
        [ button
            [ Html.Attributes.type_ "button"
            , class "inline-flex items-center px-4 py-2 border border-transparent shadow-sm font-medium rounded-md text-indigo-700 dark:text-indigo-300 bg-indigo-100 dark:bg-indigo-900 hover:bg-indigo-200 dark:hover:bg-indigo-800 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
            , Html.Events.onClick <| PageMsg.Internal AddAboutLink
            ]
            [ svg
                [ Svg.Attributes.class "-ml-1 mr-2 h-5 w-5"
                , viewBox "0 0 20 20"
                , fill "currentColor"
                ]
                [ path
                    [ d "M10 3a1 1 0 011 1v5h5a1 1 0 110 2h-5v5a1 1 0 11-2 0v-5H4a1 1 0 110-2h5V4a1 1 0 011-1z" ]
                    []
                ]
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
                [ button
                    [ Html.Attributes.type_ "button"
                    , attribute "aria-label" "Delete"
                    , class "inline-flex items-center p-1.5 mr-2 border border-gray-300 dark:border-gray-500 shadow-sm rounded-full text-gray-700 dark:text-gray-300 bg-white dark:bg-gray-800"
                    , class "hover:bg-gray-100 dark:hover:bg-gray-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                    , Html.Events.onClick <| PageMsg.Internal <| DeleteAboutLink index
                    ]
                    [ Icons.trashSolid ]
                ]
            , div
                [ class "flex-1 isolate -space-y-px rounded-md shadow-sm bg-white dark:bg-gray-700" ]
                [ div
                    [ if not showValidationErrors || aboutLinkHref.validationError == Nothing then
                        class "relative border border-gray-300 rounded-md rounded-b-none px-3 py-2 focus-within:z-10 focus-within:ring-1 focus-within:ring-indigo-600 focus-within:border-indigo-600"

                      else
                        class "relative border border-red-300 dark:border-red-700 rounded-md rounded-b-none px-3 py-2 focus-within:z-10 focus-within:ring-1 focus-within:ring-red-500 focus-within:border-red-500"
                    ]
                    [ label
                        [ class "block text-xs font-semibold text-gray-900 dark:text-gray-100"
                        , for <| ElementIds.aboutLinkHref index
                        ]
                        [ text "URL" ]
                    , input
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
                        , attribute "aria-required" "true"
                        , attribute "aria-invalid" "true" |> Extras.HtmlAttribute.showIf (aboutLinkHref.validationError /= Nothing)
                        , Html.Events.onInput (PageMsg.Internal << UpdateAboutLinkHref index)
                        ]
                        []
                    , Extras.Html.showIf (showValidationErrors && aboutLinkHref.validationError /= Nothing) <|
                        div [ class "absolute inset-y-0 right-0 pr-3 flex items-center pointer-events-none" ]
                            [ Icons.exclamationSolidRed ]
                    ]
                , div
                    [ if not showValidationErrors || aboutLinkBody.validationError == Nothing then
                        class "relative border border-gray-300 rounded-md rounded-t-none px-3 py-2 focus-within:z-10 focus-within:ring-1 focus-within:ring-indigo-600 focus-within:border-indigo-600"

                      else
                        class "relative border border-red-300 dark:border-red-700 rounded-md rounded-t-none px-3 py-2 focus-within:z-10 focus-within:ring-1 focus-within:ring-red-500 focus-within:border-red-500"
                    ]
                    [ label
                        [ class "block text-xs font-semibold text-gray-900 dark:text-gray-100"
                        , for <| ElementIds.aboutLinkBody index
                        ]
                        [ text "Text" ]
                    , div []
                        [ input
                            [ class "block w-full border-0 p-0 focus:ring-0 dark:bg-gray-700 dark:text-white"
                            , if not showValidationErrors || aboutLinkBody.validationError == Nothing then
                                class "text-gray-900 placeholder-gray-500 dark:placeholder-gray-400"

                              else
                                class "text-red-900 dark:text-red-300 placeholder-red-300 dark:placeholder-orange-700"
                            , id <| ElementIds.aboutLinkBody index
                            , name <| ElementIds.aboutLinkBody index
                            , placeholder "Example"
                            , type_ "text"
                            , value aboutLinkBody.body
                            , required True
                            , Html.Attributes.autocomplete False
                            , attribute "aria-required" "true"
                            , attribute "aria-invalid" "true" |> Extras.HtmlAttribute.showIf (aboutLinkHref.validationError /= Nothing)
                            , Html.Events.onInput (PageMsg.Internal << UpdateAboutLinkBody index)
                            ]
                            []
                        , Extras.Html.showIf (showValidationErrors && aboutLinkBody.validationError /= Nothing) <|
                            div [ class "absolute inset-y-0 right-0 pr-3 flex items-center pointer-events-none" ]
                                [ Icons.exclamationSolidRed ]
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
            [ button
                [ Html.Attributes.type_ "button"
                , class "bg-white dark:bg-gray-700 py-2 px-4 border border-gray-300 dark:border-gray-700 rounded-md shadow-sm font-medium text-gray-700 dark:text-gray-300 hover:bg-gray-50 dark:hover:bg-gray-900 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                , Html.Events.onClick <|
                    PageMsg.NavigateToListAll { common | loadedGlossaryItems = Ok glossaryItems }
                ]
                [ text "Cancel" ]
            , button
                [ Html.Attributes.type_ "button"
                , class "ml-3 inline-flex justify-center py-2 px-4 border border-transparent shadow-sm font-medium rounded-md text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                , Html.Events.onClick <| PageMsg.Internal Save
                ]
                [ text "Save" ]
            ]
        ]


view : Model -> Document Msg
view model =
    case model.common.loadedGlossaryItems of
        Ok glossaryItems ->
            { title = model.common.title
            , body =
                [ div
                    [ class "container mx-auto px-6 pb-10 lg:px-8 max-w-4xl" ]
                    [ Html.main_
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
