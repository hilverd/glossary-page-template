module Pages.CreateOrEdit exposing (Model, Msg, init, update, view)

import Array exposing (Array)
import Browser.Dom as Dom
import Data.GlossaryItem as GlossaryItem exposing (GlossaryItem)
import Data.GlossaryItems as GlossaryItems
import Data.LoadedGlossaryItems exposing (LoadedGlossaryItems)
import Extras.Html
import Extras.HtmlAttribute
import Extras.HtmlTree as HtmlTree exposing (HtmlTree(..))
import Extras.Http
import Extras.Layout as Layout
import GlossaryItemForm as Form exposing (GlossaryItemForm)
import Html exposing (Html, button, div, form, h1, h3, input, option, p, select, span, text, textarea)
import Html.Attributes exposing (attribute, class, disabled, id, required, rows, selected, type_, value)
import Html.Events
import Http
import Icons
import Json.Decode as Decode
import PageMsg exposing (PageMsg)
import Set exposing (Set)
import Svg exposing (path, svg)
import Svg.Attributes exposing (d, fill, stroke, viewBox)
import Task



-- MODEL


type alias Model =
    { enableHelpForMakingChanges : Bool
    , maybeIndex : Maybe Int
    , glossaryItems : LoadedGlossaryItems
    , form : GlossaryItemForm
    , triedToSaveWhenFormInvalid : Bool
    , errorMessageWhileSaving : Maybe String
    }


type InternalMsg
    = NoOp
    | AddTerm
    | DeleteTerm Int
    | UpdateTerm Int String
    | ToggleAbbreviation Int
    | AddDetails
    | UpdateDetails Int String
    | DeleteDetails Int
    | AddRelatedTerm
    | SelectRelatedTerm Int String
    | DeleteRelatedTerm Int
    | Save
    | FailedToSave Http.Error


type alias Msg =
    PageMsg InternalMsg


init : Bool -> Maybe Int -> LoadedGlossaryItems -> ( Model, Cmd Msg )
init enableHelpForMakingChanges maybeIndex loadedGlossaryItems =
    ( { enableHelpForMakingChanges = enableHelpForMakingChanges
      , maybeIndex = maybeIndex
      , glossaryItems = loadedGlossaryItems
      , form =
            Maybe.map2
                (\index glossaryItems ->
                    glossaryItems
                        |> Array.fromList
                        |> Array.get index
                        |> Maybe.map Form.fromGlossaryItem
                        |> Maybe.withDefault Form.empty
                )
                maybeIndex
                (loadedGlossaryItems |> Result.toMaybe)
                |> Maybe.withDefault Form.empty
      , triedToSaveWhenFormInvalid = False
      , errorMessageWhileSaving = Nothing
      }
    , if maybeIndex == Nothing then
        giveFocusToTermInputField 0

      else
        Cmd.none
    )



-- UPDATE


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        AddTerm ->
            let
                form =
                    Form.addTerm model.form

                latestTermIndex =
                    Array.length form.terms - 1
            in
            ( { model | form = form }
            , giveFocusToTermInputField latestTermIndex
            )

        DeleteTerm termIndex ->
            ( { model | form = Form.deleteTerm termIndex model.form }, Cmd.none )

        UpdateTerm termIndex body ->
            ( { model | form = Form.updateTerm termIndex model.form body }, Cmd.none )

        ToggleAbbreviation termIndex ->
            ( { model | form = Form.toggleAbbreviation termIndex model.form }, Cmd.none )

        AddDetails ->
            let
                form =
                    Form.addDetails model.form

                latestDetailsIndex =
                    Array.length form.details - 1
            in
            ( { model | form = form }
            , giveFocusToDescriptionDetailsSingle latestDetailsIndex
            )

        UpdateDetails detailsIndex body ->
            ( { model | form = Form.updateDetails detailsIndex model.form body }, Cmd.none )

        DeleteDetails detailsIndex ->
            ( { model | form = Form.deleteDetails detailsIndex model.form }, Cmd.none )

        AddRelatedTerm ->
            ( { model | form = Form.addRelatedTerm model.form }, Cmd.none )

        SelectRelatedTerm relatedTermIndex selection ->
            let
                relatedTermIdReference =
                    if selection == "" then
                        Nothing

                    else
                        Just selection
            in
            ( { model | form = Form.selectRelatedTerm relatedTermIndex model.form relatedTermIdReference }, Cmd.none )

        DeleteRelatedTerm relatedTermIndex ->
            ( { model | form = Form.deleteRelatedTerm relatedTermIndex model.form }, Cmd.none )

        Save ->
            case model.glossaryItems of
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
                            newOrUpdatedGlossaryItem =
                                Form.toGlossaryItem glossaryItems model.form

                            updatedGlossaryItems : List GlossaryItem
                            updatedGlossaryItems =
                                (case model.maybeIndex of
                                    Just index ->
                                        glossaryItems
                                            |> Array.fromList
                                            |> Array.set index newOrUpdatedGlossaryItem
                                            |> Array.toList

                                    Nothing ->
                                        newOrUpdatedGlossaryItem :: glossaryItems
                                )
                                    |> GlossaryItems.sanitise
                                    |> List.sortBy (.terms >> List.head >> Maybe.map .body >> Maybe.withDefault "" >> String.toLower)
                        in
                        ( { model | glossaryItems = Ok updatedGlossaryItems }
                        , patchHtmlFile model.enableHelpForMakingChanges updatedGlossaryItems
                        )

                _ ->
                    ( model, Cmd.none )

        FailedToSave error ->
            ( { model | errorMessageWhileSaving = error |> Extras.Http.errorToHumanReadable |> Just }
            , Cmd.none
            )


patchHtmlFile : Bool -> List GlossaryItem -> Cmd Msg
patchHtmlFile enableHelpForMakingChanges glossaryItems =
    Http.request
        { method = "PATCH"
        , headers = []
        , url = "/"
        , body =
            glossaryItems
                |> GlossaryItems.toHtmlTree enableHelpForMakingChanges Layout.Cards
                |> HtmlTree.toHtml
                |> Http.stringBody "text/html"
        , expect =
            Http.expectWhatever
                (\result ->
                    case result of
                        Ok _ ->
                            PageMsg.NavigateToListAll True True <| Ok glossaryItems

                        Err error ->
                            PageMsg.Internal <| FailedToSave error
                )
        , timeout = Nothing
        , tracker = Nothing
        }



-- VIEW


giveFocusToTermInputField : Int -> Cmd Msg
giveFocusToTermInputField index =
    Task.attempt (\_ -> PageMsg.Internal NoOp) (Dom.focus <| idForTermInputField index)


giveFocusToDescriptionDetailsSingle : Int -> Cmd Msg
giveFocusToDescriptionDetailsSingle index =
    Task.attempt (\_ -> PageMsg.Internal NoOp) (Dom.focus <| idForDescriptionDetailsSingle index)


idForTermInputField : Int -> String
idForTermInputField index =
    "term-" ++ String.fromInt index


idForDescriptionDetailsSingle : Int -> String
idForDescriptionDetailsSingle index =
    "details-" ++ String.fromInt index


viewCreateDescriptionTerm : Bool -> Bool -> Int -> Form.Term -> Html Msg
viewCreateDescriptionTerm showValidationErrors canBeDeleted index term =
    let
        abbreviationLabelId =
            "term-" ++ String.fromInt index ++ "-abbreviation"
    in
    div []
        [ div
            [ class "sm:flex sm:flex-row sm:items-center" ]
            [ div
                [ class "flex-auto max-w-2xl flex" ]
                [ span
                    [ class "inline-flex items-center" ]
                    [ button
                        [ Html.Attributes.type_ "button"
                        , attribute "aria-label" "Delete"
                        , class "inline-flex items-center p-1.5 mr-2 border border-gray-300 dark:border-gray-500 shadow-sm rounded-full text-gray-700 dark:text-gray-300 bg-white dark:bg-gray-800"
                        , Extras.HtmlAttribute.showIf canBeDeleted <| class "hover:bg-gray-100 dark:hover:bg-gray-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                        , Extras.HtmlAttribute.showIf (not canBeDeleted) <| class "opacity-50"
                        , Html.Events.onClick <| PageMsg.Internal <| DeleteTerm index
                        , disabled <| not canBeDeleted
                        ]
                        [ Icons.trashSolid ]
                    ]
                , div
                    [ class "flex-auto" ]
                    [ div
                        [ class "sm:flex sm:flex-row sm:items-center" ]
                        [ div
                            [ class "relative block w-full min-w-0" ]
                            [ input
                                [ if not showValidationErrors || term.validationError == Nothing then
                                    class "w-full min-w-0 rounded-md focus:ring-indigo-500 focus:border-indigo-500 border-gray-300 dark:border-gray-500 dark:bg-gray-700 dark:text-white"

                                  else
                                    class "w-full min-w-0 rounded-md border-red-300 dark:bg-gray-700 text-red-900 dark:text-red-700 placeholder-red-300 focus:outline-none focus:ring-red-500 focus:border-red-500"
                                , type_ "text"
                                , id <| idForTermInputField index
                                , value term.body
                                , required True
                                , Html.Attributes.autocomplete False
                                , attribute "aria-required" "true"
                                , attribute "aria-invalid" "true" |> Extras.HtmlAttribute.showIf (term.validationError /= Nothing)
                                , Html.Events.onInput (PageMsg.Internal << UpdateTerm index)
                                ]
                                []
                            , Extras.Html.showIf (showValidationErrors && term.validationError /= Nothing) <|
                                div [ class "absolute inset-y-0 right-0 pr-3 flex items-center pointer-events-none" ]
                                    [ Icons.exclamationSolidRed ]
                            ]
                        , div
                            [ class "flex-auto mt-2 sm:mt-0 relative flex items-baseline" ]
                            [ div
                                [ class "sm:ml-5" ]
                                [ div
                                    [ class "flex items-center"
                                    , Html.Events.onClick <| PageMsg.Internal <| ToggleAbbreviation index
                                    ]
                                    [ button
                                        [ Html.Attributes.type_ "button"
                                        , class "relative inline-flex shrink-0 h-6 w-11 border-2 border-transparent rounded-full cursor-pointer transition-colors ease-in-out duration-200 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                                        , class <|
                                            if term.isAbbreviation then
                                                "bg-indigo-600"

                                            else
                                                "bg-gray-200 dark:bg-gray-400"
                                        , attribute "role" "switch"
                                        , attribute "aria-checked" "false"
                                        , attribute "aria-labelledby" abbreviationLabelId
                                        ]
                                        [ span
                                            [ attribute "aria-hidden" "true"
                                            , class "pointer-events-none inline-block h-5 w-5 rounded-full bg-white shadow transform ring-0 transition ease-in-out duration-200"
                                            , class <|
                                                if term.isAbbreviation then
                                                    "translate-x-5"

                                                else
                                                    "translate-x-0"
                                            ]
                                            []
                                        ]
                                    , span
                                        [ class "ml-3 select-none"
                                        , id abbreviationLabelId
                                        ]
                                        [ span
                                            [ class "font-medium text-gray-900 dark:text-gray-300" ]
                                            [ text "Abbreviation" ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        , Extras.Html.showMaybe
            (\validationError ->
                p
                    [ class "mt-2 text-red-600" ]
                    [ text validationError ]
            )
            (if showValidationErrors then
                term.validationError

             else
                Nothing
            )
        ]


viewCreateDescriptionTerms : Bool -> Array Form.Term -> Html Msg
viewCreateDescriptionTerms showValidationErrors termsArray =
    let
        terms =
            Array.toList termsArray
    in
    div []
        [ div []
            [ h3
                [ class "text-lg leading-6 font-medium text-gray-900 dark:text-gray-100" ]
                [ text "Description Terms" ]
            , p
                [ class "mt-1 max-w-2xl text-sm text-gray-500 dark:text-gray-400" ]
                [ text "List the group of terms being defined." ]
            ]
        , div
            [ class "mt-6 sm:mt-5 space-y-6 sm:space-y-5" ]
            (List.indexedMap (viewCreateDescriptionTerm showValidationErrors (List.length terms > 1)) terms
                ++ [ div []
                        [ button
                            [ Html.Attributes.type_ "button"
                            , class "inline-flex items-center px-4 py-2 border border-transparent shadow-sm font-medium rounded-md text-indigo-700 dark:text-indigo-300 bg-indigo-100 dark:bg-indigo-900 hover:bg-indigo-200 dark:hover:bg-indigo-800 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                            , Html.Events.onClick <| PageMsg.Internal AddTerm
                            ]
                            [ svg
                                [ Svg.Attributes.class "-ml-1 mr-2 h-5 w-5", viewBox "0 0 20 20", fill "currentColor" ]
                                [ path
                                    [ d "M10 3a1 1 0 011 1v5h5a1 1 0 110 2h-5v5a1 1 0 11-2 0v-5H4a1 1 0 110-2h5V4a1 1 0 011-1z" ]
                                    []
                                ]
                            , text "Add term"
                            ]
                        ]
                   ]
            )
        ]


viewCreateDescriptionDetailsSingle : Bool -> Int -> Form.Details -> Html Msg
viewCreateDescriptionDetailsSingle showValidationErrors index detailsSingle =
    div []
        [ div [ class "flex-auto max-w-2xl flex" ]
            [ span [ class "inline-flex items-center" ]
                [ button
                    [ Html.Attributes.type_ "button"
                    , attribute "aria-label" "Delete"
                    , class "inline-flex items-center p-1.5 mr-2 border border-gray-300 dark:border-gray-500 shadow-sm rounded-full text-gray-700 dark:text-gray-300 bg-white dark:bg-gray-800"
                    , class "hover:bg-gray-100 dark:hover:bg-gray-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                    , Html.Events.onClick <| PageMsg.Internal <| DeleteDetails index
                    ]
                    [ Icons.trashSolid ]
                ]
            , div [ class "relative block min-w-0 w-full" ]
                [ textarea
                    [ rows 3
                    , if not showValidationErrors || detailsSingle.validationError == Nothing then
                        class "shadow-sm w-full rounded-md focus:ring-indigo-500 focus:border-indigo-500 border border-gray-300 dark:border-gray-500 dark:bg-gray-700 dark:text-white"

                      else
                        class "shadow-sm w-full rounded-md border-red-300 text-red-900 placeholder-red-300 focus:outline-none focus:ring-red-500 focus:border-red-500 dark:bg-gray-700"
                    , required True
                    , attribute "aria-required" "true"
                    , id <| idForDescriptionDetailsSingle index
                    , Html.Events.onInput (PageMsg.Internal << UpdateDetails index)
                    ]
                    [ text detailsSingle.body ]
                , Extras.Html.showIf (showValidationErrors && detailsSingle.validationError /= Nothing) <|
                    div [ class "absolute inset-y-0 right-0 pr-3 flex items-center pointer-events-none" ]
                        [ Icons.exclamationSolidRed ]
                ]
            ]
        , Extras.Html.showMaybe
            (\validationError ->
                p
                    [ class "mt-2 text-red-600" ]
                    [ text validationError ]
            )
            (if showValidationErrors then
                detailsSingle.validationError

             else
                Nothing
            )
        ]


viewAddDetailsButton : Html Msg
viewAddDetailsButton =
    div []
        [ button
            [ Html.Attributes.type_ "button"
            , class "inline-flex items-center px-4 py-2 border border-transparent shadow-sm font-medium rounded-md text-indigo-700 dark:text-indigo-300 bg-indigo-100 dark:bg-indigo-900 hover:bg-indigo-200 dark:hover:bg-indigo-800 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
            , Html.Events.onClick <| PageMsg.Internal AddDetails
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
            , text "Add details"
            ]
        ]


viewAddDetailsButtonForEmptyState : Html Msg
viewAddDetailsButtonForEmptyState =
    button
        [ Html.Attributes.type_ "button"
        , class "relative block max-w-lg border-2 border-gray-300 border-dashed rounded-lg p-5 text-center hover:border-gray-400 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
        , Html.Events.onClick <| PageMsg.Internal AddDetails
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
            [ text "Add details" ]
        ]


viewCreateDescriptionDetails : Bool -> Array Form.Details -> Html Msg
viewCreateDescriptionDetails showValidationErrors detailsArray =
    let
        details =
            Array.toList detailsArray
    in
    div [ class "pt-8 space-y-6 sm:pt-10 sm:space-y-5" ]
        [ div []
            [ h3 [ class "text-lg leading-6 font-medium text-gray-900 dark:text-gray-100" ]
                [ text "Description Details" ]
            , p [ class "mt-1 max-w-2xl text-sm text-gray-500 dark:text-gray-400" ]
                [ text "Provide one or more definitions for this group of terms." ]
            ]
        , div [ class "space-y-6 sm:space-y-5" ]
            (List.indexedMap (viewCreateDescriptionDetailsSingle showValidationErrors) details
                ++ [ if List.isEmpty details then
                        viewAddDetailsButtonForEmptyState

                     else
                        viewAddDetailsButton
                   ]
            )
        ]


viewCreateSeeAlsoSingle : Bool -> Set String -> List GlossaryItem.Term -> Int -> Form.RelatedTerm -> Html Msg
viewCreateSeeAlsoSingle showValidationErrors relatedTermsIdReferences allTerms index relatedTerm =
    let
        pleaseSelectOption =
            option
                [ value "" ]
                [ text "--- Please select ---" ]
    in
    div
        []
        [ div
            [ class "sm:flex sm:flex-row sm:items-center" ]
            [ div
                [ class "flex-auto max-w-lg flex" ]
                [ span
                    [ class "inline-flex items-center" ]
                    [ button
                        [ Html.Attributes.type_ "button"
                        , attribute "aria-label" "Delete"
                        , class "inline-flex items-center p-1.5 mr-2 border border-gray-300 dark:border-gray-500 shadow-sm rounded-full text-gray-700 dark:text-gray-300 bg-white dark:bg-gray-800 hover:bg-gray-100 dark:hover:bg-gray-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                        , Html.Events.onClick <| PageMsg.Internal <| DeleteRelatedTerm index
                        ]
                        [ Icons.trashSolid ]
                    ]
                , div
                    [ class "flex-auto" ]
                    [ select
                        [ class "mt-1 block w-full pl-3 pr-10 py-2 dark:bg-gray-700 dark:text-gray-200 border-gray-300 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 rounded-md"
                        , Html.Events.on "change" <|
                            Decode.map (PageMsg.Internal << SelectRelatedTerm index) <|
                                Decode.andThen Decode.succeed Html.Events.targetValue
                        ]
                        ((if relatedTerm.idReference == Nothing then
                            [ pleaseSelectOption ]

                          else
                            []
                         )
                            ++ (allTerms
                                    |> List.filter
                                        (\term ->
                                            (not <| Set.member term.id relatedTermsIdReferences)
                                                || Just term.id
                                                == relatedTerm.idReference
                                        )
                                    |> List.map
                                        (\term ->
                                            option
                                                [ value term.id
                                                , selected <| Just term.id == relatedTerm.idReference
                                                ]
                                                [ text term.body ]
                                        )
                               )
                        )
                    ]
                ]
            ]
        , Extras.Html.showMaybe
            (\validationError ->
                p
                    [ class "mt-2 text-red-600" ]
                    [ text validationError ]
            )
            (if showValidationErrors then
                relatedTerm.validationError

             else
                Nothing
            )
        ]


viewAddRelatedTermButton : Html Msg
viewAddRelatedTermButton =
    button
        [ Html.Attributes.type_ "button"
        , class "inline-flex items-center px-4 py-2 border border-transparent shadow-sm font-medium rounded-md text-indigo-700 dark:text-indigo-300 bg-indigo-100 dark:bg-indigo-900 hover:bg-indigo-200 dark:hover:bg-indigo-800 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
        , Html.Events.onClick <| PageMsg.Internal AddRelatedTerm
        ]
        [ svg
            [ Svg.Attributes.class "-ml-1 mr-2 h-5 w-5", viewBox "0 0 20 20", fill "currentColor" ]
            [ path
                [ d "M10 3a1 1 0 011 1v5h5a1 1 0 110 2h-5v5a1 1 0 11-2 0v-5H4a1 1 0 110-2h5V4a1 1 0 011-1z" ]
                []
            ]
        , text "Add related term"
        ]


viewAddRelatedTermButtonForEmptyState : Html Msg
viewAddRelatedTermButtonForEmptyState =
    button
        [ Html.Attributes.type_ "button"
        , class "relative block max-w-lg border-2 border-gray-300 border-dashed rounded-lg p-5 text-center hover:border-gray-400 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
        , Html.Events.onClick <| PageMsg.Internal AddRelatedTerm
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
            [ text "Add related term" ]
        ]


viewCreateSeeAlso : Bool -> List GlossaryItem -> Array Form.Term -> Array Form.RelatedTerm -> Html Msg
viewCreateSeeAlso showValidationErrors glossaryItems terms relatedTermsArray =
    let
        termIdsSet =
            terms |> Array.toList |> List.map (.body >> Form.termBodyToId) |> Set.fromList

        relatedTermsList =
            Array.toList relatedTermsArray

        allTerms : List GlossaryItem.Term
        allTerms =
            List.filterMap (.terms >> List.head) glossaryItems
    in
    div [ class "pt-8 space-y-6 sm:pt-10 sm:space-y-5" ]
        [ div []
            [ h3
                [ class "text-lg leading-6 font-medium text-gray-900 dark:text-gray-100" ]
                [ text "See Also" ]
            , p
                [ class "mt-1 max-w-2xl text-sm text-gray-500 dark:text-gray-400" ]
                [ text "Point to any related terms the reader might want to look up." ]
            ]
        , div
            [ class "mt-6 sm:mt-5 space-y-6 sm:space-y-5" ]
            (List.indexedMap
                (viewCreateSeeAlsoSingle
                    showValidationErrors
                    (relatedTermsList
                        |> List.filterMap .idReference
                        |> Set.fromList
                    )
                    (List.filter (\term -> not <| Set.member term.id termIdsSet) allTerms)
                )
                relatedTermsList
            )
        , div
            []
            [ if List.isEmpty relatedTermsList then
                viewAddRelatedTermButtonForEmptyState

              else
                viewAddRelatedTermButton
            ]
        ]


viewCreateFormFooter : Bool -> Maybe String -> List GlossaryItem -> GlossaryItemForm -> Html Msg
viewCreateFormFooter showValidationErrors errorMessageWhileSaving glossaryItems form =
    let
        errorDiv message =
            div
                [ class "flex justify-end mb-2" ]
                [ p
                    [ class "text-red-600" ]
                    [ text message ]
                ]
    in
    div
        [ class "pt-5" ]
        [ errorDiv "There are errors on this form — see above."
            |> Extras.Html.showIf (showValidationErrors && Form.hasValidationErrors form)
        , errorMessageWhileSaving
            |> Extras.Html.showMaybe (\errorMessage -> errorDiv <| "Failed to save — " ++ errorMessage ++ ".")
        , div
            [ class "flex justify-end" ]
            [ button
                [ Html.Attributes.type_ "button"
                , class "bg-white dark:bg-gray-700 py-2 px-4 border border-gray-300 dark:border-gray-700 rounded-md shadow-sm font-medium text-gray-700 dark:text-gray-300 hover:bg-gray-50 dark:hover:bg-gray-900 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                , Html.Events.onClick <| PageMsg.NavigateToListAll True True <| Ok glossaryItems
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


view : Model -> Html Msg
view model =
    case model.glossaryItems of
        Ok glossaryItems ->
            div
                []
                [ h1
                    [ class "text-3xl font-bold leading-tight text-gray-900 dark:text-gray-100 print:text-black pt-6" ]
                    [ text <|
                        if model.maybeIndex == Nothing then
                            "Create a New Glossary Item"

                        else
                            "Edit Glossary Item"
                    ]
                , form
                    [ class "pt-7" ]
                    [ div
                        [ class "space-y-8 divide-y divide-gray-200 sm:space-y-5" ]
                        [ viewCreateDescriptionTerms model.triedToSaveWhenFormInvalid model.form.terms
                        , viewCreateDescriptionDetails model.triedToSaveWhenFormInvalid model.form.details
                        , viewCreateSeeAlso model.triedToSaveWhenFormInvalid glossaryItems model.form.terms model.form.relatedTerms
                        , viewCreateFormFooter model.triedToSaveWhenFormInvalid model.errorMessageWhileSaving glossaryItems model.form
                        ]
                    ]
                ]

        Err _ ->
            text "Something went wrong."
