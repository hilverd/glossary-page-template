port module Pages.CreateOrEdit exposing (InternalMsg, Model, Msg, init, subscriptions, update, view)

import Accessibility exposing (Html, article, div, dl, form, h1, h2, p, span, text)
import Accessibility.Aria
import Array exposing (Array)
import Browser exposing (Document)
import Browser.Dom as Dom
import CommonModel exposing (CommonModel, OrderItemsBy(..))
import Components.Button
import Components.Copy
import Components.Form
import Components.GlossaryItemCard
import Components.SelectMenu
import Data.DetailsIndex as DetailsIndex exposing (DetailsIndex)
import Data.Glossary as Glossary
import Data.GlossaryItem exposing (GlossaryItem)
import Data.GlossaryItem.RelatedTerm as RelatedTerm
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Data.GlossaryTitle as GlossaryTitle
import Data.RelatedTermIndex as RelatedTermIndex exposing (RelatedTermIndex)
import Data.TermIndex as TermIndex exposing (TermIndex)
import ElementIds
import Extras.Html
import Extras.HtmlEvents
import Extras.HtmlTree as HtmlTree
import Extras.Http
import Extras.Task
import GlossaryItemForm as Form exposing (GlossaryItemForm)
import GlossaryItemForm.DetailsField as DetailsField exposing (DetailsField)
import GlossaryItemForm.TermField as TermField exposing (TermField)
import Html
import Html.Attributes exposing (class, id, required, style)
import Html.Events
import Http
import Icons
import PageMsg exposing (PageMsg)
import Set exposing (Set)
import Svg.Attributes
import Task



-- MODEL


type alias Model =
    { common : CommonModel
    , form : GlossaryItemForm
    , triedToSaveWhenFormInvalid : Bool
    , errorMessageWhileSaving : Maybe String
    }


type InternalMsg
    = NoOp
    | AddTerm
    | DeleteTerm TermIndex
    | UpdateTerm TermIndex String
    | ToggleAbbreviation TermIndex
    | AddDetails
    | UpdateDetails DetailsIndex String
    | DeleteDetails DetailsIndex
    | AddRelatedTerm (Maybe String)
    | SelectRelatedTerm RelatedTermIndex String
    | DeleteRelatedTerm RelatedTermIndex
    | ToggleNeedsUpdating
    | Save
    | ReceiveCurrentDateTimeForSaving String
    | FailedToSave Http.Error


type alias Msg =
    PageMsg InternalMsg


init : CommonModel -> ( Model, Cmd Msg )
init commonModel =
    case commonModel.glossary of
        Ok { items } ->
            let
                existingTerms : List Term
                existingTerms =
                    GlossaryItems.terms items

                existingPrimaryTerms : List Term
                existingPrimaryTerms =
                    GlossaryItems.primaryTerms items

                itemsListingThisItemAsRelated : List GlossaryItem
                itemsListingThisItemAsRelated =
                    commonModel.maybeIndex
                        |> Maybe.andThen
                            (\index ->
                                items
                                    |> GlossaryItems.get index
                                    |> Maybe.map
                                        (\currentItem ->
                                            GlossaryItems.orderedAlphabetically items
                                                |> List.map Tuple.second
                                                |> List.filter
                                                    (\item ->
                                                        item.relatedTerms
                                                            |> List.any
                                                                (\relatedTerm ->
                                                                    currentItem.terms
                                                                        |> List.any (\term -> Term.id term == RelatedTerm.idReference relatedTerm)
                                                                )
                                                    )
                                        )
                            )
                        |> Maybe.withDefault []

                emptyForm : GlossaryItemForm
                emptyForm =
                    Form.empty existingTerms existingPrimaryTerms itemsListingThisItemAsRelated
            in
            ( { common = commonModel
              , form =
                    Maybe.map
                        (\index ->
                            items
                                |> GlossaryItems.get index
                                |> Maybe.map
                                    (Form.fromGlossaryItem
                                        existingTerms
                                        existingPrimaryTerms
                                        itemsListingThisItemAsRelated
                                    )
                                |> Maybe.withDefault emptyForm
                        )
                        commonModel.maybeIndex
                        |> Maybe.withDefault emptyForm
              , triedToSaveWhenFormInvalid = False
              , errorMessageWhileSaving = Nothing
              }
            , if commonModel.maybeIndex == Nothing then
                0 |> TermIndex.fromInt |> giveFocusToTermInputField

              else
                Cmd.none
            )

        Err _ ->
            ( { common = commonModel
              , form = Form.empty [] [] []
              , triedToSaveWhenFormInvalid = False
              , errorMessageWhileSaving = Nothing
              }
            , Cmd.none
            )



-- PORTS


port getCurrentDateTimeForSaving : () -> Cmd msg


port receiveCurrentDateTimeForSaving : (String -> msg) -> Sub msg



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
                form : GlossaryItemForm
                form =
                    Form.addDetails model.form

                latestDetailsIndex : DetailsIndex
                latestDetailsIndex =
                    Array.length (Form.detailsFields form) - 1 |> DetailsIndex.fromInt
            in
            ( { model | form = form }
            , giveFocusToDescriptionDetailsSingle latestDetailsIndex
            )

        UpdateDetails detailsIndex body ->
            ( { model | form = Form.updateDetails detailsIndex model.form body }, Cmd.none )

        DeleteDetails detailsIndex ->
            ( { model | form = Form.deleteDetails detailsIndex model.form }, Cmd.none )

        AddRelatedTerm maybeTermId ->
            let
                form : GlossaryItemForm
                form =
                    Form.addRelatedTerm maybeTermId model.form

                latestRelatedTermIndex : RelatedTermIndex
                latestRelatedTermIndex =
                    Array.length (Form.relatedTermFields form) - 1 |> RelatedTermIndex.fromInt
            in
            ( { model | form = form }
            , giveFocusToSeeAlsoSelect latestRelatedTermIndex
            )

        SelectRelatedTerm relatedTermIndex selection ->
            let
                relatedTermIdReference : Maybe String
                relatedTermIdReference =
                    if selection == "" then
                        Nothing

                    else
                        Just selection
            in
            ( { model | form = Form.selectRelatedTerm relatedTermIndex model.form relatedTermIdReference }, Cmd.none )

        DeleteRelatedTerm relatedTermIndex ->
            ( { model | form = Form.deleteRelatedTerm relatedTermIndex model.form }, Cmd.none )

        ToggleNeedsUpdating ->
            ( { model | form = Form.toggleNeedsUpdating model.form }, Cmd.none )

        Save ->
            ( model, getCurrentDateTimeForSaving () )

        ReceiveCurrentDateTimeForSaving dateTime ->
            case model.common.glossary of
                Ok glossary ->
                    if Form.hasValidationErrors model.form then
                        ( { model
                            | triedToSaveWhenFormInvalid = True
                            , errorMessageWhileSaving = Nothing
                          }
                        , Cmd.none
                        )

                    else
                        let
                            newOrUpdatedGlossaryItem : GlossaryItem
                            newOrUpdatedGlossaryItem =
                                Form.toGlossaryItem glossary.enableMarkdownBasedSyntax glossary.items model.form <| Just dateTime

                            common : CommonModel
                            common =
                                model.common

                            ( updatedGlossaryItems, maybeIndex ) =
                                case common.maybeIndex of
                                    Just index ->
                                        ( GlossaryItems.update index newOrUpdatedGlossaryItem glossary.items
                                        , Just index
                                        )

                                    Nothing ->
                                        let
                                            updated : GlossaryItems
                                            updated =
                                                GlossaryItems.insert newOrUpdatedGlossaryItem glossary.items
                                        in
                                        ( updated
                                        , -- Find index of newly inserted item
                                          updated
                                            |> (case common.orderItemsBy of
                                                    Alphabetically ->
                                                        GlossaryItems.orderedAlphabetically

                                                    MostMentionedFirst ->
                                                        GlossaryItems.orderedByMostMentionedFirst
                                               )
                                            |> List.filter (Tuple.second >> (==) newOrUpdatedGlossaryItem)
                                            |> List.head
                                            |> Maybe.map Tuple.first
                                        )

                            model_ : Model
                            model_ =
                                { model
                                    | common =
                                        { common
                                            | glossary = Ok <| { glossary | items = updatedGlossaryItems }
                                            , maybeIndex = maybeIndex
                                        }
                                }
                        in
                        ( model_
                        , patchHtmlFile model_.common updatedGlossaryItems
                        )

                _ ->
                    ( model, Cmd.none )

        FailedToSave error ->
            ( { model | errorMessageWhileSaving = error |> Extras.Http.errorToHumanReadable |> Just }
            , Cmd.none
            )


patchHtmlFile : CommonModel -> GlossaryItems -> Cmd Msg
patchHtmlFile common glossaryItems =
    case common.glossary of
        Ok glossary ->
            let
                msg : PageMsg InternalMsg
                msg =
                    PageMsg.NavigateToListAll { common | glossary = Ok { glossary | items = glossaryItems } }
            in
            if common.enableSavingChangesInMemory then
                Extras.Task.messageToCommand msg

            else
                Http.request
                    { method = "PATCH"
                    , headers = []
                    , url = "/"
                    , body =
                        { glossary | items = glossaryItems }
                            |> Glossary.toHtmlTree common.enableExportMenu common.enableHelpForMakingChanges
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



-- VIEW


giveFocusToTermInputField : TermIndex -> Cmd Msg
giveFocusToTermInputField termIndex =
    Task.attempt (always <| PageMsg.Internal NoOp) (Dom.focus <| ElementIds.termInputField termIndex)


giveFocusToDescriptionDetailsSingle : DetailsIndex -> Cmd Msg
giveFocusToDescriptionDetailsSingle index =
    Task.attempt (always <| PageMsg.Internal NoOp) (Dom.focus <| ElementIds.descriptionDetailsSingle index)


giveFocusToSeeAlsoSelect : RelatedTermIndex -> Cmd Msg
giveFocusToSeeAlsoSelect index =
    Task.attempt (always <| PageMsg.Internal NoOp) (Dom.focus <| ElementIds.seeAlsoSelect index)


viewCreateDescriptionTerm : Bool -> Bool -> Bool -> Bool -> Int -> TermField -> Html Msg
viewCreateDescriptionTerm showMarkdownBasedSyntaxEnabled mathSupportEnabled showValidationErrors canBeDeleted index term =
    viewCreateDescriptionTermInternal
        (showMarkdownBasedSyntaxEnabled && index == 0)
        mathSupportEnabled
        showValidationErrors
        canBeDeleted
        (TermIndex.fromInt index)
        term


viewCreateDescriptionTermInternal : Bool -> Bool -> Bool -> Bool -> TermIndex -> TermField -> Html Msg
viewCreateDescriptionTermInternal showMarkdownBasedSyntaxEnabled mathSupportEnabled showValidationErrors canBeDeleted termIndex termField =
    let
        abbreviationLabelId : String
        abbreviationLabelId =
            ElementIds.abbreviationLabel termIndex
    in
    div []
        [ div
            [ class "sm:flex sm:flex-row sm:items-center" ]
            [ div
                [ class "flex-auto max-w-2xl flex" ]
                [ span
                    [ class "inline-flex items-center" ]
                    [ Components.Button.rounded canBeDeleted
                        [ Accessibility.Aria.label "Delete"
                        , Html.Events.onClick <| PageMsg.Internal <| DeleteTerm termIndex
                        ]
                        [ Icons.trash
                            [ Svg.Attributes.class "h-5 w-5" ]
                        ]
                    ]
                , div
                    [ class "flex-auto" ]
                    [ div
                        [ class "sm:flex sm:flex-row sm:items-center" ]
                        [ Html.div
                            [ class "relative block w-full min-w-0" ]
                            [ Components.Form.inputText
                                (TermField.raw termField)
                                showMarkdownBasedSyntaxEnabled
                                mathSupportEnabled
                                showValidationErrors
                                (TermField.validationError termField)
                                [ id <| ElementIds.termInputField termIndex
                                , required True
                                , Html.Attributes.autocomplete False
                                , Accessibility.Aria.label "Term"
                                , Accessibility.Aria.required True
                                , Html.Events.onInput (PageMsg.Internal << UpdateTerm termIndex)
                                , Extras.HtmlEvents.onEnter <| PageMsg.Internal NoOp
                                ]
                            ]
                        , div
                            [ class "flex-auto mt-2 sm:mt-0 relative flex items-baseline" ]
                            [ div
                                [ class "sm:ml-5" ]
                                [ Components.Button.toggle
                                    (TermField.isAbbreviation termField)
                                    abbreviationLabelId
                                    [ Html.Events.onClick <| PageMsg.Internal <| ToggleAbbreviation termIndex ]
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


viewCreateDescriptionTerms : Bool -> Bool -> Bool -> Array TermField -> Html Msg
viewCreateDescriptionTerms markdownBasedSyntaxEnabled mathSupportEnabled showValidationErrors termsArray =
    let
        terms : List TermField
        terms =
            Array.toList termsArray
    in
    div []
        [ div []
            [ h2
                [ class "text-lg leading-6 font-medium text-gray-900 dark:text-gray-100" ]
                [ text "Description Terms" ]
            , p
                [ class "mt-1 max-w-2xl text-sm text-gray-500 dark:text-gray-400" ]
                [ text "List the group of terms being defined." ]
            ]
        , div
            [ class "mt-6 sm:mt-5 space-y-6 sm:space-y-5" ]
            (List.indexedMap (viewCreateDescriptionTerm markdownBasedSyntaxEnabled mathSupportEnabled showValidationErrors (List.length terms > 1)) terms
                ++ [ div []
                        [ Components.Button.secondary
                            [ Html.Events.onClick <| PageMsg.Internal AddTerm ]
                            [ Icons.plus [ Svg.Attributes.class "mx-auto -ml-1 mr-2 h-5 w-5" ]
                            , text "Add term"
                            ]
                        ]
                   ]
            )
        ]


viewCreateDescriptionDetailsSingle : Bool -> Bool -> Bool -> Bool -> Int -> DetailsField -> Html Msg
viewCreateDescriptionDetailsSingle showNewlineWarnings markdownBasedSyntaxEnabled mathSupportEnabled showValidationErrors index detailsSingle =
    viewCreateDescriptionDetailsSingle1 showNewlineWarnings markdownBasedSyntaxEnabled mathSupportEnabled showValidationErrors (DetailsIndex.fromInt index) detailsSingle


viewCreateDescriptionDetailsSingle1 : Bool -> Bool -> Bool -> Bool -> DetailsIndex -> DetailsField -> Html Msg
viewCreateDescriptionDetailsSingle1 showNewlineWarnings markdownBasedSyntaxEnabled mathSupportEnabled showValidationErrors index detailsSingle =
    let
        raw : String
        raw =
            DetailsField.raw detailsSingle

        validationError : Maybe String
        validationError =
            DetailsField.validationError detailsSingle
    in
    div []
        [ div
            [ class "flex-auto max-w-3xl flex" ]
            [ span [ class "inline-flex items-center" ]
                [ Components.Button.rounded True
                    [ Accessibility.Aria.label "Delete"
                    , Html.Events.onClick <| PageMsg.Internal <| DeleteDetails index
                    ]
                    [ Icons.trash
                        [ Svg.Attributes.class "h-5 w-5" ]
                    ]
                ]
            , div
                [ class "relative block min-w-0 w-full" ]
                [ Components.Form.textarea
                    raw
                    (markdownBasedSyntaxEnabled && DetailsIndex.toInt index == 0)
                    mathSupportEnabled
                    showValidationErrors
                    validationError
                    [ required True
                    , Accessibility.Aria.label "Details"
                    , Accessibility.Aria.required True
                    , id <| ElementIds.descriptionDetailsSingle index
                    , Html.Events.onInput (PageMsg.Internal << UpdateDetails index)
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
        , Extras.Html.showIf (showNewlineWarnings && (raw |> String.contains "\n")) <|
            p
                [ class "mt-2 text-red-800 dark:text-red-200" ]
                [ text "This will be turned into a single paragraph — line breaks are automatically converted to spaces" ]
        ]


viewAddDetailsButton : Html Msg
viewAddDetailsButton =
    div []
        [ Components.Button.secondary
            [ Html.Events.onClick <| PageMsg.Internal AddDetails ]
            [ Icons.plus [ Svg.Attributes.class "mx-auto -ml-1 mr-2 h-5 w-5" ]
            , text "Add details"
            ]
        ]


viewAddDetailsButtonForEmptyState : Html Msg
viewAddDetailsButtonForEmptyState =
    Components.Button.emptyState
        [ Html.Events.onClick <| PageMsg.Internal AddDetails ]
        [ Icons.plus [ Svg.Attributes.class "mx-auto h-12 w-12 text-gray-400" ]
        , span
            [ class "mt-2 block font-medium text-gray-900 dark:text-gray-200" ]
            [ text "Add details" ]
        ]


viewCreateDescriptionDetails : Bool -> Bool -> Bool -> Bool -> Array DetailsField -> Html Msg
viewCreateDescriptionDetails showNewlineWarnings markdownBasedSyntaxEnabled mathSupportEnabled showValidationErrors detailsArray =
    let
        details : List DetailsField
        details =
            Array.toList detailsArray
    in
    div
        [ class "pt-8 space-y-6 sm:pt-10 sm:space-y-5" ]
        [ div []
            [ h2
                [ class "text-lg leading-6 font-medium text-gray-900 dark:text-gray-100" ]
                [ text "Description Details" ]
            , p
                [ class "mt-1 max-w-2xl text-sm text-gray-500 dark:text-gray-400" ]
                [ text "Provide one or more definitions for this group of terms." ]
            ]
        , div
            [ class "space-y-6 sm:space-y-5" ]
            (List.indexedMap (viewCreateDescriptionDetailsSingle showNewlineWarnings markdownBasedSyntaxEnabled mathSupportEnabled showValidationErrors) details
                ++ [ if List.isEmpty details then
                        viewAddDetailsButtonForEmptyState

                     else
                        viewAddDetailsButton
                   ]
            )
        ]


viewCreateSeeAlsoSingle : Bool -> Set String -> List Term -> Int -> Form.RelatedTermField -> Html Msg
viewCreateSeeAlsoSingle showValidationErrors relatedTermsIdReferences allTerms index relatedTerm =
    viewCreateSeeAlsoSingle1 showValidationErrors relatedTermsIdReferences allTerms (RelatedTermIndex.fromInt index) relatedTerm


viewCreateSeeAlsoSingle1 : Bool -> Set String -> List Term -> RelatedTermIndex -> Form.RelatedTermField -> Html Msg
viewCreateSeeAlsoSingle1 showValidationErrors relatedTermsIdReferences allTerms index relatedTerm =
    div
        []
        [ div
            [ class "sm:flex sm:flex-row sm:items-center" ]
            [ div
                [ class "flex-auto max-w-lg flex" ]
                [ span
                    [ class "inline-flex items-center" ]
                    [ Components.Button.rounded True
                        [ Accessibility.Aria.label "Delete"
                        , Html.Events.onClick <| PageMsg.Internal <| DeleteRelatedTerm index
                        ]
                        [ Icons.trash
                            [ Svg.Attributes.class "h-5 w-5" ]
                        ]
                    ]
                , Components.SelectMenu.render
                    [ Components.SelectMenu.id <| ElementIds.seeAlsoSelect index
                    , Components.SelectMenu.ariaLabel "Related term"
                    , Components.SelectMenu.validationError relatedTerm.validationError
                    , Components.SelectMenu.showValidationErrors showValidationErrors
                    , Components.SelectMenu.onChange (PageMsg.Internal << SelectRelatedTerm index)
                    ]
                    (allTerms
                        |> List.filter
                            (\term ->
                                (not <| Set.member (Term.id term) relatedTermsIdReferences)
                                    || (Just (Term.id term) == relatedTerm.idReference)
                            )
                        |> List.map
                            (\term ->
                                Components.SelectMenu.Choice
                                    (Term.id term)
                                    [ text <| Term.inlineText term ]
                                    (Just (Term.id term) == relatedTerm.idReference)
                            )
                    )
                ]
            ]
        ]


viewAddRelatedTermButton : Html Msg
viewAddRelatedTermButton =
    Components.Button.secondary
        [ Html.Events.onClick <| PageMsg.Internal <| AddRelatedTerm Nothing ]
        [ Icons.plus [ Svg.Attributes.class "mx-auto -ml-1 mr-2 h-5 w-5" ]
        , text "Add related term"
        ]


viewAddRelatedTermButtonForEmptyState : Html Msg
viewAddRelatedTermButtonForEmptyState =
    Components.Button.emptyState
        [ Html.Events.onClick <| PageMsg.Internal <| AddRelatedTerm Nothing ]
        [ Icons.plus [ Svg.Attributes.class "mx-auto h-12 w-12 text-gray-400" ]
        , span
            [ class "mt-2 block font-medium text-gray-900 dark:text-gray-200" ]
            [ text "Add related term" ]
        ]


viewCreateSeeAlso :
    Bool
    -> Bool
    -> GlossaryItems
    -> Array TermField
    -> Array Form.RelatedTermField
    -> List Term
    -> Html Msg
viewCreateSeeAlso enableMathSupport showValidationErrors glossaryItems terms relatedTermsArray suggestedRelatedTerms =
    let
        termIdsSet : Set String
        termIdsSet =
            terms |> Array.toList |> List.map (TermField.raw >> Form.termBodyToId) |> Set.fromList

        relatedTermsList : List Form.RelatedTermField
        relatedTermsList =
            Array.toList relatedTermsArray

        allTerms : List Term
        allTerms =
            glossaryItems
                |> GlossaryItems.orderedAlphabetically
                |> List.filterMap (Tuple.second >> .terms >> List.head)
    in
    div
        [ class "pt-8 space-y-6 sm:pt-10 sm:space-y-5" ]
        [ div []
            [ h2
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
                    (List.filter (\term -> not <| Set.member (Term.id term) termIdsSet) allTerms)
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
        , viewAddSuggestedSeeAlso enableMathSupport suggestedRelatedTerms |> Extras.Html.showIf (not <| List.isEmpty suggestedRelatedTerms)
        ]


viewAddSuggestedSeeAlso : Bool -> List Term -> Html Msg
viewAddSuggestedSeeAlso enableMathSupport suggestedRelatedTerms =
    div
        []
        [ p
            [ class "mb-2 max-w-2xl text-sm text-gray-500 dark:text-gray-400" ]
            [ text "Suggestions" ]
        , div
            [ class "flow-root" ]
            [ div
                [ class "-m-1 flex flex-wrap" ]
                (suggestedRelatedTerms
                    |> List.map
                        (\suggestedRelatedTerm ->
                            Components.Button.white True
                                [ class "m-1 text-sm"
                                , Html.Events.onClick <| PageMsg.Internal (AddRelatedTerm <| Just <| Term.id suggestedRelatedTerm)
                                ]
                                [ Icons.plus
                                    [ Svg.Attributes.class "-ml-1 mr-2 h-4 w-4" ]
                                , Term.view enableMathSupport suggestedRelatedTerm
                                ]
                        )
                )
            ]
        ]


viewNeedsUpdating :
    Bool
    -> Html Msg
viewNeedsUpdating on =
    div
        [ class "pt-8 space-y-6 sm:pt-10 sm:space-y-5" ]
        [ Components.Button.toggle
            on
            ElementIds.needsUpdatingToggleLabel
            [ Html.Events.onClick <| PageMsg.Internal <| ToggleNeedsUpdating ]
            [ span
                [ class "font-medium text-gray-900 dark:text-gray-300" ]
                [ text "Needs updating" ]
            ]
        ]


viewCreateFormFooter : Model -> Html Msg
viewCreateFormFooter model =
    let
        errorDiv : String -> Html msg
        errorDiv message =
            div
                [ class "flex justify-end mb-2" ]
                [ p
                    [ class "text-red-600 dark:text-red-400" ]
                    [ text message ]
                ]

        common : CommonModel
        common =
            model.common
    in
    div
        [ class "pt-5 lg:border-t dark:border-gray-700" ]
        [ errorDiv "There are errors on this form — see above."
            |> Extras.Html.showIf (model.triedToSaveWhenFormInvalid && Form.hasValidationErrors model.form)
        , model.errorMessageWhileSaving
            |> Extras.Html.showMaybe (\errorMessage -> errorDiv <| "Failed to save — " ++ errorMessage ++ ".")
        , Extras.Html.showIf common.enableSavingChangesInMemory <|
            div
                [ class "mt-2 mb-2 text-sm text-gray-500 dark:text-gray-400 sm:text-right" ]
                [ text Components.Copy.sandboxModeMessage ]
        , div
            [ class "flex justify-end" ]
            [ Components.Button.white True
                [ Html.Events.onClick <|
                    PageMsg.NavigateToListAll common
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
    case model.common.glossary of
        Ok glossary ->
            let
                terms : Array TermField
                terms =
                    Form.termFields model.form

                detailsArray : Array DetailsField
                detailsArray =
                    Form.detailsFields model.form

                relatedTerms : Array Form.RelatedTermField
                relatedTerms =
                    Form.relatedTermFields model.form

                suggestedRelatedTerms : List Term
                suggestedRelatedTerms =
                    Form.suggestRelatedTerms model.form

                newOrUpdatedGlossaryItem : GlossaryItem
                newOrUpdatedGlossaryItem =
                    Form.toGlossaryItem glossary.enableMarkdownBasedSyntax glossary.items model.form Nothing
            in
            { title = GlossaryTitle.inlineText glossary.title
            , body =
                [ div
                    [ class "container mx-auto px-6 pb-10 lg:px-8 max-w-4xl lg:max-w-screen-2xl" ]
                    [ Html.main_
                        []
                        [ h1
                            [ class "text-3xl font-bold leading-tight text-gray-900 dark:text-gray-100 print:text-black pt-6" ]
                            [ text <|
                                if model.common.maybeIndex == Nothing then
                                    "Create a New Glossary Item"

                                else
                                    "Edit Glossary Item"
                            ]
                        , form
                            [ class "pt-7" ]
                            [ div
                                [ class "lg:flex lg:space-x-8" ]
                                [ div
                                    [ class "lg:w-1/2 space-y-8 divide-y divide-gray-200 dark:divide-gray-800 sm:space-y-5" ]
                                    [ viewCreateDescriptionTerms glossary.enableMarkdownBasedSyntax glossary.enableMathSupport model.triedToSaveWhenFormInvalid terms
                                    , viewCreateDescriptionDetails
                                        (not glossary.enableMarkdownBasedSyntax)
                                        glossary.enableMarkdownBasedSyntax
                                        glossary.enableMathSupport
                                        model.triedToSaveWhenFormInvalid
                                        detailsArray
                                    , viewCreateSeeAlso
                                        glossary.enableMathSupport
                                        model.triedToSaveWhenFormInvalid
                                        glossary.items
                                        terms
                                        relatedTerms
                                        suggestedRelatedTerms
                                    , viewNeedsUpdating <| Form.needsUpdating model.form
                                    ]
                                , div
                                    [ class "mt-8 lg:w-1/2 lg:mt-0 max-w-4xl text-gray-900 dark:text-gray-100" ]
                                    [ Html.fieldset
                                        [ class "pt-4" ]
                                        [ Html.legend
                                            [ class "text-xl text-center text-gray-800 dark:text-gray-300 px-1 select-none" ]
                                            [ text "Preview" ]
                                        , article
                                            [ id ElementIds.items ]
                                            [ dl
                                                [ style "display" "block" ]
                                                [ Components.GlossaryItemCard.view
                                                    { enableMathSupport = glossary.enableMathSupport, makeLinksTabbable = True, enableLastUpdatedDates = False }
                                                    Components.GlossaryItemCard.Preview
                                                    newOrUpdatedGlossaryItem
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
            { title = "Glossary"
            , body = [ text "Something went wrong." ]
            }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveCurrentDateTimeForSaving ReceiveCurrentDateTimeForSaving |> Sub.map PageMsg.Internal
