port module Pages.CreateOrEdit exposing (InternalMsg, Model, Msg, init, subscriptions, update, view)

import Accessibility exposing (Html, article, div, dl, form, h1, h2, p, span, text)
import Accessibility.Aria
import Array exposing (Array)
import Browser exposing (Document)
import Browser.Dom as Dom
import CommonModel exposing (CommonModel)
import Components.Badge
import Components.Button
import Components.Copy
import Components.Dividers
import Components.DropdownMenu
import Components.Form
import Components.GlossaryItemCard
import Components.SelectMenu
import Components.Spinner
import Data.Glossary as Glossary
import Data.GlossaryItem as GlossaryItem exposing (GlossaryItem)
import Data.GlossaryItem.RelatedTerm as RelatedTerm
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItem.TermId as TermId exposing (TermId)
import Data.GlossaryItemIndex as GlossaryItemIndex
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Data.GlossaryTitle as GlossaryTitle
import Data.OrderItemsBy exposing (OrderItemsBy(..))
import Data.RelatedTermIndex as RelatedTermIndex exposing (RelatedTermIndex)
import Data.Saving exposing (Saving(..))
import Data.TermIndex as TermIndex exposing (TermIndex)
import Dict exposing (Dict)
import ElementIds
import Extras.Html
import Extras.HtmlEvents
import Extras.HtmlTree as HtmlTree
import Extras.Http
import Extras.Task
import GlossaryItemForm as Form exposing (GlossaryItemForm)
import GlossaryItemForm.DefinitionField as DefinitionField exposing (DefinitionField)
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
    , saving : Saving
    , dropdownMenusWithMoreOptionsForRelatedTerms : Dict Int Components.DropdownMenu.Model
    }


type InternalMsg
    = NoOp
    | AddTerm
    | DeleteTerm TermIndex
    | UpdateTerm TermIndex String
    | ToggleAbbreviation TermIndex
    | UpdateDefinition String
    | AddRelatedTerm (Maybe TermId)
    | SelectRelatedTerm RelatedTermIndex String
    | DeleteRelatedTerm RelatedTermIndex
    | DropdownMenuWithMoreOptionsForRelatedTermMsg Int Components.DropdownMenu.Msg
    | MoveRelatedTermUp RelatedTermIndex
    | MoveRelatedTermDown RelatedTermIndex
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

                existingPreferredTerms : List Term
                existingPreferredTerms =
                    GlossaryItems.preferredTerms items

                itemsListingThisItemAsRelated : List GlossaryItem
                itemsListingThisItemAsRelated =
                    commonModel.maybeIndex
                        |> Maybe.andThen
                            (\index ->
                                items
                                    |> GlossaryItems.get index
                                    |> Maybe.map
                                        (\itemBeingEdited ->
                                            let
                                                preferredTermIdOfItemBeingEdited =
                                                    itemBeingEdited
                                                        |> GlossaryItem.preferredTerm
                                                        |> Term.id
                                            in
                                            GlossaryItems.orderedAlphabetically items
                                                |> Array.toList
                                                |> List.map Tuple.second
                                                |> List.filter
                                                    (\item ->
                                                        item
                                                            |> GlossaryItem.relatedPreferredTerms
                                                            |> List.any
                                                                (RelatedTerm.idReference
                                                                    >> (==) preferredTermIdOfItemBeingEdited
                                                                )
                                                    )
                                        )
                            )
                        |> Maybe.withDefault []

                emptyForm : GlossaryItemForm
                emptyForm =
                    Form.empty existingTerms existingPreferredTerms itemsListingThisItemAsRelated

                form =
                    Maybe.map
                        (\index ->
                            items
                                |> GlossaryItems.get index
                                |> Maybe.map
                                    (Form.fromGlossaryItem
                                        existingTerms
                                        existingPreferredTerms
                                        itemsListingThisItemAsRelated
                                    )
                                |> Maybe.withDefault emptyForm
                        )
                        commonModel.maybeIndex
                        |> Maybe.withDefault emptyForm
            in
            ( { common = commonModel
              , form = form
              , triedToSaveWhenFormInvalid = False
              , saving = NotSaving
              , dropdownMenusWithMoreOptionsForRelatedTerms =
                    dropdownMenusWithMoreOptionsForRelatedTermsForForm form
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
              , saving = NotSaving
              , dropdownMenusWithMoreOptionsForRelatedTerms = Dict.empty
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
                        [ Components.DropdownMenu.id <| ElementIds.moreOptionsForRelatedTermDropdownMenu index ]
                    )
                    result
            )
            Dict.empty



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

        UpdateDefinition body ->
            ( { model | form = Form.updateDefinition model.form body }, Cmd.none )

        AddRelatedTerm maybeTermId ->
            let
                form : GlossaryItemForm
                form =
                    Form.addRelatedTerm maybeTermId model.form

                latestRelatedTermIndex : RelatedTermIndex
                latestRelatedTermIndex =
                    Array.length (Form.relatedTermFields form) - 1 |> RelatedTermIndex.fromInt
            in
            ( { model
                | form = form
                , dropdownMenusWithMoreOptionsForRelatedTerms = dropdownMenusWithMoreOptionsForRelatedTermsForForm form
              }
            , giveFocusToSeeAlsoSelect latestRelatedTermIndex
            )

        SelectRelatedTerm relatedTermIndex selection ->
            let
                relatedTermIdReference : Maybe TermId
                relatedTermIdReference =
                    if selection == "" then
                        Nothing

                    else
                        Just <| TermId.fromString selection
            in
            ( { model | form = Form.selectRelatedTerm relatedTermIndex model.form relatedTermIdReference }, Cmd.none )

        DeleteRelatedTerm relatedTermIndex ->
            let
                form =
                    Form.deleteRelatedTerm relatedTermIndex model.form
            in
            ( { model
                | form = form
                , dropdownMenusWithMoreOptionsForRelatedTerms = dropdownMenusWithMoreOptionsForRelatedTermsForForm form
              }
            , Cmd.none
            )

        DropdownMenuWithMoreOptionsForRelatedTermMsg relatedTermIndexInt msg_ ->
            model.dropdownMenusWithMoreOptionsForRelatedTerms
                |> Dict.get relatedTermIndexInt
                |> Maybe.map
                    (\dropdownMenu ->
                        Components.DropdownMenu.update
                            (\x ->
                                let
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

        MoveRelatedTermUp relatedTermIndexInt ->
            let
                form =
                    Form.moveRelatedTermUp relatedTermIndexInt model.form
            in
            ( { model
                | form = form
                , dropdownMenusWithMoreOptionsForRelatedTerms = dropdownMenusWithMoreOptionsForRelatedTermsForForm form
              }
            , Cmd.none
            )

        MoveRelatedTermDown relatedTermIndexInt ->
            let
                form =
                    Form.moveRelatedTermDown relatedTermIndexInt model.form
            in
            ( { model
                | form = form
                , dropdownMenusWithMoreOptionsForRelatedTerms = dropdownMenusWithMoreOptionsForRelatedTermsForForm form
              }
            , Cmd.none
            )

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
                            , saving = NotSaving
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
                                            |> GlossaryItems.orderedAlphabetically
                                            |> Array.toList
                                            |> List.filter (Tuple.second >> (==) newOrUpdatedGlossaryItem)
                                            |> List.head
                                            |> Maybe.map Tuple.first
                                        )

                            updatedGlossaryItemsWithFocusedOn =
                                case common.orderItemsBy of
                                    FocusedOn termId ->
                                        GlossaryItems.enableFocusingOn termId updatedGlossaryItems

                                    _ ->
                                        updatedGlossaryItems

                            model_ : Model
                            model_ =
                                { model
                                    | common =
                                        { common
                                            | glossary = Ok <| { glossary | items = updatedGlossaryItemsWithFocusedOn }
                                            , maybeIndex = maybeIndex
                                        }
                                    , saving = SavingInProgress
                                }
                        in
                        ( model_
                        , patchHtmlFile model_.common updatedGlossaryItemsWithFocusedOn
                        )

                _ ->
                    ( model, Cmd.none )

        FailedToSave error ->
            ( { model | saving = SavingFailed <| Extras.Http.errorToHumanReadable <| error }
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


giveFocusToSeeAlsoSelect : RelatedTermIndex -> Cmd Msg
giveFocusToSeeAlsoSelect index =
    Task.attempt (always <| PageMsg.Internal NoOp) (Dom.focus <| ElementIds.seeAlsoSelect index)


viewCreateTerm : Bool -> Bool -> Bool -> Bool -> Int -> TermField -> Html Msg
viewCreateTerm showMarkdownBasedSyntaxEnabled mathSupportEnabled showValidationErrors canBeDeleted index term =
    viewCreateTermInternal
        (showMarkdownBasedSyntaxEnabled && index == 0)
        mathSupportEnabled
        showValidationErrors
        canBeDeleted
        (TermIndex.fromInt index)
        term


viewCreateTermInternal : Bool -> Bool -> Bool -> Bool -> TermIndex -> TermField -> Html Msg
viewCreateTermInternal showMarkdownBasedSyntaxEnabled mathSupportEnabled showValidationErrors canBeDeleted termIndex termField =
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
                            [ class "block w-full min-w-0" ]
                            [ Components.Form.inputText
                                (TermField.raw termField)
                                showMarkdownBasedSyntaxEnabled
                                mathSupportEnabled
                                showValidationErrors
                                (TermField.validationError termField)
                                [ id <| ElementIds.termInputField termIndex
                                , required True
                                , Html.Attributes.autocomplete False
                                , Html.Attributes.placeholder <|
                                    if TermIndex.toInt termIndex == 0 then
                                        "Preferred term"

                                    else
                                        "Alternative term"
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


viewCreateTerms : Bool -> Bool -> Bool -> Array TermField -> Html Msg
viewCreateTerms markdownBasedSyntaxEnabled mathSupportEnabled showValidationErrors termsArray =
    let
        terms : List TermField
        terms =
            Array.toList termsArray
    in
    div []
        [ div []
            [ h2
                [ class "text-lg leading-6 font-medium text-gray-900 dark:text-gray-100" ]
                [ text "Terms" ]
            , p
                [ class "mt-1 max-w-2xl text-sm text-gray-500 dark:text-gray-400" ]
                [ text "List the group of terms being defined. The first one is considered the "
                , Html.em
                    []
                    [ text "preferred" ]
                , text " term."
                ]
            ]
        , div
            [ class "mt-6 sm:mt-5 space-y-6 sm:space-y-5" ]
            (List.indexedMap (viewCreateTerm markdownBasedSyntaxEnabled mathSupportEnabled showValidationErrors (List.length terms > 1)) terms
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


viewDefinitionSingle : Bool -> Bool -> Bool -> Bool -> DefinitionField -> Html Msg
viewDefinitionSingle showNewlineWarnings markdownBasedSyntaxEnabled mathSupportEnabled showValidationErrors definitionSingle =
    viewDefinitionSingle1 showNewlineWarnings markdownBasedSyntaxEnabled mathSupportEnabled showValidationErrors definitionSingle


viewDefinitionSingle1 : Bool -> Bool -> Bool -> Bool -> DefinitionField -> Html Msg
viewDefinitionSingle1 showNewlineWarnings markdownBasedSyntaxEnabled mathSupportEnabled showValidationErrors definitionSingle =
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
                    markdownBasedSyntaxEnabled
                    mathSupportEnabled
                    showValidationErrors
                    validationError
                    [ required True
                    , Accessibility.Aria.label "Definition"
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
        , Extras.Html.showIf (showNewlineWarnings && (raw |> String.contains "\n")) <|
            p
                [ class "mt-2 text-red-800 dark:text-red-200" ]
                [ text "This will be turned into a single paragraph — line breaks are automatically converted to spaces" ]
        ]


viewDefinition : Bool -> Bool -> Bool -> Bool -> DefinitionField -> Html Msg
viewDefinition showNewlineWarnings markdownBasedSyntaxEnabled mathSupportEnabled showValidationErrors definitionField =
    div
        [ class "pt-8 space-y-6 sm:pt-10 sm:space-y-5" ]
        [ div []
            [ h2
                [ class "text-lg leading-6 font-medium text-gray-900 dark:text-gray-100" ]
                [ text "Definition" ]
            , p
                [ class "mt-1 max-w-2xl text-sm text-gray-500 dark:text-gray-400" ]
                [ text "Provide a definition (optional) for this group of terms." ]
            ]
        , div
            [ class "space-y-6 sm:space-y-5" ]
            [ viewDefinitionSingle showNewlineWarnings markdownBasedSyntaxEnabled mathSupportEnabled showValidationErrors definitionField ]
        ]


viewTags : Html Msg
viewTags =
    div
        [ class "pt-8 space-y-6 sm:pt-10 sm:space-y-5" ]
        [ div []
            [ h2
                [ class "text-lg leading-6 font-medium text-gray-900 dark:text-gray-100" ]
                [ text "Tags" ]
            , p
                [ class "mt-1 max-w-2xl text-sm text-gray-500 dark:text-gray-400" ]
                [ text "Select any tags that apply to this item." ]
            ]
        , div
            []
            [ Components.Badge.indigoWithCheckbox
                True
                "First Tag"
                [ class "mr-2 mb-2" ]
            , Components.Badge.indigoWithCheckbox
                True
                "Second Tag"
                [ class "mr-2 mb-2" ]
            , Components.Badge.indigoWithCheckbox
                True
                "First Tag"
                [ class "mr-2 mb-2" ]
            , Components.Badge.indigoWithCheckbox
                True
                "First Tag"
                [ class "mr-2 mb-2" ]
            , Components.Badge.indigoWithCheckbox
                True
                "First Tag"
                [ class "mr-2 mb-2" ]
            , Components.Badge.indigoWithCheckbox
                True
                "First Tag"
                [ class "mr-2 mb-2" ]
            , Components.Badge.indigoWithCheckbox
                True
                "First Tag"
                [ class "mr-2 mb-2" ]
            ]
        ]


viewCreateSeeAlsoSingle :
    Bool
    -> Set String
    -> Int
    -> List Term
    -> Dict Int Components.DropdownMenu.Model
    -> Int
    -> Form.RelatedTermField
    -> Html Msg
viewCreateSeeAlsoSingle showValidationErrors relatedTermsIdReferences numberOfRelatedTerms allTerms dropdownMenusWithMoreOptionsForRelatedTerms index relatedTerm =
    viewCreateSeeAlsoSingle1
        showValidationErrors
        relatedTermsIdReferences
        numberOfRelatedTerms
        allTerms
        (Dict.get index dropdownMenusWithMoreOptionsForRelatedTerms)
        (RelatedTermIndex.fromInt index)
        relatedTerm


viewCreateSeeAlsoSingle1 :
    Bool
    -> Set String
    -> Int
    -> List Term
    -> Maybe Components.DropdownMenu.Model
    -> RelatedTermIndex
    -> Form.RelatedTermField
    -> Html Msg
viewCreateSeeAlsoSingle1 showValidationErrors relatedTermsIdReferences numberOfRelatedTerms allTerms maybeDropdownMenuWithMoreOptions index relatedTerm =
    div
        []
        [ div
            [ class "flex-auto max-w-xl flex items-center" ]
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
                , Components.SelectMenu.ariaLabel "Related preferred term"
                , Components.SelectMenu.validationError relatedTerm.validationError
                , Components.SelectMenu.showValidationErrors showValidationErrors
                , Components.SelectMenu.onChange (PageMsg.Internal << SelectRelatedTerm index)
                ]
                (allTerms
                    |> List.filter
                        (\term ->
                            (not <| Set.member (Term.id term |> TermId.toString) relatedTermsIdReferences)
                                || (Just (Term.id term) == relatedTerm.idReference)
                        )
                    |> List.map
                        (\term ->
                            Components.SelectMenu.Choice
                                (Term.id term |> TermId.toString)
                                [ text <| Term.inlineText term ]
                                (Just (Term.id term) == relatedTerm.idReference)
                        )
                )
            , Extras.Html.showIf (numberOfRelatedTerms > 1) <|
                Extras.Html.showMaybe
                    (\dropdownMenuWithMoreOptions ->
                        span
                            [ class "sm:hidden ml-2 flex items-center" ]
                            [ viewMoreOptionsForRelatedTermDropdownButton numberOfRelatedTerms index dropdownMenuWithMoreOptions ]
                    )
                    maybeDropdownMenuWithMoreOptions
            , div
                [ class "hidden sm:block sm:ml-2 flex items-center" ]
                [ Components.Button.rounded (RelatedTermIndex.toInt index > 0)
                    [ Accessibility.Aria.label "Move up"
                    , Html.Events.onClick <| PageMsg.Internal <| MoveRelatedTermUp index
                    ]
                    [ Icons.arrowUp
                        [ Svg.Attributes.class "h-5 w-5" ]
                    ]
                , Components.Button.rounded (RelatedTermIndex.toInt index + 1 < numberOfRelatedTerms)
                    [ Accessibility.Aria.label "Move down"
                    , Html.Events.onClick <| PageMsg.Internal <| MoveRelatedTermDown index
                    , class ""
                    ]
                    [ Icons.arrowDown
                        [ Svg.Attributes.class "h-5 w-5" ]
                    ]
                ]
            ]
        ]


viewMoreOptionsForRelatedTermDropdownButton : Int -> RelatedTermIndex -> Components.DropdownMenu.Model -> Html Msg
viewMoreOptionsForRelatedTermDropdownButton numberOfRelatedTerms index dropdownMenuWithMoreOptionsForRelatedTerm =
    let
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
                            , text "Move up"
                            ]
                        ]
                        (PageMsg.Internal <| MoveRelatedTermUp index)

              else
                Nothing
            , if indexInt + 1 < numberOfRelatedTerms then
                Just <|
                    Components.DropdownMenu.choice
                        [ span
                            [ class "inline-flex items-center" ]
                            [ Icons.arrowDown
                                [ Svg.Attributes.class "h-5 w-5 text-gray-500 dark:text-gray-400 mr-2" ]
                            , text "Move down"
                            ]
                        ]
                        (PageMsg.Internal <| MoveRelatedTermDown index)

              else
                Nothing
            ]
        )


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
    -> Dict Int Components.DropdownMenu.Model
    -> List Term
    -> Html Msg
viewCreateSeeAlso enableMathSupport showValidationErrors glossaryItems terms relatedTermsArray dropdownMenusWithMoreOptionsForRelatedTerms suggestedRelatedTerms =
    let
        termIdsSet : Set String
        termIdsSet =
            terms |> Array.toList |> List.map (TermField.raw >> Form.termBodyToId) |> Set.fromList

        relatedTermsList : List Form.RelatedTermField
        relatedTermsList =
            Array.toList relatedTermsArray

        allPreferredTerms : List Term
        allPreferredTerms =
            glossaryItems
                |> GlossaryItems.orderedAlphabetically
                |> Array.toList
                |> List.map (Tuple.second >> GlossaryItem.preferredTerm)
    in
    div
        [ class "pt-8 space-y-6 sm:pt-10 sm:space-y-5" ]
        [ div []
            [ h2
                [ class "text-lg leading-6 font-medium text-gray-900 dark:text-gray-100" ]
                [ text "Related terms" ]
            , p
                [ class "mt-1 max-w-2xl text-sm text-gray-500 dark:text-gray-400" ]
                [ text "Point to any related preferred terms the reader might want to look up." ]
            ]
        , div
            [ class "mt-6 sm:mt-5 space-y-6 sm:space-y-5" ]
            (List.indexedMap
                (viewCreateSeeAlsoSingle
                    showValidationErrors
                    (relatedTermsList
                        |> List.filterMap (.idReference >> Maybe.map TermId.toString)
                        |> Set.fromList
                    )
                    (List.length relatedTermsList)
                    (List.filter (\term -> not <| Set.member (Term.id term |> TermId.toString) termIdsSet) allPreferredTerms)
                    dropdownMenusWithMoreOptionsForRelatedTerms
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
                                , Term.view enableMathSupport [] suggestedRelatedTerm
                                ]
                        )
                )
            ]
        ]


viewMiscellaneous :
    Bool
    -> Html Msg
viewMiscellaneous on =
    div
        [ class "pt-8 space-y-6 sm:pt-10 sm:space-y-5" ]
        [ div []
            [ h2
                [ class "text-lg leading-6 font-medium text-gray-900 dark:text-gray-100" ]
                [ text "Miscellaneous" ]
            ]
        , Components.Button.toggle
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
    in
    div
        [ class "pt-5 lg:border-t dark:border-gray-700 flex flex-col items-center" ]
        [ errorDiv "There are errors on this form — see above."
            |> Extras.Html.showIf (model.triedToSaveWhenFormInvalid && Form.hasValidationErrors model.form)
        , Extras.Html.showIf common.enableSavingChangesInMemory <|
            div
                [ class "mt-2 mb-2 text-sm text-gray-500 dark:text-gray-400 sm:text-right" ]
                [ text Components.Copy.sandboxModeMessage ]
        , div
            [ class "flex items-center" ]
            [ Components.Button.white
                (saving /= SavingInProgress)
                [ Html.Events.onClick <|
                    PageMsg.NavigateToListAll common
                ]
                [ text "Cancel" ]
            , Components.Button.primary
                (saving /= SavingInProgress && not (model.triedToSaveWhenFormInvalid && Form.hasValidationErrors model.form))
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
        Ok glossary ->
            let
                terms : Array TermField
                terms =
                    Form.termFields model.form

                definitionArray : DefinitionField
                definitionArray =
                    Form.definitionField model.form

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
                    [ class "container mx-auto px-6 pb-16 lg:px-8 max-w-4xl lg:max-w-screen-2xl" ]
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
                                    [ class "lg:w-1/2" ]
                                    [ viewCreateTerms glossary.enableMarkdownBasedSyntax glossary.enableMathSupport model.triedToSaveWhenFormInvalid terms
                                    , viewDefinition
                                        (not glossary.enableMarkdownBasedSyntax)
                                        glossary.enableMarkdownBasedSyntax
                                        glossary.enableMathSupport
                                        model.triedToSaveWhenFormInvalid
                                        definitionArray
                                    , viewTags
                                    , viewCreateSeeAlso
                                        glossary.enableMathSupport
                                        model.triedToSaveWhenFormInvalid
                                        glossary.items
                                        terms
                                        relatedTerms
                                        model.dropdownMenusWithMoreOptionsForRelatedTerms
                                        suggestedRelatedTerms
                                    , viewMiscellaneous <| Form.needsUpdating model.form
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
                                                    { previous = Nothing
                                                    , item = Just ( GlossaryItemIndex.fromInt -1, newOrUpdatedGlossaryItem )
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
            { title = "Glossary"
            , body = [ text "Something went wrong." ]
            }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ receiveCurrentDateTimeForSaving ReceiveCurrentDateTimeForSaving
            |> Sub.map PageMsg.Internal
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
