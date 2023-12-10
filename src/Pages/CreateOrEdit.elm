port module Pages.CreateOrEdit exposing (InternalMsg, Model, Msg, init, subscriptions, update, view)

import Accessibility exposing (Html, article, div, dl, form, h1, h2, p, span, text)
import Accessibility.Aria
import Array exposing (Array)
import Browser exposing (Document)
import Browser.Dom as Dom
import CommonModel exposing (CommonModel)
import Components.Badge
import Components.Button
import Components.DropdownMenu
import Components.Form
import Components.GlossaryItemCard
import Components.SelectMenu
import Components.Spinner
import Data.Glossary as Glossary
import Data.GlossaryItem.Tag as Tag exposing (Tag)
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItem.TermId as TermId exposing (TermId)
import Data.GlossaryItemForHtml as GlossaryItemForHtml exposing (GlossaryItemForHtml)
import Data.GlossaryItemId as GlossaryItemId
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Data.GlossaryTitle as GlossaryTitle
import Data.RelatedTermIndex as RelatedTermIndex exposing (RelatedTermIndex)
import Data.Saving exposing (Saving(..))
import Data.TagId as TagId exposing (TagId)
import Data.TermIndex as TermIndex exposing (TermIndex)
import Dict exposing (Dict)
import ElementIds
import Extras.Html
import Extras.HtmlAttribute
import Extras.HtmlEvents
import Extras.HtmlTree as HtmlTree
import Extras.Task
import GlossaryItemForm as Form exposing (GlossaryItemForm)
import GlossaryItemForm.DefinitionField as DefinitionField exposing (DefinitionField)
import GlossaryItemForm.TermField as TermField exposing (TermField)
import Html
import Html.Attributes exposing (class, id, required, style)
import Html.Events
import Http
import Icons
import Internationalisation as I18n
import PageMsg exposing (PageMsg)
import QueryParameters
import Save
import Set exposing (Set)
import Svg.Attributes
import Task



-- MODEL


type alias Model =
    { common : CommonModel
    , form : GlossaryItemForm
    , triedToSaveWhenFormInvalid : Bool
    , glossaryItemsError : Maybe String
    , saving : Saving
    , dropdownMenusWithMoreOptionsForRelatedTerms : Dict Int Components.DropdownMenu.Model
    }


updateForm : (GlossaryItemForm -> GlossaryItemForm) -> Model -> Model
updateForm f model =
    { model | form = f model.form, glossaryItemsError = Nothing }


type InternalMsg
    = NoOp
    | AddTerm
    | DeleteTerm TermIndex
    | UpdateTerm TermIndex String
    | ToggleAbbreviation TermIndex
    | ToggleTagCheckbox Tag
    | UpdateDefinition String
    | SelectDisambiguationTag String
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
                tags : List ( TagId, Tag )
                tags =
                    GlossaryItems.tagByIdList items

                existingTerms : List Term
                existingTerms =
                    GlossaryItems.disambiguatedPreferredTerms Nothing items

                existingDisambiguatedPreferredTerms : List Term
                existingDisambiguatedPreferredTerms =
                    GlossaryItems.disambiguatedPreferredTerms Nothing items

                preferredTermsOfItemsListingThisItemAsRelated : List Term
                preferredTermsOfItemsListingThisItemAsRelated =
                    commonModel.maybeId
                        |> Maybe.map
                            (\id ->
                                items
                                    |> GlossaryItems.relatedForWhichItems id
                                    |> List.filterMap (\id_ -> GlossaryItems.disambiguatedPreferredTerm id_ items)
                            )
                        |> Maybe.withDefault []

                filterByTagId : Maybe TagId
                filterByTagId =
                    commonModel.queryParameters
                        |> QueryParameters.filterByTag
                        |> Maybe.andThen (\tag -> GlossaryItems.tagIdFromTag tag items)

                emptyForm : GlossaryItemForm
                emptyForm =
                    Form.empty
                        existingTerms
                        existingDisambiguatedPreferredTerms
                        tags
                        filterByTagId
                        preferredTermsOfItemsListingThisItemAsRelated

                form =
                    Maybe.andThen
                        (\id ->
                            items
                                |> GlossaryItems.get id
                                |> Maybe.map
                                    (\itemForHtml ->
                                        let
                                            disambiguationTagId : Maybe TagId
                                            disambiguationTagId =
                                                itemForHtml
                                                    |> GlossaryItemForHtml.disambiguationTag
                                                    |> Maybe.andThen (\tag -> GlossaryItems.tagIdFromTag tag items)
                                        in
                                        Form.fromGlossaryItemForHtml
                                            existingTerms
                                            existingDisambiguatedPreferredTerms
                                            tags
                                            preferredTermsOfItemsListingThisItemAsRelated
                                            (GlossaryItemForHtml.relatedPreferredTerms itemForHtml)
                                            disambiguationTagId
                                            itemForHtml
                                    )
                        )
                        commonModel.maybeId
                        |> Maybe.withDefault emptyForm
            in
            ( { common = commonModel
              , form = form
              , triedToSaveWhenFormInvalid = False
              , glossaryItemsError = Nothing
              , saving = NotSaving
              , dropdownMenusWithMoreOptionsForRelatedTerms =
                    dropdownMenusWithMoreOptionsForRelatedTermsForForm form
              }
            , if commonModel.maybeId == Nothing then
                0 |> TermIndex.fromInt |> giveFocusToTermInputField

              else
                Cmd.none
            )

        Err _ ->
            ( { common = commonModel
              , form = Form.empty [] [] [] Nothing []
              , triedToSaveWhenFormInvalid = False
              , glossaryItemsError = Nothing
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
            ( updateForm (always form) model
            , giveFocusToTermInputField latestTermIndex
            )

        DeleteTerm termIndex ->
            ( updateForm (Form.deleteTerm termIndex) model
            , Cmd.none
            )

        UpdateTerm termIndex body ->
            ( updateForm (Form.updateTerm termIndex body) model
            , Cmd.none
            )

        ToggleAbbreviation termIndex ->
            ( updateForm (Form.toggleAbbreviation termIndex) model, Cmd.none )

        ToggleTagCheckbox tag ->
            ( updateForm (Form.toggleTagCheckbox tag) model, Cmd.none )

        UpdateDefinition body ->
            ( updateForm (Form.updateDefinition body) model, Cmd.none )

        SelectDisambiguationTag rawTag ->
            let
                disambiguationTagId : Maybe TagId
                disambiguationTagId =
                    model.form
                        |> Form.tagCheckboxes
                        |> List.filterMap
                            (\( ( tagId, tag ), _ ) ->
                                if Tag.raw tag == rawTag then
                                    Just tagId

                                else
                                    Nothing
                            )
                        |> List.head
            in
            ( updateForm (Form.updateDisambiguationTagId disambiguationTagId) model
            , Cmd.none
            )

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
                | dropdownMenusWithMoreOptionsForRelatedTerms = dropdownMenusWithMoreOptionsForRelatedTermsForForm form
              }
                |> updateForm (always form)
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
            ( updateForm (Form.selectRelatedTerm relatedTermIndex relatedTermIdReference) model
            , Cmd.none
            )

        DeleteRelatedTerm relatedTermIndex ->
            let
                form =
                    Form.deleteRelatedTerm relatedTermIndex model.form
            in
            ( { model
                | dropdownMenusWithMoreOptionsForRelatedTerms = dropdownMenusWithMoreOptionsForRelatedTermsForForm form
              }
                |> updateForm (always form)
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
                | dropdownMenusWithMoreOptionsForRelatedTerms = dropdownMenusWithMoreOptionsForRelatedTermsForForm form
              }
                |> updateForm (always form)
            , Cmd.none
            )

        MoveRelatedTermDown relatedTermIndexInt ->
            let
                form =
                    Form.moveRelatedTermDown relatedTermIndexInt model.form
            in
            ( { model
                | dropdownMenusWithMoreOptionsForRelatedTerms = dropdownMenusWithMoreOptionsForRelatedTermsForForm form
              }
                |> updateForm (always form)
            , Cmd.none
            )

        ToggleNeedsUpdating ->
            ( updateForm Form.toggleNeedsUpdating model
            , Cmd.none
            )

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
                            newOrUpdatedGlossaryItem : GlossaryItemForHtml
                            newOrUpdatedGlossaryItem =
                                Form.toGlossaryItem glossary.items model.form <| Just dateTime

                            common : CommonModel
                            common =
                                model.common

                            ( updatedGlossaryItems, maybeId ) =
                                case common.maybeId of
                                    Just id ->
                                        ( GlossaryItems.update id newOrUpdatedGlossaryItem glossary.items
                                        , Just id
                                        )

                                    Nothing ->
                                        let
                                            updated : Result String GlossaryItems
                                            updated =
                                                GlossaryItems.insert newOrUpdatedGlossaryItem glossary.items
                                        in
                                        ( updated
                                        , updated
                                            |> Result.toMaybe
                                            |> Maybe.andThen
                                                (\updated_ ->
                                                    -- Find index of newly inserted item
                                                    updated_
                                                        |> GlossaryItems.orderedAlphabetically Nothing
                                                        |> List.filter (Tuple.second >> (==) newOrUpdatedGlossaryItem)
                                                        |> List.head
                                                        |> Maybe.map Tuple.first
                                                )
                                        )
                        in
                        case updatedGlossaryItems of
                            Ok updatedGlossaryItems_ ->
                                let
                                    glossary1 : Glossary.Glossary
                                    glossary1 =
                                        { glossary | items = updatedGlossaryItems_ }

                                    common1 : CommonModel
                                    common1 =
                                        { common
                                            | glossary = Ok glossary1
                                            , maybeId = maybeId
                                        }
                                in
                                ( { model | saving = SavingInProgress }
                                , Save.patchHtmlFile
                                    common1
                                    glossary1
                                    (PageMsg.Internal << FailedToSave)
                                    (PageMsg.NavigateToListAll common1)
                                )

                            Err error ->
                                ( { model | glossaryItemsError = Just error }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        FailedToSave error ->
            ( { model | saving = SavingFailed <| I18n.httpErrorDescription <| error }
            , Cmd.none
            )



-- VIEW


giveFocusToTermInputField : TermIndex -> Cmd Msg
giveFocusToTermInputField termIndex =
    Task.attempt (always <| PageMsg.Internal NoOp) (Dom.focus <| ElementIds.termInputField termIndex)


giveFocusToSeeAlsoSelect : RelatedTermIndex -> Cmd Msg
giveFocusToSeeAlsoSelect index =
    Task.attempt (always <| PageMsg.Internal NoOp) (Dom.focus <| ElementIds.seeAlsoSelect index)


viewCreateTerm : Bool -> Bool -> Bool -> Int -> TermField -> Html Msg
viewCreateTerm mathSupportEnabled showValidationErrors canBeDeleted index term =
    viewCreateTermInternal
        (index == 0)
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
                    [ class "inline-flex items-center"
                    , Extras.HtmlAttribute.showIf showMarkdownBasedSyntaxEnabled <| class "sm:pt-6"
                    ]
                    [ Components.Button.rounded canBeDeleted
                        [ Accessibility.Aria.label I18n.delete
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
                                        I18n.preferredTerm

                                    else
                                        I18n.alternativeTerm
                                , Accessibility.Aria.label I18n.term
                                , Accessibility.Aria.required True
                                , Html.Events.onInput (PageMsg.Internal << UpdateTerm termIndex)
                                , Extras.HtmlEvents.onEnter <| PageMsg.Internal NoOp
                                ]
                            ]
                        , div
                            [ class "flex-auto mt-2 sm:mt-0 relative flex items-baseline"
                            , Extras.HtmlAttribute.showIf showMarkdownBasedSyntaxEnabled <| class "sm:pt-6"
                            ]
                            [ div
                                [ class "sm:ml-5" ]
                                [ Components.Button.toggle
                                    (TermField.isAbbreviation termField)
                                    abbreviationLabelId
                                    [ Html.Events.onClick <| PageMsg.Internal <| ToggleAbbreviation termIndex ]
                                    [ span
                                        [ class "font-medium text-gray-900 dark:text-gray-300" ]
                                        [ text I18n.abbreviation ]
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


viewCreateTerms : Bool -> Bool -> Array TermField -> Html Msg
viewCreateTerms mathSupportEnabled showValidationErrors termsArray =
    let
        terms : List TermField
        terms =
            Array.toList termsArray
    in
    div []
        [ div []
            [ h2
                [ class "text-lg leading-6 font-medium text-gray-900 dark:text-gray-100" ]
                [ text I18n.terms ]
            , p
                [ class "mt-1 max-w-2xl text-sm text-gray-500 dark:text-gray-400" ]
                I18n.listTheGroupOfTermsBeingDefined
            ]
        , div
            [ class "mt-6 sm:mt-5 space-y-6 sm:space-y-5" ]
            (List.indexedMap (viewCreateTerm mathSupportEnabled showValidationErrors (List.length terms > 1)) terms
                ++ [ div []
                        [ Components.Button.secondary
                            [ Html.Events.onClick <| PageMsg.Internal AddTerm ]
                            [ Icons.plus [ Svg.Attributes.class "mx-auto -ml-1 mr-2 h-5 w-5" ]
                            , text I18n.addTermButton
                            ]
                        ]
                   ]
            )
        ]


viewDefinitionSingle : Bool -> Bool -> DefinitionField -> Html Msg
viewDefinitionSingle mathSupportEnabled showValidationErrors definitionSingle =
    viewDefinitionSingle1 mathSupportEnabled showValidationErrors definitionSingle


viewDefinitionSingle1 : Bool -> Bool -> DefinitionField -> Html Msg
viewDefinitionSingle1 mathSupportEnabled showValidationErrors definitionSingle =
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
                    True
                    mathSupportEnabled
                    showValidationErrors
                    validationError
                    [ required True
                    , Accessibility.Aria.label I18n.definition
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
        ]


viewDefinition : Bool -> Bool -> DefinitionField -> Html Msg
viewDefinition mathSupportEnabled showValidationErrors definitionField =
    div
        [ class "pt-8 space-y-6 sm:pt-10 sm:space-y-5" ]
        [ div []
            [ h2
                [ class "text-lg leading-6 font-medium text-gray-900 dark:text-gray-100" ]
                [ text I18n.definition ]
            , p
                [ class "mt-1 max-w-2xl text-sm text-gray-500 dark:text-gray-400" ]
                [ text I18n.provideADefinitionForThisGroupOfTerms ]
            ]
        , div
            [ class "space-y-6 sm:space-y-5" ]
            [ viewDefinitionSingle mathSupportEnabled showValidationErrors definitionField ]
        ]


viewTags : Bool -> List ( ( TagId, Tag ), Bool ) -> Html Msg
viewTags enableMathSupport tagCheckboxes =
    div
        [ class "pt-8 space-y-6 sm:pt-10 sm:space-y-5" ]
        [ div []
            [ h2
                [ class "text-lg leading-6 font-medium text-gray-900 dark:text-gray-100" ]
                [ text I18n.tags ]
            , p
                [ class "mt-1 max-w-2xl text-sm text-gray-500 dark:text-gray-400" ]
                [ text I18n.selectAllTagsThatApplyToThisItem ]
            ]
        , div
            []
            (tagCheckboxes
                |> List.map
                    (\( ( tagId, tag ), checked ) ->
                        Components.Badge.indigoWithCheckbox
                            { tabbable = True, checked = checked }
                            (tagId |> TagId.toInt |> String.fromInt |> (++) "tag-")
                            (PageMsg.Internal <| ToggleTagCheckbox tag)
                            [ class "mr-2 mb-2" ]
                            [ Tag.view enableMathSupport [] tag ]
                    )
            )
        ]


viewDisambiguationTag : Maybe TagId -> List ( TagId, Tag ) -> Html Msg
viewDisambiguationTag disambiguationTagId tags =
    div
        [ class "pt-8 space-y-6 sm:pt-10 sm:space-y-5" ]
        [ div []
            [ h2
                [ class "text-lg leading-6 font-medium text-gray-900 dark:text-gray-100" ]
                [ text I18n.disambiguationTagOptional ]
            , p
                [ class "mt-1 max-w-2xl text-sm text-gray-500 dark:text-gray-400" ]
                [ text I18n.chooseWhichTagShouldBeUsedToDistinguishThisItem ]
            ]
        , div
            [ class "max-w-md" ]
            [ Components.SelectMenu.render
                [ Components.SelectMenu.id <| ElementIds.disambiguationTagSelect
                , Components.SelectMenu.ariaLabel I18n.disambiguationTag
                , Components.SelectMenu.onChange (PageMsg.Internal << SelectDisambiguationTag)
                ]
                (Components.SelectMenu.Choice
                    ""
                    [ text I18n.none ]
                    (disambiguationTagId == Nothing)
                    :: (tags
                            |> List.map
                                (\( tagId, tag ) ->
                                    Components.SelectMenu.Choice
                                        (Tag.raw tag)
                                        [ text <| Tag.inlineText tag ]
                                        (disambiguationTagId == Just tagId)
                                )
                       )
                )
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
            [ class "flex-auto max-w-2xl flex items-center" ]
            [ span
                [ class "inline-flex items-center" ]
                [ Components.Button.rounded True
                    [ Accessibility.Aria.label I18n.delete
                    , Html.Events.onClick <| PageMsg.Internal <| DeleteRelatedTerm index
                    ]
                    [ Icons.trash
                        [ Svg.Attributes.class "h-5 w-5" ]
                    ]
                ]
            , Components.SelectMenu.render
                [ Components.SelectMenu.id <| ElementIds.seeAlsoSelect index
                , Components.SelectMenu.ariaLabel I18n.relatedItem
                , Components.SelectMenu.validationError relatedTerm.validationError
                , Components.SelectMenu.showValidationErrors showValidationErrors
                , Components.SelectMenu.onChange (PageMsg.Internal << SelectRelatedTerm index)
                ]
                (allTerms
                    |> List.filter
                        (\term ->
                            (not <| Set.member (Term.id term |> TermId.toString) relatedTermsIdReferences)
                                || (Just (Term.id term) == relatedTerm.id)
                        )
                    |> List.map
                        (\term ->
                            Components.SelectMenu.Choice
                                (Term.id term |> TermId.toString)
                                [ text <| Term.inlineText term ]
                                (Just (Term.id term) == relatedTerm.id)
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
                    [ Accessibility.Aria.label I18n.moveUp
                    , Html.Events.onClick <| PageMsg.Internal <| MoveRelatedTermUp index
                    ]
                    [ Icons.arrowUp
                        [ Svg.Attributes.class "h-5 w-5" ]
                    ]
                , Components.Button.rounded (RelatedTermIndex.toInt index + 1 < numberOfRelatedTerms)
                    [ Accessibility.Aria.label I18n.moveDown
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
                            , text I18n.moveUp
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
                            , text I18n.moveDown
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
        , text I18n.addRelatedItem
        ]


viewAddRelatedTermButtonForEmptyState : Html Msg
viewAddRelatedTermButtonForEmptyState =
    Components.Button.emptyState
        [ Html.Events.onClick <| PageMsg.Internal <| AddRelatedTerm Nothing ]
        [ Icons.plus [ Svg.Attributes.class "mx-auto h-12 w-12 text-gray-400" ]
        , span
            [ class "mt-2 block font-medium text-gray-900 dark:text-gray-200" ]
            [ text I18n.addRelatedItem ]
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
                |> GlossaryItems.orderedAlphabetically Nothing
                |> List.map (Tuple.second >> GlossaryItemForHtml.disambiguatedPreferredTerm)
    in
    div
        [ class "pt-8 space-y-6 sm:pt-10 sm:space-y-5" ]
        [ div []
            [ h2
                [ class "text-lg leading-6 font-medium text-gray-900 dark:text-gray-100" ]
                [ text I18n.relatedItems ]
            , p
                [ class "mt-1 max-w-2xl text-sm text-gray-500 dark:text-gray-400" ]
                [ text I18n.pointToAnyRelatedItems ]
            ]
        , div
            [ class "mt-6 sm:mt-5 space-y-6 sm:space-y-5" ]
            (List.indexedMap
                (viewCreateSeeAlsoSingle
                    showValidationErrors
                    (relatedTermsList
                        |> List.filterMap (.id >> Maybe.map TermId.toString)
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
            [ text I18n.suggestions ]
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


viewMiscellaneous : Bool -> Html Msg
viewMiscellaneous on =
    div
        [ class "pt-8 space-y-6 sm:pt-10 sm:space-y-5" ]
        [ div []
            [ h2
                [ class "text-lg leading-6 font-medium text-gray-900 dark:text-gray-100" ]
                [ text I18n.miscellaneous ]
            ]
        , Components.Button.toggle
            on
            ElementIds.needsUpdatingToggleLabel
            [ Html.Events.onClick <| PageMsg.Internal <| ToggleNeedsUpdating ]
            [ span
                [ class "font-medium text-gray-900 dark:text-gray-300" ]
                [ text I18n.needsUpdating ]
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
                [ class "flex justify-end mb-4" ]
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
        [ errorDiv I18n.thereAreErrorsOnThisFormSeeAbove
            |> Extras.Html.showIf (model.triedToSaveWhenFormInvalid && Form.hasValidationErrors model.form)
        , Extras.Html.showMaybe
            (\glossaryItemsError ->
                errorDiv <| I18n.unableToSaveAsItWouldResultInTheFollowing ++ ": " ++ glossaryItemsError ++ "."
            )
            model.glossaryItemsError
        , Extras.Html.showIf common.enableSavingChangesInMemory <|
            div
                [ class "mt-2 mb-2 text-sm text-gray-500 dark:text-gray-400 sm:text-right" ]
                [ text I18n.sandboxModeMessage ]
        , div
            [ class "flex items-center" ]
            [ Components.Button.white
                (saving /= SavingInProgress)
                [ Html.Events.onClick <|
                    PageMsg.NavigateToListAll common
                ]
                [ text I18n.cancel ]
            , Components.Button.primary
                (saving /= SavingInProgress && not (model.triedToSaveWhenFormInvalid && Form.hasValidationErrors model.form))
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
    case model.common.glossary of
        Ok glossary ->
            let
                terms : Array TermField
                terms =
                    Form.termFields model.form

                definitionArray : DefinitionField
                definitionArray =
                    Form.definitionField model.form

                disambiguationTagId : Maybe TagId
                disambiguationTagId =
                    Form.disambiguationTagId model.form

                relatedTerms : Array Form.RelatedTermField
                relatedTerms =
                    Form.relatedTermFields model.form

                suggestedRelatedTerms : List Term
                suggestedRelatedTerms =
                    Form.suggestRelatedTerms model.form

                newOrUpdatedGlossaryItem : GlossaryItemForHtml
                newOrUpdatedGlossaryItem =
                    Form.toGlossaryItem glossary.items model.form Nothing
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
                                if model.common.maybeId == Nothing then
                                    I18n.createANewGlossaryItemCapitalised

                                else
                                    I18n.editGlossaryItemCapitalised
                            ]
                        , form
                            [ class "pt-7" ]
                            [ div
                                [ class "lg:flex lg:space-x-8" ]
                                [ div
                                    [ class "lg:w-1/2" ]
                                    [ viewCreateTerms glossary.enableMathSupport model.triedToSaveWhenFormInvalid terms
                                    , viewDefinition
                                        glossary.enableMathSupport
                                        model.triedToSaveWhenFormInvalid
                                        definitionArray
                                    , model.form
                                        |> Form.tagCheckboxes
                                        |> viewTags glossary.enableMathSupport
                                        |> Extras.Html.showIf (not <| List.isEmpty <| Form.tagCheckboxes model.form)
                                    , model.form
                                        |> Form.tagCheckboxes
                                        |> List.filterMap
                                            (\( tagWithId, checked ) ->
                                                if checked then
                                                    Just tagWithId

                                                else
                                                    Nothing
                                            )
                                        |> (\tags ->
                                                Extras.Html.showIf (not <| List.isEmpty tags) <|
                                                    viewDisambiguationTag disambiguationTagId tags
                                           )
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
                                        [ class "pt-4 lg:sticky lg:top-5" ]
                                        [ Html.legend
                                            [ class "text-xl text-center text-gray-800 dark:text-gray-300 px-1 select-none" ]
                                            [ text I18n.preview ]
                                        , article
                                            [ id ElementIds.items ]
                                            [ dl
                                                [ style "display" "block" ]
                                                [ Components.GlossaryItemCard.view
                                                    { enableMathSupport = glossary.enableMathSupport, makeLinksTabbable = True, enableLastUpdatedDates = False }
                                                    Components.GlossaryItemCard.Preview
                                                    { previous = Nothing
                                                    , item = Just ( GlossaryItemId.create -1, newOrUpdatedGlossaryItem )
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
            { title = I18n.glossaryCapitalised
            , body = [ text I18n.somethingWentWrong ]
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
