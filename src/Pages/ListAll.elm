port module Pages.ListAll exposing (InternalMsg, Layout, MenuForMobileVisibility, Model, Msg, SearchDialog, init, subscriptions, update, view)

import Accessibility
    exposing
        ( Html
        , button
        , details
        , div
        , fieldset
        , h1
        , h2
        , h5
        , header
        , label
        , legend
        , li
        , nav
        , p
        , pre
        , span
        , summary
        , text
        , ul
        )
import Accessibility.Aria
import Accessibility.Key
import Accessibility.Role
import Array
import Browser exposing (Document)
import Browser.Dom as Dom
import Browser.Navigation as Navigation
import CommonModel exposing (CommonModel)
import Components.AboutSection
import Components.Badge
import Components.Button
import Components.Dividers
import Components.DropdownMenu
import Components.GlossaryItemCard
import Components.ModalDialog
import Components.SearchDialog
import Components.SelectMenu
import Components.Spinner
import Data.CardWidth as CardWidth exposing (CardWidth)
import Data.Editability as Editability exposing (Editability(..))
import Data.Glossary as Glossary exposing (Glossary)
import Data.GlossaryItem.Tag as Tag exposing (Tag)
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItem.TermId as TermId exposing (TermId)
import Data.GlossaryItemForHtml as GlossaryItemForHtml exposing (GlossaryItemForHtml)
import Data.GlossaryItemId exposing (GlossaryItemId)
import Data.GlossaryItemWithPreviousAndNext exposing (GlossaryItemWithPreviousAndNext)
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Data.GlossaryTitle as GlossaryTitle
import Data.IndexOfTerms as IndexOfTerms exposing (IndexOfTerms, TermGroup)
import Data.OrderItemsBy exposing (OrderItemsBy(..))
import Data.Saving exposing (Saving(..))
import Data.TagDescription as TagDescription exposing (TagDescription)
import Data.TagId exposing (TagId)
import Data.Theme exposing (Theme(..))
import ElementIds
import Export.Anki
import Export.Json
import Export.Markdown
import Extras.BrowserDom
import Extras.Html
import Extras.HtmlAttribute
import Extras.HtmlEvents
import Extras.HtmlTree as HtmlTree
import Extras.Http
import Extras.Task
import Extras.Url exposing (fragmentOnly)
import Html
import Html.Attributes exposing (class, for, href, id, readonly)
import Html.Events
import Http
import Icons
import Internationalisation as I18n
import PageMsg exposing (PageMsg)
import Process
import QueryParameters
import Save
import Search
import Svg.Attributes exposing (fill, height, stroke, width)
import Task



-- MODEL


type GradualVisibility
    = Visible
    | Disappearing
    | Invisible


type alias MenuForMobileVisibility =
    GradualVisibility


type alias SearchDialog =
    { term : String
    , results : List Components.SearchDialog.SearchResult
    , model : Components.SearchDialog.Model (PageMsg InternalMsg)
    }


type Layout
    = ShowAllItems
    | ShowSingleItem


type alias Model =
    { common : CommonModel
    , menuForMobileVisibility : MenuForMobileVisibility
    , themeDropdownMenu : Components.DropdownMenu.Model
    , exportDropdownMenu : Components.DropdownMenu.Model
    , searchDialog : SearchDialog
    , layout : Layout
    , confirmDeleteId : Maybe GlossaryItemId
    , deleting : Saving
    , savingSettings : Saving
    , mostRecentTermIdForOrderingItemsFocusedOn : Maybe TermId
    , resultOfAttemptingToCopyEditorCommandToClipboard : Maybe Bool
    }


type InternalMsg
    = NoOp
    | MakeChanges
    | ShowMenuForMobile
    | StartHidingMenuForMobile
    | CompleteHidingMenuForMobile
    | BackToTop Bool
    | ThemeDropdownMenuMsg Components.DropdownMenu.Msg
    | ExportDropdownMenuMsg Components.DropdownMenu.Msg
    | SearchDialogMsg Components.SearchDialog.Msg
    | HideSearchDialog
    | UpdateSearchString String
    | ChangeTheme Theme
    | ChangeLayoutToShowSingle GlossaryItemId
    | ShowRelatedTermAsSingle Term
    | ChangeLayoutToShowAll
    | ConfirmDelete GlossaryItemId
    | CancelDelete
    | Delete GlossaryItemId
    | Deleted GlossaryItems
    | FailedToDelete Http.Error
    | JumpToTermIndexGroup Bool String
    | ChangeOrderItemsBy OrderItemsBy
    | ChangeCardWidth CardWidth
    | ToggleEnableExportMenu
    | ToggleEnableOrderItemsButtons
    | ToggleEnableLastUpdatedDates
    | ChangedSettings CommonModel
    | FailedToChangeSettings Http.Error
    | DownloadMarkdown
    | DownloadAnki
    | DownloadJson
    | SelectAllInTextFieldWithCommandToRunEditor
    | CopyEditorCommandToClipboard String
    | AttemptedToCopyEditorCommandToClipboard Bool
    | ClearResultOfAttemptingToCopyEditorCommandToClipboard
    | FilterByTag Tag
    | DoNotFilterByTag


type alias Msg =
    PageMsg InternalMsg


init : CommonModel -> ( Model, Cmd Msg )
init commonModel =
    ( { common = commonModel
      , menuForMobileVisibility = Invisible
      , layout = ShowAllItems
      , confirmDeleteId = Nothing
      , themeDropdownMenu =
            Components.DropdownMenu.init
                [ Components.DropdownMenu.id ElementIds.themeDropdownButton ]
      , exportDropdownMenu =
            Components.DropdownMenu.init
                [ Components.DropdownMenu.id ElementIds.exportDropdownButton ]
      , searchDialog =
            { term = ""
            , results = []
            , model =
                Components.SearchDialog.init ElementIds.searchDialog
                    [ Components.SearchDialog.onChangeSearchString (PageMsg.Internal << UpdateSearchString)
                    , Components.SearchDialog.onShow <| preventBackgroundScrolling ()
                    , Components.SearchDialog.onHide <| Extras.Task.messageToCommand <| PageMsg.Internal HideSearchDialog
                    ]
            }
      , deleting = NotSaving
      , savingSettings = NotSaving
      , mostRecentTermIdForOrderingItemsFocusedOn =
            case QueryParameters.orderItemsBy commonModel.queryParameters of
                FocusedOn termId ->
                    Just termId

                _ ->
                    Nothing
      , resultOfAttemptingToCopyEditorCommandToClipboard = Nothing
      }
    , case commonModel.maybeId of
        Just id ->
            scrollGlossaryItemIntoView id

        Nothing ->
            commonModel.fragment
                |> Maybe.map (Extras.BrowserDom.scrollElementIntoView <| PageMsg.Internal NoOp)
                |> Maybe.withDefault Cmd.none
    )



-- PORTS


port allowBackgroundScrolling : () -> Cmd msg


port preventBackgroundScrolling : () -> Cmd msg


port changeTheme : Maybe String -> Cmd msg


port scrollElementIntoView : String -> Cmd msg


port copyEditorCommandToClipboard : String -> Cmd msg


port attemptedToCopyEditorCommandToClipboard : (Bool -> msg) -> Sub msg


port selectAllInTextFieldWithCommandToRunEditor : () -> Cmd msg



-- UPDATE


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        MakeChanges ->
            let
                common0 =
                    model.common

                editability1 =
                    Editability.startEditing model.common.editability
            in
            ( { model | common = { common0 | editability = editability1 } }, Cmd.none )

        ShowMenuForMobile ->
            ( { model | menuForMobileVisibility = Visible }
            , Cmd.batch
                [ preventBackgroundScrolling ()
                , Extras.BrowserDom.scrollToTopInElement (PageMsg.Internal NoOp) ElementIds.indexForMobile
                ]
            )

        StartHidingMenuForMobile ->
            ( { model | menuForMobileVisibility = Disappearing }
            , Cmd.batch
                [ Process.sleep 100 |> Task.perform (always <| PageMsg.Internal CompleteHidingMenuForMobile)
                , allowBackgroundScrolling ()
                ]
            )

        CompleteHidingMenuForMobile ->
            ( { model | menuForMobileVisibility = Invisible }, Cmd.none )

        BackToTop staticSidebar ->
            let
                idOfSidebarOrMenu : String
                idOfSidebarOrMenu =
                    if staticSidebar then
                        ElementIds.staticSidebarForDesktop

                    else
                        ElementIds.indexForMobile
            in
            ( { model | menuForMobileVisibility = Disappearing }
            , Cmd.batch
                [ Extras.BrowserDom.scrollToTopInElement (PageMsg.Internal NoOp) idOfSidebarOrMenu
                , Extras.BrowserDom.scrollToTop <| PageMsg.Internal NoOp
                , Process.sleep 100 |> Task.perform (always <| PageMsg.Internal CompleteHidingMenuForMobile)
                , allowBackgroundScrolling ()
                ]
            )

        ThemeDropdownMenuMsg msg_ ->
            Components.DropdownMenu.update
                (\x -> { model | themeDropdownMenu = x, exportDropdownMenu = Components.DropdownMenu.hidden model.exportDropdownMenu })
                (PageMsg.Internal << ThemeDropdownMenuMsg)
                msg_
                model.themeDropdownMenu

        ExportDropdownMenuMsg msg_ ->
            Components.DropdownMenu.update
                (\x -> { model | themeDropdownMenu = Components.DropdownMenu.hidden model.themeDropdownMenu, exportDropdownMenu = x })
                (PageMsg.Internal << ExportDropdownMenuMsg)
                msg_
                model.exportDropdownMenu

        SearchDialogMsg msg_ ->
            Components.SearchDialog.update
                (\x ->
                    let
                        searchDialog0 : SearchDialog
                        searchDialog0 =
                            model.searchDialog
                    in
                    { model | searchDialog = { searchDialog0 | model = x } }
                )
                (PageMsg.Internal << SearchDialogMsg)
                msg_
                model.searchDialog.model

        HideSearchDialog ->
            ( let
                searchDialog0 : SearchDialog
                searchDialog0 =
                    model.searchDialog
              in
              { model | searchDialog = { searchDialog0 | term = "", results = [] } }
            , allowBackgroundScrolling ()
            )

        UpdateSearchString searchString ->
            ( let
                searchDialog0 : SearchDialog
                searchDialog0 =
                    model.searchDialog

                results : List Components.SearchDialog.SearchResult
                results =
                    case model.common.glossary of
                        Ok glossary ->
                            glossary
                                |> Glossary.items
                                |> Search.search model.common.enableMathSupport (filterByTag model) searchString

                        Err _ ->
                            []
              in
              { model
                | searchDialog =
                    { searchDialog0
                        | term = searchString
                        , results = results
                    }
              }
            , Cmd.none
            )

        ChangeTheme theme ->
            let
                common0 =
                    model.common
            in
            ( { model
                | common = { common0 | theme = theme }
                , themeDropdownMenu = Components.DropdownMenu.hidden model.themeDropdownMenu
              }
            , changeTheme <|
                case theme of
                    Light ->
                        Just "light"

                    Dark ->
                        Just "dark"

                    System ->
                        Nothing
            )

        ChangeLayoutToShowSingle index ->
            let
                common0 =
                    model.common
            in
            ( { model
                | common = { common0 | maybeId = Just index }
                , layout = ShowSingleItem
              }
            , preventBackgroundScrolling ()
            )

        ShowRelatedTermAsSingle relatedTerm ->
            let
                model1 =
                    case model.common.glossary of
                        Ok glossary ->
                            glossary
                                |> Glossary.items
                                |> GlossaryItems.itemIdFromDisambiguatedPreferredTermId (Term.id relatedTerm)
                                |> Maybe.map
                                    (\index ->
                                        let
                                            common0 =
                                                model.common
                                        in
                                        { model
                                            | common = { common0 | maybeId = Just index }
                                        }
                                    )
                                |> Maybe.withDefault model

                        _ ->
                            model
            in
            ( model1, Cmd.none )

        ChangeLayoutToShowAll ->
            ( { model | layout = ShowAllItems }
            , Cmd.batch
                [ allowBackgroundScrolling ()
                , model.common.maybeId
                    |> Maybe.map (ElementIds.glossaryItemDiv >> scrollElementIntoView)
                    |> Maybe.withDefault Cmd.none
                ]
            )

        ConfirmDelete index ->
            ( { model | confirmDeleteId = Just index }, preventBackgroundScrolling () )

        CancelDelete ->
            if model.confirmDeleteId /= Nothing then
                ( { model
                    | confirmDeleteId = Nothing
                    , deleting = NotSaving
                  }
                , allowBackgroundScrolling ()
                )

            else
                ( { model | deleting = NotSaving }, Cmd.none )

        Delete id ->
            case model.common.glossary of
                Ok glossary ->
                    let
                        updatedGlossaryItems : Result String GlossaryItems
                        updatedGlossaryItems =
                            GlossaryItems.remove id (Glossary.items glossary)
                    in
                    case updatedGlossaryItems of
                        Ok updatedGlossaryItems_ ->
                            let
                                glossary1 : Glossary.Glossary
                                glossary1 =
                                    glossary |> Glossary.setItems updatedGlossaryItems_
                            in
                            ( { model
                                | deleting = SavingInProgress
                                , savingSettings = NotSaving
                              }
                            , Save.save
                                model.common.editability
                                glossary1
                                (PageMsg.Internal << FailedToDelete)
                                (PageMsg.Internal <| Deleted updatedGlossaryItems_)
                            )

                        Err error ->
                            ( { model
                                | deleting = SavingFailed error
                                , savingSettings = NotSaving
                              }
                            , Cmd.none
                            )

                _ ->
                    ( model, Cmd.none )

        Deleted updatedGlossaryItems ->
            let
                common : CommonModel
                common =
                    model.common

                cmd =
                    Cmd.batch
                        [ allowBackgroundScrolling ()
                        , giveFocusToOuter
                        ]
            in
            case common.glossary of
                Ok glossary ->
                    ( { model
                        | common = { common | glossary = Ok <| Glossary.setItems updatedGlossaryItems glossary }
                        , confirmDeleteId = Nothing
                        , deleting = NotSaving
                        , savingSettings = NotSaving
                      }
                    , cmd
                    )

                Err _ ->
                    ( { model
                        | confirmDeleteId = Nothing
                        , deleting = NotSaving
                        , savingSettings = NotSaving
                      }
                    , cmd
                    )

        FailedToDelete error ->
            ( { model
                | deleting = SavingFailed <| Extras.Http.httpErrorDescriptionAskingToReloadOnConflict error
                , savingSettings = NotSaving
              }
            , Cmd.none
            )

        JumpToTermIndexGroup staticSidebar termIndexGroupLabel ->
            let
                idOfSidebarOrMenu : String
                idOfSidebarOrMenu =
                    if staticSidebar then
                        ElementIds.staticSidebarForDesktop

                    else
                        ElementIds.indexForMobile
            in
            ( model
            , Dom.getViewportOf idOfSidebarOrMenu
                |> Task.andThen
                    (\viewport ->
                        termIndexGroupLabel
                            |> ElementIds.termIndexGroupLabel staticSidebar
                            |> Dom.getElement
                            |> Task.andThen
                                (\termIndexGroupElement ->
                                    if staticSidebar then
                                        ElementIds.quickSearchButtonAndLetterGrid
                                            |> Dom.getElement
                                            |> Task.andThen
                                                (\quickSearchButtonAndLetterGridElement ->
                                                    let
                                                        height : Float
                                                        height =
                                                            quickSearchButtonAndLetterGridElement.element.height
                                                    in
                                                    Dom.setViewportOf idOfSidebarOrMenu 0 (viewport.viewport.y + termIndexGroupElement.element.y - termIndexGroupElement.viewport.y - height)
                                                        |> Task.onError
                                                            (always <| Task.succeed ())
                                                )

                                    else
                                        Dom.setViewportOf idOfSidebarOrMenu 0 (viewport.viewport.y + termIndexGroupElement.element.y - termIndexGroupElement.viewport.y - 25)
                                            |> Task.onError
                                                (always <| Task.succeed ())
                                )
                    )
                |> Task.attempt (always <| PageMsg.Internal NoOp)
            )

        ChangeOrderItemsBy orderItemsBy ->
            let
                common : CommonModel
                common =
                    model.common

                mostRecentTermIdForOrderingItemsFocusedOn1 : Maybe TermId
                mostRecentTermIdForOrderingItemsFocusedOn1 =
                    case orderItemsBy of
                        FocusedOn termId ->
                            Just termId

                        _ ->
                            model.mostRecentTermIdForOrderingItemsFocusedOn

                updatedQueryParameters =
                    QueryParameters.setOrderItemsBy orderItemsBy model.common.queryParameters

                common1 =
                    { common | queryParameters = updatedQueryParameters }
            in
            ( { model
                | common = common1
                , mostRecentTermIdForOrderingItemsFocusedOn = mostRecentTermIdForOrderingItemsFocusedOn1
              }
            , common1
                |> CommonModel.relativeUrl
                |> Navigation.pushUrl model.common.key
            )

        ChangeCardWidth cardWidth ->
            case model.common.glossary of
                Ok glossary ->
                    let
                        glossary1 : Glossary
                        glossary1 =
                            glossary |> Glossary.setCardWidth cardWidth

                        common0 =
                            model.common

                        common1 =
                            { common0 | glossary = Ok glossary1 }
                    in
                    ( { model
                        | confirmDeleteId = Nothing
                        , deleting = NotSaving
                        , savingSettings = SavingInProgress
                      }
                    , Save.save
                        common1.editability
                        glossary1
                        (PageMsg.Internal << FailedToChangeSettings)
                        (PageMsg.Internal <| ChangedSettings common1)
                    )

                _ ->
                    ( model, Cmd.none )

        ToggleEnableExportMenu ->
            case model.common.glossary of
                Ok glossary ->
                    let
                        glossary1 =
                            glossary
                                |> Glossary.setEnableExportMenu (not <| Glossary.enableExportMenu glossary)
                    in
                    ( { model
                        | confirmDeleteId = Nothing
                        , deleting = NotSaving
                        , savingSettings = SavingInProgress
                      }
                    , Save.save
                        model.common.editability
                        glossary1
                        (PageMsg.Internal << FailedToChangeSettings)
                        (PageMsg.Internal <| ChangedSettings model.common)
                    )

                _ ->
                    ( model, Cmd.none )

        ToggleEnableOrderItemsButtons ->
            case model.common.glossary of
                Ok glossary ->
                    let
                        glossary1 =
                            glossary
                                |> Glossary.setEnableOrderItemsButtons (not <| Glossary.enableOrderItemsButtons glossary)
                    in
                    ( { model
                        | confirmDeleteId = Nothing
                        , deleting = NotSaving
                        , savingSettings = SavingInProgress
                      }
                    , Save.save
                        model.common.editability
                        glossary1
                        (PageMsg.Internal << FailedToChangeSettings)
                        (PageMsg.Internal <| ChangedSettings model.common)
                    )

                _ ->
                    ( model, Cmd.none )

        ToggleEnableLastUpdatedDates ->
            case model.common.glossary of
                Ok glossary ->
                    let
                        glossary1 : Glossary
                        glossary1 =
                            glossary
                                |> Glossary.setEnableLastUpdatedDates (not <| Glossary.enableLastUpdatedDates glossary)

                        common0 =
                            model.common

                        common1 =
                            { common0 | glossary = Ok glossary1 }
                    in
                    ( { model
                        | confirmDeleteId = Nothing
                        , deleting = NotSaving
                        , savingSettings = SavingInProgress
                      }
                    , Save.save
                        common1.editability
                        glossary1
                        (PageMsg.Internal << FailedToChangeSettings)
                        (PageMsg.Internal <| ChangedSettings common1)
                    )

                _ ->
                    ( model, Cmd.none )

        ChangedSettings common ->
            ( { model | common = common, savingSettings = NotSaving }
            , if common.editability == Editability.EditingInMemory then
                Cmd.none

              else
                Navigation.reload
            )

        FailedToChangeSettings error ->
            ( { model
                | savingSettings = SavingFailed <| Extras.Http.httpErrorDescriptionAskingToReloadOnConflict error
              }
            , Cmd.none
            )

        DownloadMarkdown ->
            ( { model | exportDropdownMenu = Components.DropdownMenu.hidden model.exportDropdownMenu }
            , case model.common.glossary of
                Ok glossary ->
                    Export.Markdown.download
                        (Glossary.title glossary)
                        (Glossary.aboutSection glossary)
                        (Glossary.items glossary)

                _ ->
                    Cmd.none
            )

        DownloadAnki ->
            ( { model | exportDropdownMenu = Components.DropdownMenu.hidden model.exportDropdownMenu }
            , case model.common.glossary of
                Ok glossary ->
                    Export.Anki.download
                        model.common.enableMathSupport
                        (Glossary.title glossary)
                        (Glossary.aboutSection glossary)
                        (Glossary.items glossary)

                _ ->
                    Cmd.none
            )

        DownloadJson ->
            ( { model | exportDropdownMenu = Components.DropdownMenu.hidden model.exportDropdownMenu }
            , case model.common.glossary of
                Ok glossary ->
                    Export.Json.download glossary

                _ ->
                    Cmd.none
            )

        SelectAllInTextFieldWithCommandToRunEditor ->
            ( model, selectAllInTextFieldWithCommandToRunEditor () )

        CopyEditorCommandToClipboard textToCopy ->
            ( model, copyEditorCommandToClipboard textToCopy )

        AttemptedToCopyEditorCommandToClipboard success ->
            ( { model | resultOfAttemptingToCopyEditorCommandToClipboard = Just success }
            , Process.sleep 1000 |> Task.perform (always <| PageMsg.Internal ClearResultOfAttemptingToCopyEditorCommandToClipboard)
            )

        ClearResultOfAttemptingToCopyEditorCommandToClipboard ->
            ( { model | resultOfAttemptingToCopyEditorCommandToClipboard = Nothing }, Cmd.none )

        FilterByTag tag ->
            let
                common0 : CommonModel
                common0 =
                    model.common

                orderItemsBy_ : OrderItemsBy
                orderItemsBy_ =
                    case QueryParameters.orderItemsBy common0.queryParameters of
                        FocusedOn _ ->
                            Alphabetically

                        _ ->
                            QueryParameters.orderItemsBy common0.queryParameters

                common1 =
                    { common0
                        | queryParameters =
                            common0.queryParameters
                                |> QueryParameters.setOrderItemsBy orderItemsBy_
                                |> QueryParameters.setFilterByTag (Just tag)
                    }
            in
            ( { model | common = common1 }
            , Cmd.batch
                [ Extras.BrowserDom.scrollToTop <| PageMsg.Internal NoOp
                , common1
                    |> CommonModel.relativeUrl
                    |> Navigation.pushUrl model.common.key
                ]
            )

        DoNotFilterByTag ->
            let
                common0 : CommonModel
                common0 =
                    model.common

                common1 =
                    { common0
                        | queryParameters =
                            common0.queryParameters
                                |> QueryParameters.setFilterByTag Nothing
                    }
            in
            ( { model | common = common1 }
            , common1
                |> CommonModel.relativeUrl
                |> Navigation.pushUrl model.common.key
            )


filterByTag : Model -> Maybe TagId
filterByTag model =
    case model.common.glossary of
        Ok glossary ->
            let
                queryParametersTag : Maybe Tag
                queryParametersTag =
                    model.common.queryParameters
                        |> QueryParameters.filterByTag
            in
            queryParametersTag
                |> Maybe.andThen
                    (\tag -> glossary |> Glossary.items |> GlossaryItems.tagIdFromTag tag)

        _ ->
            Nothing


giveFocusToOuter : Cmd Msg
giveFocusToOuter =
    Task.attempt (always <| PageMsg.Internal NoOp) (Dom.focus <| ElementIds.outer)


scrollGlossaryItemIntoView : GlossaryItemId -> Cmd Msg
scrollGlossaryItemIntoView =
    ElementIds.glossaryItemDiv >> (Extras.BrowserDom.scrollElementIntoView <| PageMsg.Internal NoOp)



-- VIEW


viewMakingChangesHelp : Maybe Bool -> Maybe String -> Bool -> Html Msg
viewMakingChangesHelp resultOfAttemptingToCopyEditorCommandToClipboard filename tabbable =
    let
        defaultFilename : String
        defaultFilename =
            "glossary.html"

        command =
            "sed -n '/START OF editor.js$/,$p' "
                ++ Maybe.withDefault defaultFilename filename
                ++ (if filename == Just defaultFilename then
                        " | node"

                    else
                        " | FILE=" ++ (filename |> Maybe.withDefault defaultFilename) ++ " node"
                   )
    in
    div
        [ class "mb-5 rounded-md overflow-x-auto bg-amber-50 dark:bg-gray-700 text-gray-700 dark:text-gray-300 print:hidden"
        , class "pt-4 pr-4 pl-4 pb-2"
        ]
        [ details
            [ Accessibility.Key.tabbable tabbable ]
            [ summary
                [ class "mb-1 text-lg leading-6 items-center font-medium text-gray-900 dark:text-gray-100 select-none" ]
                [ span
                    [ class "ml-2" ]
                    [ text I18n.howToMakeChangesTitle ]
                ]
            , div
                [ class "mb-1 max-w-xl" ]
                [ I18n.webInterfaceDescription
                , I18n.runTheFollowingCommand tabbable
                , div
                    [ class "mt-3 flex rounded-md shadow-sm" ]
                    [ div
                        [ class "block w-full flex flex-grow items-stretch focus-within:z-10" ]
                        [ Accessibility.inputText
                            command
                            [ class "w-full min-w-0 rounded-none rounded-l-md focus:ring-indigo-500 focus:border-indigo-500 focus:ring-inset border-gray-300 dark:border-gray-500 dark:bg-gray-700 dark:text-white placeholder-gray-500 dark:placeholder-gray-400 font-mono text-sm"
                            , readonly True
                            , id ElementIds.textFieldWithCommandToRunEditor
                            , Html.Events.onClick <| PageMsg.Internal SelectAllInTextFieldWithCommandToRunEditor
                            ]
                        , Components.Button.white
                            True
                            [ class "rounded-none rounded-r-md border-l-0 focus:ring-2 focus:ring-inset"
                            , Accessibility.Aria.label I18n.copyToClipboard
                            , Extras.HtmlAttribute.showIf (resultOfAttemptingToCopyEditorCommandToClipboard == Nothing) <|
                                (Html.Events.onClick <| PageMsg.Internal <| CopyEditorCommandToClipboard command)
                            ]
                            [ if resultOfAttemptingToCopyEditorCommandToClipboard == Just True then
                                Icons.tick
                                    [ Svg.Attributes.class "h-5 w-5 text-green-700 dark:text-green-300" ]

                              else
                                Icons.copy
                                    [ Svg.Attributes.class "h-5 w-5 text-gray-500 dark:text-gray-300" ]
                            ]
                        ]
                    ]
                , I18n.youCanHideTheseInstructions
                ]
            ]
        ]


viewSettings : Glossary -> Model -> Html Msg
viewSettings glossary model =
    let
        errorDiv : String -> Html msg
        errorDiv message =
            div
                [ class "mt-2" ]
                [ p
                    [ class "text-red-600" ]
                    [ text message ]
                ]
    in
    div
        [ class "mb-5 rounded-md max-w-xl overflow-x-auto bg-amber-50 dark:bg-gray-700 text-gray-700 dark:text-gray-300 print:hidden"
        , class "pt-4 pr-4 pl-4 pb-2"
        ]
        [ details
            [ Accessibility.Key.tabbable <| noModalDialogShown model
            , class "relative"
            ]
            [ Extras.Html.showIf (model.savingSettings == SavingInProgress) <|
                div
                    [ class "absolute top-1/2 bottom-1/2 left-1/2 right-1/2" ]
                    [ Components.Spinner.view
                        [ Svg.Attributes.class "w-12 h-12" ]
                        (model.savingSettings == SavingInProgress)
                    ]
            , summary
                [ class "mb-1 text-lg leading-6 items-center font-medium text-gray-900 dark:text-gray-100 select-none" ]
                [ span
                    [ class "ml-2" ]
                    [ text I18n.settings ]
                ]
            , div
                [ Extras.HtmlAttribute.showIf (model.savingSettings == SavingInProgress) <|
                    class "opacity-25"
                ]
                [ Extras.Html.showIf (model.common.editability == EditingWithIncludedBackend) <|
                    div
                        [ class "mt-6" ]
                        [ p
                            [ class "mt-3 max-w-xl" ]
                            [ text I18n.theseSettingsAreUpdatedInTheHtmlFile
                            ]
                        ]
                , Extras.Html.showIf (model.common.editability == EditingWithIncludedBackend) <|
                    div
                        [ class "mt-6 pb-2" ]
                        [ viewSelectInputSyntax model.common.enableMathSupport ]
                , div
                    [ class "mt-6 pb-2" ]
                    [ viewSelectCardWidth glossary model
                    ]
                , div
                    [ class "mt-6 pb-2" ]
                    [ Components.Button.toggle
                        (Glossary.enableExportMenu glossary)
                        ElementIds.showExportMenuLabel
                        [ Html.Events.onClick <| PageMsg.Internal ToggleEnableExportMenu ]
                        [ span
                            [ class "font-medium text-gray-900 dark:text-gray-300" ]
                            [ text I18n.showExportMenu ]
                        ]
                    ]
                , div
                    [ class "mt-6 pb-2" ]
                    [ Components.Button.toggle
                        (Glossary.enableOrderItemsButtons glossary)
                        ElementIds.showOrderItemsButtons
                        [ Html.Events.onClick <| PageMsg.Internal ToggleEnableOrderItemsButtons ]
                        [ span
                            [ class "font-medium text-gray-900 dark:text-gray-300" ]
                            [ text I18n.showOrderItemsButtons ]
                        ]
                    ]
                , div
                    [ class "mt-6 pb-2" ]
                    [ Components.Button.toggle
                        (Glossary.enableLastUpdatedDates glossary)
                        ElementIds.showLastUpdatedDatesLabel
                        [ Html.Events.onClick <| PageMsg.Internal ToggleEnableLastUpdatedDates ]
                        [ span
                            [ class "font-medium text-gray-900 dark:text-gray-300" ]
                            [ text I18n.showLastUpdatedDates ]
                        ]
                    ]
                , case model.savingSettings of
                    SavingFailed errorMessage ->
                        errorDiv <| I18n.failedToSave ++ " — " ++ errorMessage ++ "."

                    _ ->
                        Extras.Html.nothing
                ]
            ]
        ]


viewTermIndexItem : Bool -> Bool -> IndexOfTerms.Entry -> List (Html Msg)
viewTermIndexItem enableMathSupport tabbable entry =
    case entry of
        IndexOfTerms.PreferredTerm term ->
            [ li []
                [ Html.a
                    [ class "group block border-l pl-4 -ml-px border-transparent hover:border-slate-400 dark:hover:border-slate-400 font-medium text-slate-700 hover:text-slate-900 dark:text-slate-400 dark:hover:text-slate-300"
                    , Html.Attributes.href <| fragmentOnly <| TermId.toString <| Term.id term
                    , Html.Attributes.target "_self"
                    , Accessibility.Key.tabbable tabbable
                    , Html.Events.onClick <| PageMsg.Internal StartHidingMenuForMobile
                    ]
                    [ Term.view enableMathSupport [] term ]
                ]
            ]

        IndexOfTerms.AlternativeTerm term preferredTerms ->
            li
                [ Html.Attributes.attribute "style" "margin-top: 1rem" ]
                [ Html.span
                    [ class "block border-l pl-4 -ml-px border-transparent select-none" ]
                    [ Term.view enableMathSupport
                        [ class "" ]
                        term
                    ]
                ]
                :: List.indexedMap
                    (\index preferredTerm ->
                        li
                            [ Extras.HtmlAttribute.showIf (index + 1 == List.length preferredTerms) <|
                                Html.Attributes.attribute "style" "margin-bottom: 1rem"
                            , Extras.HtmlAttribute.showIf (index == 0) <|
                                Html.Attributes.attribute "style" "margin-top: 0.25rem"
                            , Extras.HtmlAttribute.showIf (index /= 0) <|
                                Html.Attributes.attribute "style" "margin-top: 0rem"
                            ]
                            [ Html.a
                                [ class "group block border-l pl-4 -ml-px border-transparent hover:border-slate-400 dark:hover:border-slate-400 font-medium text-slate-700 hover:text-slate-900 dark:text-slate-400 dark:hover:text-slate-300"
                                , Html.Attributes.href <| fragmentOnly <| TermId.toString <| Term.id preferredTerm
                                , Html.Attributes.target "_self"
                                , Accessibility.Key.tabbable tabbable
                                , Html.Events.onClick <| PageMsg.Internal StartHidingMenuForMobile
                                ]
                                [ span
                                    [ class "inline-flex items-center group-hover:underline" ]
                                    [ Icons.cornerDownRight
                                        [ Svg.Attributes.class "h-5 w-5 shrink-0 pb-0.5 mr-1.5 text-gray-400 dark:text-gray-400 group-hover:text-gray-500 dark:group-hover:text-gray-300"
                                        ]
                                    , Term.view enableMathSupport [] preferredTerm
                                    ]
                                ]
                            ]
                    )
                    preferredTerms


viewTermIndexGroup : Bool -> Bool -> Bool -> TermGroup -> Html Msg
viewTermIndexGroup enableMathSupport tabbable staticSidebar { label, entries } =
    li
        [ id <| ElementIds.termIndexGroupLabel staticSidebar label
        , class "mt-6"
        ]
        [ h5
            [ class "mb-8 lg:mb-3 font-semibold text-slate-700 dark:text-slate-300" ]
            [ text label ]
        , ul
            [ class "space-y-6 lg:space-y-2 border-l border-slate-200 dark:border-slate-600" ]
            (List.concatMap (viewTermIndexItem enableMathSupport tabbable) entries)
        ]


viewIndexOfTerms : Bool -> Bool -> Bool -> IndexOfTerms -> Html Msg
viewIndexOfTerms enableMathSupport tabbable staticSidebar indexOfTerms =
    let
        termGroups : List TermGroup
        termGroups =
            IndexOfTerms.termGroups indexOfTerms
    in
    ul
        [ id <| ElementIds.termsIndex staticSidebar
        , class "mb-10"
        ]
        (List.filterMap
            (\termIndexGroup ->
                if List.isEmpty termIndexGroup.entries then
                    Nothing

                else
                    Just <| viewTermIndexGroup enableMathSupport tabbable staticSidebar termIndexGroup
            )
            termGroups
        )


viewGlossaryItem :
    { enableMathSupport : Bool
    , tabbable : Bool
    , editable : Bool
    , enableLastUpdatedDates : Bool
    , shownAsSingle : Bool
    }
    -> Model
    -> GlossaryItemWithPreviousAndNext
    -> Html Msg
viewGlossaryItem { enableMathSupport, tabbable, editable, enableLastUpdatedDates, shownAsSingle } model itemWithPreviousAndNext =
    let
        common : CommonModel
        common =
            model.common
    in
    Extras.Html.showMaybe
        (\( id, _ ) ->
            Components.GlossaryItemCard.view
                { enableMathSupport = enableMathSupport, makeLinksTabbable = tabbable, enableLastUpdatedDates = enableLastUpdatedDates }
                (Components.GlossaryItemCard.Normal
                    { tabbable = tabbable
                    , onClickViewFull = PageMsg.Internal <| ChangeLayoutToShowSingle id
                    , onClickEdit = PageMsg.NavigateToCreateOrEdit { common | maybeId = Just id }
                    , onClickDelete = PageMsg.Internal <| ConfirmDelete id
                    , onClickTag = PageMsg.Internal << FilterByTag
                    , onClickItem = PageMsg.Internal << ChangeLayoutToShowSingle
                    , onClickRelatedTerm = PageMsg.Internal << ShowRelatedTermAsSingle
                    , editable = editable
                    , shownAsSingle = shownAsSingle
                    }
                )
                itemWithPreviousAndNext
        )
        itemWithPreviousAndNext.item


itemWithPreviousAndNextForId : GlossaryItemId -> List ( GlossaryItemId, GlossaryItemForHtml ) -> GlossaryItemWithPreviousAndNext
itemWithPreviousAndNextForId id indexedGlossaryItems =
    let
        indexedGlossaryItemsArray =
            Array.fromList indexedGlossaryItems

        indexed =
            Array.indexedMap Tuple.pair indexedGlossaryItemsArray
    in
    indexed
        |> Array.foldl
            (\( artificialIndex, ( id0, item0 ) ) { previous, item, next } ->
                if id == id0 then
                    { previous = Array.get (artificialIndex - 1) indexedGlossaryItemsArray
                    , item = Just ( id0, item0 )
                    , next = Array.get (artificialIndex + 1) indexedGlossaryItemsArray
                    }

                else
                    { previous = previous, item = item, next = next }
            )
            { previous = Nothing, item = Nothing, next = Nothing }


viewSingleItemModalDialog :
    Model
    -> { enableMathSupport : Bool, editable : Bool, tabbable : Bool, enableLastUpdatedDates : Bool }
    -> List ( GlossaryItemId, GlossaryItemForHtml )
    -> Maybe GlossaryItemId
    -> Html Msg
viewSingleItemModalDialog model { enableMathSupport, editable, tabbable, enableLastUpdatedDates } indexedGlossaryItems =
    Maybe.map
        (\id ->
            let
                itemWithPreviousAndNext =
                    itemWithPreviousAndNextForId id indexedGlossaryItems
            in
            Components.ModalDialog.view
                (PageMsg.Internal ChangeLayoutToShowAll)
                ElementIds.viewSingleItemModalTitle
                True
                [ class "relative max-w-xl lg:max-w-3xl mx-1.5 bg-white dark:bg-gray-800" ]
                (Html.div
                    []
                    [ Html.div
                        [ class "absolute right-0 top-0 pr-4 pt-4" ]
                        [ Components.Button.text
                            [ Accessibility.Key.tabbable tabbable
                            , Html.Events.onClick <| PageMsg.Internal ChangeLayoutToShowAll
                            ]
                            [ Icons.xMark
                                [ Svg.Attributes.class "h-5 w-5 text-gray-400 dark:text-gray-300 hover:text-gray-500 dark:hover:text-gray-400" ]
                            ]
                        ]
                    , Html.dl
                        [ Html.Attributes.style "display" "block" ]
                        [ viewGlossaryItem
                            { enableMathSupport = enableMathSupport
                            , tabbable = tabbable
                            , editable = editable
                            , enableLastUpdatedDates = enableLastUpdatedDates
                            , shownAsSingle = True
                            }
                            model
                            itemWithPreviousAndNext
                        ]
                    ]
                )
                True
        )
        >> Maybe.withDefault
            (Components.ModalDialog.view
                (PageMsg.Internal NoOp)
                ElementIds.viewSingleItemModalTitle
                True
                []
                Extras.Html.nothing
                False
            )


viewConfirmDeleteModal : Editability -> Maybe GlossaryItemId -> Saving -> Html Msg
viewConfirmDeleteModal editability maybeIdOfItemToDelete deleting =
    Components.ModalDialog.view
        (PageMsg.Internal CancelDelete)
        ElementIds.confirmDeleteModalTitle
        False
        [ class "sm:max-w-lg bg-white dark:bg-gray-700" ]
        (Html.div []
            [ div
                [ class "sm:flex sm:items-start" ]
                [ div
                    [ class "mx-auto shrink-0 flex items-center justify-center h-12 w-12 rounded-full bg-red-100 dark:bg-red-300 sm:mx-0 sm:h-10 sm:w-10" ]
                    [ Icons.exclamation
                        [ Svg.Attributes.class "h-6 w-6 text-red-600 dark:text-red-800"
                        , Accessibility.Aria.hidden True
                        ]
                    ]
                , div
                    [ class "mt-3 text-center sm:mt-0 sm:ml-4 sm:text-left" ]
                    [ h2
                        [ class "text-lg leading-6 font-medium text-gray-900 dark:text-gray-100"
                        , id ElementIds.confirmDeleteModalTitle
                        ]
                        [ text I18n.deleteItem
                        ]
                    , div
                        [ class "mt-2" ]
                        [ p
                            [ class "text-sm text-gray-500 dark:text-gray-400" ]
                            [ text I18n.areYouSureYouWantToDeleteThisItem ]
                        ]
                    ]
                ]
            , Extras.Html.showIf (editability == Editability.EditingInMemory) <|
                div
                    [ class "mt-5 sm:mt-4 text-sm text-gray-500 dark:text-gray-400 sm:text-right" ]
                    [ text I18n.savingChangesInMemoryMessage ]
            , case deleting of
                SavingFailed errorMessage ->
                    div
                        [ class "flex justify-end mt-2" ]
                        [ p
                            [ class "text-red-600 dark:text-red-400" ]
                            [ text <| I18n.failedToSave ++ " — " ++ errorMessage ++ "." ]
                        ]

                _ ->
                    Extras.Html.nothing
            , div
                [ class "mt-5 sm:mt-4 sm:flex sm:flex-row-reverse sm:items-center" ]
                [ Components.Button.primary
                    (deleting /= SavingInProgress)
                    [ class "w-full bg-red-600 dark:bg-red-400 focus:ring-red-500 sm:ml-3 sm:w-auto sm:text-sm dark:text-gray-800"
                    , Extras.HtmlAttribute.showIf (deleting /= SavingInProgress) <| class "hover:bg-red-700 dark:hover:bg-red-600"
                    , Extras.HtmlAttribute.showMaybe
                        (Html.Events.onClick << PageMsg.Internal << Delete)
                        maybeIdOfItemToDelete
                    ]
                    [ text I18n.delete ]
                , Components.Button.white
                    (deleting /= SavingInProgress)
                    [ class "mt-3 w-full sm:mt-0 sm:w-auto sm:text-sm"
                    , Html.Events.onClick <| PageMsg.Internal CancelDelete
                    , Extras.HtmlEvents.onEnter <| PageMsg.Internal CancelDelete
                    ]
                    [ text I18n.cancel ]
                , span
                    [ class "w-full sm:w-auto sm:order-last sm:mr-3 flex justify-center" ]
                    [ Components.Spinner.view
                        [ Svg.Attributes.class "mt-3 sm:mt-0 w-8 h-8" ]
                        (deleting == SavingInProgress)
                    ]
                ]
            ]
        )
        (maybeIdOfItemToDelete /= Nothing)


viewMakeChangesButton : Editability -> Bool -> Html Msg
viewMakeChangesButton editability tabbable =
    div
        [ class "mb-4 flex-none print:hidden" ]
        [ Components.Button.white True
            [ Html.Events.onClick <| PageMsg.Internal MakeChanges
            , Accessibility.Key.tabbable tabbable
            ]
            [ Icons.pencil
                [ Svg.Attributes.class "h-5 w-5" ]
            , span
                [ class "ml-2 inline-flex items-center" ]
                [ text I18n.makeChanges
                , Html.kbd
                    [ class "ml-2 inline-flex items-center rounded border border-gray-700 dark:border-gray-300 px-1 font-sans text-xs" ]
                    [ text "e" ]
                ]
            ]
        , Extras.Html.showIf (editability == Editability.EditingInMemory) <|
            div
                [ class "my-4 text-sm text-gray-500 dark:text-gray-400" ]
                [ text I18n.savingChangesInMemoryMessage ]
        ]


viewEditTitleAndAboutButton : Bool -> CommonModel -> Html Msg
viewEditTitleAndAboutButton tabbable common =
    div
        [ class "pb-3 print:hidden" ]
        [ Components.Button.text
            [ Html.Events.onClick <| PageMsg.NavigateToEditTitleAndAbout { common | maybeId = Nothing }
            , Accessibility.Key.tabbable tabbable
            ]
            [ Icons.pencil
                [ Svg.Attributes.class "h-5 w-5 text-gray-400 dark:text-gray-300 hover:text-gray-500 dark:hover:text-gray-400" ]
            , span
                [ class "ml-2" ]
                [ text I18n.editTitleAndAboutSectionButton ]
            ]
        ]


viewCreateGlossaryItemButtonForEmptyState : Bool -> CommonModel -> Html Msg
viewCreateGlossaryItemButtonForEmptyState tabbable common =
    div
        [ class "pt-4 print:hidden" ]
        [ Components.Button.emptyState
            [ class "p-9"
            , Html.Events.onClick <| PageMsg.NavigateToCreateOrEdit { common | maybeId = Nothing }
            , Accessibility.Key.tabbable tabbable
            ]
            [ Icons.viewGridAdd
                [ Svg.Attributes.class "mx-auto h-12 w-12 text-gray-400" ]
            , span
                [ class "mt-2 inline-flex items-center font-medium text-gray-900 dark:text-gray-200" ]
                [ text I18n.createANewGlossaryItem
                , Html.kbd
                    [ class "ml-2 rounded border border-indigo-700 dark:border-indigo-300 px-1 font-sans text-xs" ]
                    [ text "n" ]
                ]
            ]
        ]


viewCreateGlossaryItemButton : Bool -> CommonModel -> Html Msg
viewCreateGlossaryItemButton tabbable common =
    div
        [ class "pb-2 print:hidden" ]
        [ Components.Button.secondary
            [ Html.Events.onClick <| PageMsg.NavigateToCreateOrEdit { common | maybeId = Nothing }
            , Accessibility.Key.tabbable tabbable
            ]
            [ Icons.viewGridAdd
                [ Svg.Attributes.class "mx-auto -ml-1 mr-2 h-5 w-5"
                , fill "currentColor"
                , stroke "none"
                ]
            , span
                [ class "inline-flex items-center" ]
                [ text I18n.createANewGlossaryItem
                , Html.kbd
                    [ class "ml-2 inline-flex items-center rounded border border-indigo-700 dark:border-indigo-300 px-1 font-sans text-xs" ]
                    [ text "n" ]
                ]
            ]
        ]


viewCards :
    Model
    ->
        { enableMathSupport : Bool
        , enableOrderItemsButtons : Bool
        , editable : Bool
        , tabbable : Bool
        , enableLastUpdatedDates : Bool
        }
    -> List Tag
    -> GlossaryItems
    -> ( List ( GlossaryItemId, GlossaryItemForHtml ), List ( GlossaryItemId, GlossaryItemForHtml ) )
    -> Html Msg
viewCards model { enableMathSupport, enableOrderItemsButtons, editable, tabbable, enableLastUpdatedDates } tags glossaryItems ( indexedGlossaryItems, otherIndexedGlossaryItems ) =
    let
        filterByTag_ : Maybe TagId
        filterByTag_ =
            filterByTag model

        combinedIndexedGlossaryItems : List ( GlossaryItemId, GlossaryItemForHtml )
        combinedIndexedGlossaryItems =
            List.append indexedGlossaryItems otherIndexedGlossaryItems

        disambiguatedPreferredTermsWithDefinitions : List Term
        disambiguatedPreferredTermsWithDefinitions =
            GlossaryItems.disambiguatedPreferredTermsWhichHaveDefinitions
                filterByTag_
                glossaryItems

        orderItemsFocusedOnTerm : Maybe Term
        orderItemsFocusedOnTerm =
            case QueryParameters.orderItemsBy model.common.queryParameters of
                FocusedOn termId ->
                    GlossaryItems.disambiguatedPreferredTermFromId termId glossaryItems

                _ ->
                    Nothing

        viewIndexedItem : ( GlossaryItemId, GlossaryItemForHtml ) -> Html Msg
        viewIndexedItem indexedItem =
            viewGlossaryItem
                { enableMathSupport = enableMathSupport
                , tabbable = tabbable
                , editable = editable
                , enableLastUpdatedDates = enableLastUpdatedDates
                , shownAsSingle = False
                }
                model
                { previous = Nothing, item = Just indexedItem, next = Nothing }

        filterByTagWithDescription : Maybe ( Tag, TagDescription )
        filterByTagWithDescription =
            filterByTag_
                |> Maybe.andThen
                    (\tagId ->
                        GlossaryItems.tagFromId tagId glossaryItems
                            |> Maybe.andThen
                                (\tag ->
                                    GlossaryItems.tagDescriptionFromId tagId glossaryItems
                                        |> Maybe.map (\description -> ( tag, description ))
                                )
                    )

        totalNumberOfItems : Int
        totalNumberOfItems =
            List.length combinedIndexedGlossaryItems

        recommendedMaximumNumberOfItems : Int
        recommendedMaximumNumberOfItems =
            500
    in
    Html.article
        [ Html.Attributes.id ElementIds.items
        , Extras.HtmlAttribute.showIf enableOrderItemsButtons <| class "mt-3 pt-2 border-t border-gray-300 dark:border-gray-700"
        ]
        [ Extras.Html.showMaybe
            (viewCurrentTagFilter { enableMathSupport = enableMathSupport, tabbable = tabbable })
            filterByTagWithDescription
        , Extras.Html.showIf (filterByTagWithDescription == Nothing) <|
            viewAllTagFilters { enableMathSupport = enableMathSupport, tabbable = tabbable } tags
        , Extras.Html.showIf (Editability.editing model.common.editability) <|
            div
                [ class "flex-none mt-4" ]
                [ viewManageTagsButton tabbable model.common ]
        , div
            []
            [ Extras.Html.showIf editable <|
                div
                    [ class "pt-2" ]
                    [ if List.isEmpty combinedIndexedGlossaryItems then
                        viewCreateGlossaryItemButtonForEmptyState tabbable model.common

                      else
                        viewCreateGlossaryItemButton tabbable model.common
                    ]
            ]
        , Extras.Html.showIf (editable && totalNumberOfItems > recommendedMaximumNumberOfItems) <|
            I18n.glossaryContainsTooManyItems recommendedMaximumNumberOfItems
        , Extras.Html.showIf
            (List.isEmpty combinedIndexedGlossaryItems && filterByTag_ /= Nothing)
          <|
            div
                [ class "mt-4" ]
                [ text I18n.noMatchingItemsFound ]
        , Extras.Html.showIf
            (enableOrderItemsButtons
                && (not <| List.isEmpty combinedIndexedGlossaryItems)
            )
          <|
            viewOrderItemsBy
                model
                totalNumberOfItems
                enableMathSupport
                disambiguatedPreferredTermsWithDefinitions
                orderItemsFocusedOnTerm
        , Html.dl
            [ class "mt-4" ]
            (List.map viewIndexedItem indexedGlossaryItems)
        , Extras.Html.showIf
            ((not <| List.isEmpty indexedGlossaryItems)
                && (not <| List.isEmpty otherIndexedGlossaryItems)
            )
          <|
            Components.Dividers.withLabel
                [ class "my-10" ]
                I18n.otherItems
        , Extras.Html.showIf (not <| List.isEmpty otherIndexedGlossaryItems) <|
            Html.dl
                []
                (List.map viewIndexedItem otherIndexedGlossaryItems)
        , Components.SearchDialog.view
            (PageMsg.Internal << SearchDialogMsg)
            model.searchDialog.model
            model.searchDialog.term
            model.searchDialog.results
        , viewConfirmDeleteModal
            model.common.editability
            model.confirmDeleteId
            model.deleting
        , viewSingleItemModalDialog
            model
            { enableMathSupport = enableMathSupport
            , editable = editable
            , tabbable = tabbable
            , enableLastUpdatedDates = enableLastUpdatedDates
            }
            combinedIndexedGlossaryItems
          <|
            case ( model.layout, model.common.maybeId ) of
                ( ShowSingleItem, Just id ) ->
                    Just id

                _ ->
                    Nothing
        ]


viewMenuForMobile : Model -> Bool -> Bool -> IndexOfTerms -> Html Msg
viewMenuForMobile model enableMathSupport tabbable termIndex =
    div
        [ class "invisible" |> Extras.HtmlAttribute.showIf (model.menuForMobileVisibility == Invisible)
        , class "fixed inset-0 flex z-40 lg:hidden"
        , Accessibility.Role.dialog
        , Accessibility.Aria.modal True
        ]
        [ Html.div
            [ class "fixed inset-0 bg-gray-600 bg-opacity-75"
            , if model.menuForMobileVisibility == Visible then
                class "transition-opacity motion-reduce:transition-none ease-linear duration-300 opacity-100"

              else
                class "transition-opacity motion-reduce:transition-none ease-linear duration-300 opacity-0"
            , Html.Events.onClick <| PageMsg.Internal StartHidingMenuForMobile
            , Accessibility.Aria.hidden True
            ]
            []
        , div
            [ class "relative flex-1 flex flex-col max-w-xs w-full pt-5 bg-white dark:bg-gray-900"
            , if model.menuForMobileVisibility == Visible then
                class "transition motion-reduce:transition-none ease-in-out duration-300 transform motion-reduce:transform-none translate-x-0"

              else
                class "transition motion-reduce:transition-none ease-in-out duration-300 transform motion-reduce:transform-none -translate-x-full"
            ]
            [ div
                [ class "absolute top-0 right-0 -mr-12 pt-2"
                , if model.menuForMobileVisibility == Visible then
                    class "motion-reduce:transition-none ease-in-out duration-300 opacity-100"

                  else
                    class "motion-reduce:transition-none ease-in-out duration-300 opacity-0"
                ]
                [ button
                    [ Html.Attributes.type_ "button"
                    , class "ml-1 flex items-center justify-center h-10 w-10 rounded-full focus:outline-none focus:ring-2 focus:ring-inset focus:ring-white dark:focus:ring-gray-500"
                    , Html.Events.onClick <| PageMsg.Internal StartHidingMenuForMobile
                    ]
                    [ span
                        [ class "sr-only" ]
                        [ text I18n.closeSidebar
                        ]
                    , Icons.xMark
                        [ Svg.Attributes.class "h-6 w-6 text-white" ]
                    ]
                ]
            , div
                [ id ElementIds.indexForMobile
                , class "flex-1 h-0 overflow-y-auto"
                ]
                [ nav
                    [ class "px-4 pt-1 pb-6" ]
                    [ viewBackToTopLink False tabbable
                    , viewTermIndexFirstCharacterGrid False tabbable termIndex
                    , viewIndexOfTerms enableMathSupport tabbable False termIndex
                    ]
                ]
            ]
        , div
            [ class "shrink-0 w-14", Accessibility.Aria.hidden True ]
            []
        ]


viewBackToTopLink : Bool -> Bool -> Html Msg
viewBackToTopLink staticSidebar tabbable =
    div
        [ class "bg-white dark:bg-slate-900 pb-3 pointer-events-auto text-right" ]
        [ Html.a
            [ href <| fragmentOnly ElementIds.container
            , Extras.HtmlEvents.onClickStopPropagation <|
                PageMsg.Internal <|
                    BackToTop staticSidebar
            , Accessibility.Key.tabbable tabbable
            ]
            [ span
                [ class "inline-flex" ]
                [ text I18n.backToTop
                , Icons.arrowUp
                    [ Svg.Attributes.class "w-5 h-5 ml-2" ]
                ]
            ]
        ]


viewQuickSearchButton : Bool -> Html Msg
viewQuickSearchButton tabbable =
    div
        [ class "px-3 pb-4 bg-white dark:bg-slate-900" ]
        [ div
            [ class "bg-gray-50 dark:bg-slate-900 relative pointer-events-auto" ]
            [ button
                [ Html.Attributes.type_ "button"
                , class "hidden w-full lg:flex items-center text-sm leading-6 text-slate-500 rounded-md ring-1 ring-slate-900/10 shadow-sm py-1.5 pl-2 pr-3 hover:ring-slate-400 dark:hover:ring-slate-600 dark:bg-slate-800 dark:highlight-white/5 dark:hover:bg-slate-800 select-none"
                , Html.Events.onClick <| PageMsg.Internal <| SearchDialogMsg Components.SearchDialog.show
                , Accessibility.Aria.hidden True
                , Accessibility.Key.tabbable tabbable
                ]
                [ Icons.search
                    [ width "24"
                    , height "24"
                    , Svg.Attributes.class "mr-3 flex-none"
                    ]
                , text I18n.quickSearch
                , span
                    [ class "ml-auto pl-3 flex-none text-xs font-semibold" ]
                    [ text I18n.ctrlK
                    ]
                ]
            ]
        ]


viewTermIndexFirstCharacter : Bool -> Bool -> String -> Bool -> Html Msg
viewTermIndexFirstCharacter staticSidebar tabbable firstCharacter enabled =
    Components.Button.white (tabbable && enabled)
        [ class "m-0.5 px-3 py-2 leading-4"
        , Html.Events.onClick <|
            PageMsg.Internal <|
                if enabled then
                    JumpToTermIndexGroup staticSidebar firstCharacter

                else
                    NoOp
        ]
        [ text firstCharacter ]


viewTermIndexFirstCharacterGrid : Bool -> Bool -> IndexOfTerms -> Html Msg
viewTermIndexFirstCharacterGrid staticSidebar tabbable indexOfTerms =
    let
        termGroups : List TermGroup
        termGroups =
            IndexOfTerms.termGroups indexOfTerms
    in
    div
        [ class "bg-white dark:bg-slate-900 select-none pointer-events-auto" ]
        (List.map
            (\termIndexGroup ->
                viewTermIndexFirstCharacter
                    staticSidebar
                    tabbable
                    termIndexGroup.label
                    (not <| List.isEmpty termIndexGroup.entries)
            )
            termGroups
        )


viewQuickSearchButtonAndLetterGrid : Bool -> Bool -> IndexOfTerms -> Html Msg
viewQuickSearchButtonAndLetterGrid staticSidebar tabbable indexOfTerms =
    div
        [ id ElementIds.quickSearchButtonAndLetterGrid
        , class "z-10 -mb-6 sticky top-0 -ml-0.5 pointer-events-none"
        ]
        [ div
            [ class "h-7 bg-white dark:bg-slate-900" ]
            []
        , div
            [ class "pr-4 bg-white dark:bg-slate-900" ]
            [ viewBackToTopLink True tabbable ]
        , viewQuickSearchButton tabbable
        , div
            [ class "px-3 bg-white dark:bg-slate-900" ]
            [ viewTermIndexFirstCharacterGrid staticSidebar tabbable indexOfTerms ]
        , div
            [ class "h-8 bg-gradient-to-b from-white dark:from-slate-900" ]
            []
        ]


viewStaticSidebarForDesktop : Bool -> Bool -> IndexOfTerms -> Html Msg
viewStaticSidebarForDesktop enableMathSupport tabbable termIndex =
    div
        [ class "hidden print:hidden lg:flex lg:flex-col lg:w-64 lg:fixed lg:inset-y-0 lg:border-r lg:border-gray-200 lg:bg-white lg:dark:border-gray-800 lg:dark:bg-gray-900"
        ]
        [ div
            [ id ElementIds.staticSidebarForDesktop
            , class "h-0 flex-1 flex flex-col overflow-y-auto"
            ]
            [ viewQuickSearchButtonAndLetterGrid True tabbable termIndex
            , nav
                [ class "px-3" ]
                [ viewIndexOfTerms enableMathSupport tabbable True termIndex ]
            ]
        ]


viewTopBar : Bool -> Theme -> Components.DropdownMenu.Model -> Maybe Components.DropdownMenu.Model -> Html Msg
viewTopBar tabbable theme themeDropdownMenu maybeExportDropdownMenu =
    div
        [ class "sticky top-0 z-20 shrink-0 flex justify-between h-16 bg-white dark:bg-gray-900 border-b border-gray-200 dark:border-gray-800 lg:hidden print:hidden items-center" ]
        [ div
            [ class "flex-1" ]
            [ button
                [ Html.Attributes.type_ "button"
                , class "px-4 border-r border-gray-200 dark:border-gray-700 text-gray-500 focus:outline-none lg:hidden"
                , Html.Events.onClick <| PageMsg.Internal ShowMenuForMobile
                ]
                [ span
                    [ class "sr-only" ]
                    [ text I18n.openSidebar ]
                , Icons.menu
                    [ Svg.Attributes.class "h-6 w-6"
                    , Accessibility.Aria.hidden True
                    ]
                ]
            ]
        , div
            [ class "pr-4" ]
            [ button
                [ Html.Attributes.type_ "button"
                , class "ml-auto text-slate-500 w-8 h-8 -my-1 flex items-center justify-center hover:text-slate-600 lg:hidden dark:text-slate-400 dark:hover:text-slate-300"
                , Html.Events.onClick <| PageMsg.Internal <| SearchDialogMsg Components.SearchDialog.show
                ]
                [ span
                    [ class "sr-only" ]
                    [ text I18n.search ]
                , Icons.search
                    [ width "24"
                    , height "24"
                    , Accessibility.Aria.hidden True
                    ]
                ]
            ]
        , div
            [ class "flex pr-4" ]
            [ viewThemeButton tabbable theme themeDropdownMenu ]
        , Extras.Html.showMaybe
            (\exportDropdownMenu ->
                div
                    [ class "flex pr-4" ]
                    [ viewExportButton tabbable exportDropdownMenu ]
            )
            maybeExportDropdownMenu
        ]


themeIcon : Theme -> List (Html.Attribute msg) -> Html msg
themeIcon theme =
    case theme of
        Light ->
            Icons.sun

        Dark ->
            Icons.moon

        System ->
            Icons.computerDesktop


viewThemeButton : Bool -> Theme -> Components.DropdownMenu.Model -> Html Msg
viewThemeButton enabled theme themeDropdownMenu =
    Components.DropdownMenu.view
        (PageMsg.Internal << ThemeDropdownMenuMsg)
        themeDropdownMenu
        enabled
        (Components.DropdownMenu.Chevron
            [ themeIcon theme
                [ Svg.Attributes.class "h-5 w-5" ]
            ]
        )
        [ Components.DropdownMenu.choice
            [ span
                [ class "inline-flex items-center" ]
                [ themeIcon Light
                    [ Svg.Attributes.class "h-5 w-5 text-gray-500 dark:text-gray-400 mr-2" ]
                , text I18n.themeLight
                ]
            ]
            (PageMsg.Internal <| ChangeTheme Light)
        , Components.DropdownMenu.choice
            [ span
                [ class "inline-flex items-center" ]
                [ themeIcon Dark
                    [ Svg.Attributes.class "h-5 w-5 text-gray-500 dark:text-gray-400 mr-2" ]
                , text I18n.themeDark
                ]
            ]
            (PageMsg.Internal <| ChangeTheme Dark)
        , Components.DropdownMenu.choice
            [ span
                [ class "inline-flex items-center" ]
                [ themeIcon System
                    [ Svg.Attributes.class "h-5 w-5 text-gray-500 dark:text-gray-400 mr-2" ]
                , text I18n.themeSystem
                ]
            ]
            (PageMsg.Internal <| ChangeTheme System)
        ]


viewExportButton : Bool -> Components.DropdownMenu.Model -> Html Msg
viewExportButton enabled exportDropdownMenu =
    Components.DropdownMenu.view
        (PageMsg.Internal << ExportDropdownMenuMsg)
        exportDropdownMenu
        enabled
        (Components.DropdownMenu.Chevron
            [ Icons.documentDownload
                [ Svg.Attributes.class "h-5 w-5 mr-2" ]
            , text I18n.export
            ]
        )
        [ Components.DropdownMenu.choice
            [ span
                [ class "inline-flex items-center" ]
                [ Icons.anki
                    [ Svg.Attributes.class "h-5 w-5 text-gray-500 dark:text-gray-600 mr-2" ]
                , text I18n.ankiDeck
                ]
            ]
            (PageMsg.Internal <| DownloadAnki)
        , Components.DropdownMenu.choice
            [ span
                [ class "inline-flex items-center" ]
                [ Icons.braces
                    [ Svg.Attributes.class "h-5 w-5 text-gray-500 dark:text-gray-400 mr-2" ]
                , text I18n.json
                ]
            ]
            (PageMsg.Internal <| DownloadJson)
        , Components.DropdownMenu.choice
            [ span
                [ class "inline-flex items-center" ]
                [ Icons.markdown
                    [ Svg.Attributes.class "h-5 w-5 text-gray-500 dark:text-gray-400 mr-2" ]
                , text "Markdown"
                ]
            ]
            (PageMsg.Internal <| DownloadMarkdown)
        ]


viewSelectInputSyntax : Bool -> Html Msg
viewSelectInputSyntax enableMathSupport =
    div
        []
        [ if enableMathSupport then
            I18n.mathSupportIsEnabled

          else
            I18n.howToEnableMathSupport
        ]


viewSelectCardWidth : Glossary -> Model -> Html Msg
viewSelectCardWidth glossary model =
    let
        tabbable : Bool
        tabbable =
            noModalDialogShown model

        cardWidth : CardWidth
        cardWidth =
            Glossary.cardWidth glossary
    in
    div
        []
        [ fieldset []
            [ legend
                [ class "mb-4 font-medium text-gray-900 dark:text-gray-100" ]
                [ text I18n.cardWidth ]
            , div
                [ class "space-y-4 sm:flex sm:items-center sm:space-y-0 sm:space-x-6" ]
                [ div
                    [ class "flex items-center" ]
                    [ Components.Button.radio
                        "card-width"
                        "card-width-compact"
                        (cardWidth == CardWidth.Compact)
                        tabbable
                        [ id ElementIds.cardWidthCompact
                        , Html.Events.onClick <| PageMsg.Internal <| ChangeCardWidth CardWidth.Compact
                        ]
                    , label
                        [ class "ml-3 block font-medium text-gray-700 dark:text-gray-300 select-none"
                        , for ElementIds.cardWidthCompact
                        ]
                        [ text I18n.cardWidthCompact ]
                    ]
                , div
                    [ class "flex items-center" ]
                    [ Components.Button.radio
                        "card-width"
                        "card-width-intermediate"
                        (cardWidth == CardWidth.Intermediate)
                        tabbable
                        [ id ElementIds.cardWidthIntermediate
                        , Html.Events.onClick <| PageMsg.Internal <| ChangeCardWidth CardWidth.Intermediate
                        ]
                    , label
                        [ class "ml-3 block font-medium text-gray-700 dark:text-gray-300 select-none"
                        , for ElementIds.cardWidthIntermediate
                        ]
                        [ text I18n.cardWidthIntermediate ]
                    ]
                , div
                    [ class "flex items-center" ]
                    [ Components.Button.radio
                        "card-width"
                        "card-width-wide"
                        (cardWidth == CardWidth.Wide)
                        tabbable
                        [ id ElementIds.cardWidthWide
                        , Html.Events.onClick <| PageMsg.Internal <| ChangeCardWidth CardWidth.Wide
                        ]
                    , label
                        [ class "ml-3 block font-medium text-gray-700 dark:text-gray-300 select-none"
                        , for ElementIds.cardWidthWide
                        ]
                        [ text I18n.cardWidthWide ]
                    ]
                ]
            ]
        ]


viewCurrentTagFilter : { enableMathSupport : Bool, tabbable : Bool } -> ( Tag, TagDescription ) -> Html Msg
viewCurrentTagFilter { enableMathSupport, tabbable } ( tag, tagDescription ) =
    div
        [ class "pt-3" ]
        [ span
            [ class "print:hidden mr-2 font-medium text-gray-900 dark:text-gray-100" ]
            [ text I18n.onlyShowingItemsForTag ]
        , Components.Badge.indigoWithBorderAndRemoveButton
            tabbable
            [ class "print:hidden mt-2" ]
            (PageMsg.Internal DoNotFilterByTag)
            [ Tag.view enableMathSupport [] tag ]
        , div
            [ class "mt-3 mb-4" ]
            [ TagDescription.view
                { enableMathSupport = enableMathSupport
                , makeLinksTabbable = tabbable
                }
                []
                tagDescription
            ]
        ]


viewAllTagFilters : { enableMathSupport : Bool, tabbable : Bool } -> List Tag -> Html Msg
viewAllTagFilters { enableMathSupport, tabbable } tags =
    Extras.Html.showIf (not <| List.isEmpty tags) <|
        div
            [ class "print:hidden pt-3" ]
            (span
                [ class "mr-2 font-medium text-gray-900 dark:text-gray-100" ]
                [ text <| I18n.tags ++ ":" ]
                :: (tags
                        |> List.map
                            (\tag ->
                                Components.Button.soft
                                    tabbable
                                    [ class "mr-2 mt-2"
                                    , Html.Events.onClick <| PageMsg.Internal <| FilterByTag tag
                                    ]
                                    [ Tag.view enableMathSupport [] tag ]
                            )
                   )
            )


viewManageTagsButton : Bool -> CommonModel -> Html Msg
viewManageTagsButton tabbable common =
    div
        [ class "pb-3 print:hidden" ]
        [ Components.Button.text
            [ Html.Events.onClick <| PageMsg.NavigateToManageTags { common | maybeId = Nothing }
            , Accessibility.Key.tabbable tabbable
            ]
            [ Icons.pencil
                [ Svg.Attributes.class "h-5 w-5 text-gray-400 dark:text-gray-300 hover:text-gray-500 dark:hover:text-gray-400" ]
            , span
                [ class "ml-2" ]
                [ text I18n.manageTags ]
            ]
        ]


viewOrderItemsBy : Model -> Int -> Bool -> List Term -> Maybe Term -> Html Msg
viewOrderItemsBy model numberOfItems enableMathSupport disambiguatedPreferredTermsWithDefinitions orderItemsFocusedOnTerm =
    let
        tabbable : Bool
        tabbable =
            noModalDialogShown model
    in
    div
        [ class "print:hidden pt-4 pb-2" ]
        [ fieldset []
            [ legend
                [ class "mb-4 font-medium text-gray-900 dark:text-gray-100" ]
                [ text I18n.orderItems
                , span
                    [ class "ml-2 text-gray-500 dark:text-gray-400" ]
                    [ text "("
                    , text <| String.fromInt numberOfItems
                    , text ")"
                    ]
                ]
            , div
                [ class "space-y-4 xl:flex xl:items-center xl:space-y-0 xl:space-x-6" ]
                [ div
                    [ class "flex items-center" ]
                    [ Components.Button.radio
                        "order-items-by"
                        "order-items-alphabetically"
                        (QueryParameters.orderItemsBy model.common.queryParameters == Alphabetically)
                        tabbable
                        [ id ElementIds.orderItemsAlphabetically
                        , Html.Events.onClick <| PageMsg.Internal <| ChangeOrderItemsBy Alphabetically
                        ]
                    , label
                        [ class "ml-3 block font-medium text-gray-700 dark:text-gray-300 select-none"
                        , for ElementIds.orderItemsAlphabetically
                        ]
                        [ text I18n.alphabetically ]
                    ]
                , div
                    [ class "flex items-center" ]
                    [ Components.Button.radio
                        "order-items-by"
                        "order-items-most-mentioned-first"
                        (QueryParameters.orderItemsBy model.common.queryParameters == MostMentionedFirst)
                        tabbable
                        [ id ElementIds.orderItemsMostMentionedFirst
                        , Html.Events.onClick <| PageMsg.Internal <| ChangeOrderItemsBy MostMentionedFirst
                        ]
                    , label
                        [ class "ml-3 block font-medium text-gray-700 dark:text-gray-300 select-none"
                        , for ElementIds.orderItemsMostMentionedFirst
                        ]
                        [ text I18n.mostMentionedFirst ]
                    ]
                , div
                    [ class "flex items-center" ]
                    [ Components.Button.radio
                        "order-items-by"
                        "order-items-focused-on"
                        (case QueryParameters.orderItemsBy model.common.queryParameters of
                            FocusedOn _ ->
                                True

                            _ ->
                                False
                        )
                        tabbable
                        [ id ElementIds.orderItemsFocusedOn
                        , Html.Attributes.disabled <| model.mostRecentTermIdForOrderingItemsFocusedOn == Nothing
                        , Extras.HtmlAttribute.showMaybe
                            (\termId ->
                                Html.Events.onClick <| PageMsg.Internal <| ChangeOrderItemsBy <| FocusedOn termId
                            )
                            model.mostRecentTermIdForOrderingItemsFocusedOn
                        ]
                    , label
                        [ class "ml-3 inline-flex items-center font-medium text-gray-700 dark:text-gray-300 select-none"
                        , for ElementIds.orderItemsFocusedOn
                        ]
                        [ span
                            [ class "mr-2" ]
                            [ text I18n.focusedOn
                            ]
                        , Components.SelectMenu.render
                            [ Components.SelectMenu.id <| ElementIds.orderItemsFocusedOnSelect
                            , Components.SelectMenu.ariaLabel I18n.focusOnTerm
                            , Components.SelectMenu.onChange (PageMsg.Internal << ChangeOrderItemsBy << FocusedOn << TermId.fromString)
                            , Components.SelectMenu.enabled tabbable
                            ]
                            (disambiguatedPreferredTermsWithDefinitions
                                |> List.map
                                    (\disambiguatedPreferredTerm ->
                                        let
                                            preferredTermId =
                                                Term.id disambiguatedPreferredTerm
                                        in
                                        Components.SelectMenu.Choice
                                            (TermId.toString preferredTermId)
                                            [ text <| Term.inlineText disambiguatedPreferredTerm ]
                                            (model.mostRecentTermIdForOrderingItemsFocusedOn == Just preferredTermId)
                                    )
                            )
                        ]
                    ]
                ]
            ]
        , Extras.Html.showIf (QueryParameters.orderItemsBy model.common.queryParameters == MostMentionedFirst) <|
            p
                [ class "mt-2 text-gray-700 dark:text-gray-300" ]
                [ text I18n.explanationForMostMentionedFirst ]
        , Extras.Html.showMaybe
            (Term.view enableMathSupport []
                >> I18n.explanationForFocusedOn
            )
            orderItemsFocusedOnTerm
        ]


type MenuOrDialogShown
    = MenuForMobileShown
    | SearchDialogShown
    | ConfirmDeleteModalDialogShown GlossaryItemId
    | ViewSingleItemModalDialogShown GlossaryItemId
    | NoMenuOrDialogShown


menuOrDialogShown : Model -> MenuOrDialogShown
menuOrDialogShown model =
    if Components.SearchDialog.visible model.searchDialog.model then
        SearchDialogShown

    else if model.menuForMobileVisibility == Visible then
        MenuForMobileShown

    else if model.layout == ShowSingleItem then
        model.common.maybeId
            |> Maybe.map ViewSingleItemModalDialogShown
            |> Maybe.withDefault NoMenuOrDialogShown

    else
        model.confirmDeleteId
            |> Maybe.map ConfirmDeleteModalDialogShown
            |> Maybe.withDefault NoMenuOrDialogShown


noModalDialogShown : Model -> Bool
noModalDialogShown model =
    case menuOrDialogShown model of
        MenuForMobileShown ->
            True

        SearchDialogShown ->
            False

        ConfirmDeleteModalDialogShown _ ->
            False

        ViewSingleItemModalDialogShown _ ->
            False

        NoMenuOrDialogShown ->
            True


pageTitle : Model -> Glossary -> String
pageTitle model glossary =
    let
        glossaryTitle =
            glossary |> Glossary.title |> GlossaryTitle.inlineText
    in
    case ( model.layout, model.common.maybeId ) of
        ( ShowSingleItem, Just id ) ->
            let
                disambiguatedPreferredTerm =
                    glossary
                        |> Glossary.items
                        |> GlossaryItems.get id
                        |> Maybe.map
                            (\item ->
                                item
                                    |> GlossaryItemForHtml.disambiguatedPreferredTerm
                                    |> Term.inlineText
                            )
            in
            disambiguatedPreferredTerm
                |> Maybe.map
                    (\disambiguatedPreferredTerm_ ->
                        disambiguatedPreferredTerm_ ++ " · " ++ glossaryTitle
                    )
                |> Maybe.withDefault glossaryTitle

        _ ->
            glossaryTitle


view : Model -> Document Msg
view model =
    case model.common.glossary of
        Ok glossary ->
            let
                noModalDialogShown_ : Bool
                noModalDialogShown_ =
                    noModalDialogShown model

                items : GlossaryItems
                items =
                    Glossary.items glossary

                filterByTag_ : Maybe TagId
                filterByTag_ =
                    filterByTag model

                indexOfTerms : IndexOfTerms
                indexOfTerms =
                    IndexOfTerms.fromGlossaryItems filterByTag_ items
            in
            { title = pageTitle model glossary
            , body =
                [ Html.div
                    [ class "min-h-full focus:outline-none"
                    , Html.Attributes.id ElementIds.outer
                    , Accessibility.Key.tabbable True
                    , Html.Events.preventDefaultOn "keydown"
                        (Extras.HtmlEvents.preventDefaultOnDecoder
                            (\event ->
                                case menuOrDialogShown model of
                                    ConfirmDeleteModalDialogShown index ->
                                        if event == Extras.HtmlEvents.escape then
                                            Just <| ( PageMsg.Internal CancelDelete, True )

                                        else if event == Extras.HtmlEvents.enter then
                                            Just <| ( PageMsg.Internal <| Delete index, True )

                                        else
                                            Nothing

                                    SearchDialogShown ->
                                        if event == Extras.HtmlEvents.escape || event == Extras.HtmlEvents.controlK then
                                            Just <| ( PageMsg.Internal <| SearchDialogMsg Components.SearchDialog.hide, True )

                                        else
                                            Nothing

                                    MenuForMobileShown ->
                                        if event == Extras.HtmlEvents.escape then
                                            Just <| ( PageMsg.Internal StartHidingMenuForMobile, True )

                                        else
                                            Nothing

                                    ViewSingleItemModalDialogShown id ->
                                        if event == Extras.HtmlEvents.escape then
                                            Just <| ( PageMsg.Internal ChangeLayoutToShowAll, True )

                                        else
                                            let
                                                itemWithPreviousAndNext =
                                                    items
                                                        |> (case QueryParameters.orderItemsBy model.common.queryParameters of
                                                                Alphabetically ->
                                                                    GlossaryItems.orderedAlphabetically filterByTag_

                                                                MostMentionedFirst ->
                                                                    GlossaryItems.orderedByMostMentionedFirst filterByTag_

                                                                FocusedOn termId ->
                                                                    \items_ ->
                                                                        GlossaryItems.itemIdFromDisambiguatedPreferredTermId termId items_
                                                                            |> Maybe.map
                                                                                (\itemId -> GlossaryItems.orderedFocusedOn filterByTag_ itemId items_)
                                                                            |> Maybe.withDefault ( [], [] )
                                                                            |> (\( lhs, rhs ) -> List.append lhs rhs)
                                                           )
                                                        |> itemWithPreviousAndNextForId id
                                            in
                                            if event == Extras.HtmlEvents.leftArrow then
                                                itemWithPreviousAndNext.previous
                                                    |> Maybe.map
                                                        (\( newIndex, _ ) -> ( PageMsg.Internal <| ChangeLayoutToShowSingle newIndex, True ))

                                            else if event == Extras.HtmlEvents.rightArrow then
                                                itemWithPreviousAndNext.next
                                                    |> Maybe.map
                                                        (\( newIndex, _ ) -> ( PageMsg.Internal <| ChangeLayoutToShowSingle newIndex, True ))

                                            else
                                                Nothing

                                    NoMenuOrDialogShown ->
                                        if event == Extras.HtmlEvents.controlK then
                                            Just <| ( PageMsg.Internal <| SearchDialogMsg Components.SearchDialog.show, True )

                                        else if Editability.canEdit model.common.editability && event == Extras.HtmlEvents.e then
                                            Just <| ( PageMsg.Internal MakeChanges, True )

                                        else if Editability.editing model.common.editability && event == Extras.HtmlEvents.n then
                                            let
                                                common_ : CommonModel
                                                common_ =
                                                    model.common
                                            in
                                            Just <| ( PageMsg.NavigateToCreateOrEdit { common_ | maybeId = Nothing }, True )

                                        else
                                            Nothing
                            )
                        )
                    ]
                    [ viewMenuForMobile model model.common.enableMathSupport noModalDialogShown_ indexOfTerms
                    , viewStaticSidebarForDesktop model.common.enableMathSupport noModalDialogShown_ indexOfTerms
                    , div
                        [ class "lg:pl-64 flex flex-col" ]
                        [ viewTopBar noModalDialogShown_
                            model.common.theme
                            model.themeDropdownMenu
                            (if Glossary.enableExportMenu glossary then
                                Just model.exportDropdownMenu

                             else
                                Nothing
                            )
                        , div
                            [ Html.Attributes.id ElementIds.container
                            , class "relative"
                            , Extras.HtmlAttribute.fromBool "data-markdown-rendered" True
                            , glossary |> Glossary.cardWidth |> CardWidth.toHtmlTreeAttribute |> HtmlTree.attributeToHtmlAttribute
                            ]
                            [ header [] <|
                                let
                                    showExportButton : Bool
                                    showExportButton =
                                        Glossary.enableExportMenu glossary
                                in
                                [ div
                                    [ class "lg:border-b border-gray-300 dark:border-gray-700 lg:mb-4" ]
                                    [ div
                                        [ class "flex flex-row justify-start lg:justify-end" ]
                                        [ Extras.Html.showIf (Editability.canEdit model.common.editability) <|
                                            viewMakeChangesButton model.common.editability noModalDialogShown_
                                        , div
                                            [ class "hidden lg:block ml-auto pb-3 pt-0.5" ]
                                            [ viewThemeButton noModalDialogShown_ model.common.theme model.themeDropdownMenu
                                            ]
                                        , div
                                            [ class "hidden lg:block" ]
                                            [ Extras.Html.showIf showExportButton <|
                                                span
                                                    [ class "pl-4 pb-3" ]
                                                    [ viewExportButton noModalDialogShown_ model.exportDropdownMenu ]
                                            ]
                                        ]
                                    ]
                                , viewMakingChangesHelp model.resultOfAttemptingToCopyEditorCommandToClipboard model.common.filename noModalDialogShown_
                                    |> Extras.Html.showIf (model.common.editability == ReadOnlyWithHelpForMakingChanges)
                                , Extras.Html.showIf (Editability.editing model.common.editability) <| viewSettings glossary model
                                , h1
                                    [ id ElementIds.title ]
                                    [ glossary |> Glossary.title |> GlossaryTitle.view model.common.enableMathSupport ]
                                ]
                            , Html.main_
                                []
                                [ glossary
                                    |> Glossary.aboutSection
                                    |> Components.AboutSection.view
                                        { enableMathSupport = model.common.enableMathSupport
                                        , modalDialogShown = not noModalDialogShown_
                                        }
                                , Extras.Html.showIf (Editability.editing model.common.editability) <|
                                    div
                                        [ class "flex-none mt-2" ]
                                        [ viewEditTitleAndAboutButton noModalDialogShown_ model.common ]
                                , items
                                    |> (case QueryParameters.orderItemsBy model.common.queryParameters of
                                            Alphabetically ->
                                                GlossaryItems.orderedAlphabetically filterByTag_
                                                    >> (\lhs -> ( lhs, [] ))

                                            MostMentionedFirst ->
                                                GlossaryItems.orderedByMostMentionedFirst filterByTag_
                                                    >> (\lhs -> ( lhs, [] ))

                                            FocusedOn termId ->
                                                let
                                                    itemId : Maybe GlossaryItemId
                                                    itemId =
                                                        GlossaryItems.itemIdFromDisambiguatedPreferredTermId termId items
                                                in
                                                case itemId of
                                                    Just itemId_ ->
                                                        GlossaryItems.orderedFocusedOn filterByTag_ itemId_

                                                    Nothing ->
                                                        always
                                                            (items
                                                                |> GlossaryItems.orderedAlphabetically filterByTag_
                                                                |> (\lhs -> ( lhs, [] ))
                                                            )
                                       )
                                    |> viewCards
                                        model
                                        { enableMathSupport = model.common.enableMathSupport
                                        , enableOrderItemsButtons = Glossary.enableOrderItemsButtons glossary
                                        , editable = Editability.editing model.common.editability
                                        , tabbable = noModalDialogShown_
                                        , enableLastUpdatedDates = Glossary.enableLastUpdatedDates glossary
                                        }
                                        (GlossaryItems.tags items)
                                        items
                                ]
                            , Html.footer
                                []
                                [ div
                                    []
                                    [ I18n.builtUsingGlossaryPageTemplate noModalDialogShown_ ]
                                ]
                            ]
                        ]
                    ]
                ]
            }

        Err error ->
            { title = I18n.glossaryCapitalised
            , body = [ pre [] [ text error ] ]
            }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Components.DropdownMenu.subscriptions model.themeDropdownMenu
            |> Sub.map (ThemeDropdownMenuMsg >> PageMsg.Internal)
        , Components.DropdownMenu.subscriptions model.exportDropdownMenu
            |> Sub.map (ExportDropdownMenuMsg >> PageMsg.Internal)
        , attemptedToCopyEditorCommandToClipboard (AttemptedToCopyEditorCommandToClipboard >> PageMsg.Internal)
        ]
