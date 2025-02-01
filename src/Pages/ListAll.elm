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
import Data.DescribedTag as DescribedTag exposing (DescribedTag)
import Data.Editability as Editability exposing (Editability(..))
import Data.GlossaryChange as GlossaryChange
import Data.GlossaryChangelist as GlossaryChangelist exposing (GlossaryChangelist)
import Data.GlossaryForUi as GlossaryForUi exposing (GlossaryForUi)
import Data.GlossaryItem.DisambiguatedTerm as DisambiguatedTerm exposing (DisambiguatedTerm)
import Data.GlossaryItem.RawTerm as RawTerm exposing (RawTerm)
import Data.GlossaryItem.Tag as Tag exposing (Tag)
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItemForUi as GlossaryItemForUi exposing (GlossaryItemForUi)
import Data.GlossaryItemId as GlossaryItemId exposing (GlossaryItemId)
import Data.GlossaryItemWithPreviousAndNext exposing (GlossaryItemWithPreviousAndNext)
import Data.GlossaryItemsForUi as GlossaryItemsForUi exposing (GlossaryItemsForUi)
import Data.GlossaryTitle as GlossaryTitle
import Data.GradualVisibility as GradualVisibility exposing (GradualVisibility)
import Data.IndexOfTerms as IndexOfTerms exposing (IndexOfTerms, TermGroup)
import Data.OrderItemsBy exposing (OrderItemsBy(..))
import Data.Saving exposing (Saving(..))
import Data.TagDescription as TagDescription
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
import Extras.Url
import Html
import Html.Attributes exposing (class, for, href, id, readonly)
import Html.Events
import Html.Keyed
import Html.Lazy
import Http
import Icons
import Internationalisation as I18n
import PageMsg exposing (PageMsg)
import Process
import QueryParameters exposing (QueryParameters)
import Save
import Search
import Svg.Attributes exposing (fill, height, stroke, width)
import Task



-- MODEL


type alias MenuForMobileVisibility =
    GradualVisibility


type BackToTopLinkVisibility
    = Visible Int
    | Disappearing Int
    | Invisible Int


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
    , backToTopLinkVisibility : BackToTopLinkVisibility
    , themeDropdownMenu : Components.DropdownMenu.Model
    , exportDropdownMenu : Components.DropdownMenu.Model
    , indexFilterString : String
    , searchDialog : SearchDialog
    , layout : Layout
    , itemWithFocus : Maybe GlossaryItemId
    , confirmDeleteId : Maybe GlossaryItemId
    , deleting : Saving
    , savingSettings : Saving
    , mostRecentRawTermForOrderingItemsFocusedOn : Maybe RawTerm
    , resultOfAttemptingToCopyEditorCommandToClipboard : Maybe Bool
    }


type InternalMsg
    = NoOp
    | MakeChanges
    | ShowMenuForMobile
    | StartHidingMenuForMobile
    | CompleteHidingMenuForMobile
    | BackToTop Bool Int
    | ThemeDropdownMenuMsg Components.DropdownMenu.Msg
    | ExportDropdownMenuMsg Components.DropdownMenu.Msg
    | SearchDialogMsg Components.SearchDialog.Msg
    | SearchDialogWasHidden
    | UpdateIndexFilterString String
    | UpdateSearchString String
    | ChangeTheme Theme
    | ScrollingUpWhileFarAwayFromTheTop
    | StartHidingBackToTopLink Int
    | CompleteHidingBackToTopLink Int
    | ImmediatelyHideBackToTopLink
    | JumpToItem GlossaryItemId
    | ChangeLayoutToShowSingle GlossaryItemId
    | ShowRelatedTermAsSingle Term
    | ChangeLayoutToShowAll
    | ConfirmDelete GlossaryItemId
    | CancelDelete
    | Delete GlossaryItemId
    | Deleted GlossaryForUi
    | FailedToDelete Http.Error
    | JumpToTermIndexGroup Bool String
    | ChangeOrderItemsBy OrderItemsBy
    | ChangeCardWidth CardWidth
    | ChangeDefaultTheme Theme
    | ToggleEnableExportMenu
    | ToggleEnableOrderItemsButtons
    | ToggleEnableLastUpdatedDates
    | ChangedSettings GlossaryForUi
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


init : CommonModel -> Maybe GlossaryItemId -> ( Model, Cmd Msg )
init commonModel itemWithFocus =
    ( { common = commonModel
      , menuForMobileVisibility = GradualVisibility.Invisible
      , backToTopLinkVisibility = Invisible 0
      , layout = ShowAllItems
      , itemWithFocus = itemWithFocus
      , confirmDeleteId = Nothing
      , themeDropdownMenu =
            Components.DropdownMenu.init
                [ Components.DropdownMenu.id ElementIds.themeDropdownButton ]
      , exportDropdownMenu =
            Components.DropdownMenu.init
                [ Components.DropdownMenu.id ElementIds.exportDropdownButton ]
      , indexFilterString = ""
      , searchDialog =
            { term = ""
            , results = []
            , model =
                Components.SearchDialog.init ElementIds.searchDialog
                    [ Components.SearchDialog.onChangeSearchString (PageMsg.Internal << UpdateSearchString)
                    , Components.SearchDialog.onShow <|
                        Cmd.batch
                            [ preventBackgroundScrolling ()
                            , Extras.Task.messageToCommand <| PageMsg.Internal ImmediatelyHideBackToTopLink
                            ]
                    , Components.SearchDialog.onHide <| Extras.Task.messageToCommand <| PageMsg.Internal SearchDialogWasHidden
                    ]
            }
      , deleting = NotCurrentlySaving
      , savingSettings = NotCurrentlySaving
      , mostRecentRawTermForOrderingItemsFocusedOn =
            case QueryParameters.orderItemsBy commonModel.queryParameters of
                FocusedOn rawTerm ->
                    Just rawTerm

                _ ->
                    Nothing
      , resultOfAttemptingToCopyEditorCommandToClipboard = Nothing
      }
    , case itemWithFocus of
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


port scrollingUpWhileFarAwayFromTheTop : (() -> msg) -> Sub msg



-- UPDATE


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        MakeChanges ->
            let
                common0 : CommonModel
                common0 =
                    model.common

                editability1 : Editability
                editability1 =
                    Editability.startEditing model.common.editability
            in
            ( { model | common = { common0 | editability = editability1 } }, Cmd.none )

        ShowMenuForMobile ->
            ( { model | menuForMobileVisibility = GradualVisibility.Visible }
            , Cmd.batch
                [ preventBackgroundScrolling ()
                , Extras.BrowserDom.scrollToTopInElement (PageMsg.Internal NoOp) ElementIds.indexForMobile
                ]
            )

        StartHidingMenuForMobile ->
            ( { model | menuForMobileVisibility = GradualVisibility.Disappearing }
            , Cmd.batch
                [ Process.sleep 100 |> Task.perform (always <| PageMsg.Internal CompleteHidingMenuForMobile)
                , allowBackgroundScrolling ()
                ]
            )

        CompleteHidingMenuForMobile ->
            ( { model | menuForMobileVisibility = GradualVisibility.Invisible }, Cmd.none )

        BackToTop staticSidebar counter ->
            let
                idOfSidebarOrMenu : String
                idOfSidebarOrMenu =
                    if staticSidebar then
                        ElementIds.staticSidebarForDesktop

                    else
                        ElementIds.indexForMobile
            in
            ( { model
                | menuForMobileVisibility = GradualVisibility.Disappearing
                , backToTopLinkVisibility = Disappearing counter
              }
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

        SearchDialogWasHidden ->
            ( let
                searchDialog0 : SearchDialog
                searchDialog0 =
                    model.searchDialog
              in
              { model | searchDialog = { searchDialog0 | term = "", results = [] } }
            , allowBackgroundScrolling ()
            )

        UpdateIndexFilterString string ->
            ( { model | indexFilterString = string }, Cmd.none )

        UpdateSearchString searchString ->
            let
                searchDialog0 : SearchDialog
                searchDialog0 =
                    model.searchDialog

                results : List Components.SearchDialog.SearchResult
                results =
                    case model.common.glossaryForUi of
                        Ok glossaryForUi ->
                            glossaryForUi
                                |> GlossaryForUi.items
                                |> Search.search model.common.enableMathSupport (filterByTagId model) searchString

                        Err _ ->
                            []

                model1 : Model
                model1 =
                    { model
                        | searchDialog =
                            { searchDialog0
                                | term = searchString
                                , results = results
                            }
                    }
            in
            update (SearchDialogMsg Components.SearchDialog.searchStringWasJustUpdated) model1

        ChangeTheme theme ->
            let
                common0 : CommonModel
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
                        Just "system"
            )

        ScrollingUpWhileFarAwayFromTheTop ->
            let
                counter_ : Int
                counter_ =
                    backToTopLinkVisibilityCounter model.backToTopLinkVisibility + 1

                backToTopLinkVisibility : BackToTopLinkVisibility
                backToTopLinkVisibility =
                    Visible counter_
            in
            ( { model | backToTopLinkVisibility = backToTopLinkVisibility }
            , Process.sleep 3000 |> Task.perform (always <| PageMsg.Internal <| StartHidingBackToTopLink counter_)
            )

        StartHidingBackToTopLink counter ->
            if model.backToTopLinkVisibility == Visible counter then
                ( { model | backToTopLinkVisibility = Disappearing counter }
                , Process.sleep 300 |> Task.perform (always <| PageMsg.Internal <| CompleteHidingBackToTopLink counter)
                )

            else
                ( model, Cmd.none )

        CompleteHidingBackToTopLink counter ->
            if model.backToTopLinkVisibility == Disappearing counter then
                ( { model | backToTopLinkVisibility = Invisible counter }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        ImmediatelyHideBackToTopLink ->
            ( { model
                | backToTopLinkVisibility =
                    case model.backToTopLinkVisibility of
                        Invisible counter ->
                            Invisible counter

                        Visible counter ->
                            Invisible counter

                        Disappearing counter ->
                            Invisible counter
              }
            , Cmd.none
            )

        JumpToItem itemId ->
            ( { model
                | menuForMobileVisibility = GradualVisibility.Disappearing
                , itemWithFocus = Just itemId
              }
            , Cmd.batch
                [ Process.sleep 100 |> Task.perform (always <| PageMsg.Internal CompleteHidingMenuForMobile)
                , allowBackgroundScrolling ()
                , scrollGlossaryItemIntoView itemId
                ]
            )

        ChangeLayoutToShowSingle index ->
            ( { model
                | layout = ShowSingleItem
                , itemWithFocus = Just index
                , backToTopLinkVisibility =
                    case model.backToTopLinkVisibility of
                        Invisible counter ->
                            Invisible counter

                        Visible counter ->
                            Invisible counter

                        Disappearing counter ->
                            Invisible counter
              }
            , preventBackgroundScrolling ()
            )

        ShowRelatedTermAsSingle relatedTerm ->
            let
                model1 : Model
                model1 =
                    case model.common.glossaryForUi of
                        Ok glossaryForUi ->
                            glossaryForUi
                                |> GlossaryForUi.items
                                |> GlossaryItemsForUi.itemIdFromRawDisambiguatedPreferredTerm (Term.raw relatedTerm)
                                |> Maybe.map (\index -> { model | itemWithFocus = Just index })
                                |> Maybe.withDefault model

                        _ ->
                            model
            in
            ( model1, Cmd.none )

        ChangeLayoutToShowAll ->
            ( { model | layout = ShowAllItems }
            , Cmd.batch
                [ allowBackgroundScrolling ()
                , model.itemWithFocus
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
                    , deleting = NotCurrentlySaving
                  }
                , allowBackgroundScrolling ()
                )

            else
                ( { model | deleting = NotCurrentlySaving }, Cmd.none )

        Delete id ->
            case model.common.glossaryForUi of
                Ok glossaryForUi ->
                    let
                        changelist : GlossaryChangelist
                        changelist =
                            GlossaryChangelist.create
                                (GlossaryForUi.versionNumber glossaryForUi)
                                [ GlossaryChange.Remove id ]

                        ( saving, cmd ) =
                            Save.changeAndSave model.common.editability
                                glossaryForUi
                                changelist
                                (PageMsg.Internal << FailedToDelete)
                                (\( _, updatedGlossary ) ->
                                    PageMsg.Internal <| Deleted updatedGlossary
                                )
                    in
                    ( { model | deleting = saving }
                    , cmd
                    )

                _ ->
                    ( model, Cmd.none )

        Deleted updatedGlossaryForUi ->
            let
                common : CommonModel
                common =
                    model.common

                cmd : Cmd Msg
                cmd =
                    Cmd.batch
                        [ allowBackgroundScrolling ()
                        , giveFocusToOuter
                        ]
            in
            ( { model
                | common = { common | glossaryForUi = Ok <| updatedGlossaryForUi }
                , itemWithFocus = Nothing
                , confirmDeleteId = Nothing
                , deleting = NotCurrentlySaving
                , savingSettings = NotCurrentlySaving
              }
            , cmd
            )

        FailedToDelete error ->
            ( { model
                | deleting = SavingFailed <| Extras.Http.httpErrorDescriptionAskingToReloadOnUnauthorisedOrConflict error
                , savingSettings = NotCurrentlySaving
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
                                        ElementIds.letterGrid
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

                mostRecentTermIdForOrderingItemsFocusedOn1 : Maybe RawTerm
                mostRecentTermIdForOrderingItemsFocusedOn1 =
                    case orderItemsBy of
                        FocusedOn rawTerm ->
                            Just rawTerm

                        _ ->
                            model.mostRecentRawTermForOrderingItemsFocusedOn

                updatedQueryParameters : QueryParameters
                updatedQueryParameters =
                    QueryParameters.setOrderItemsBy orderItemsBy model.common.queryParameters

                common1 : CommonModel
                common1 =
                    { common | queryParameters = updatedQueryParameters }
            in
            ( { model
                | common = common1
                , mostRecentRawTermForOrderingItemsFocusedOn = mostRecentTermIdForOrderingItemsFocusedOn1
              }
            , common1
                |> CommonModel.relativeUrl
                |> Navigation.pushUrl model.common.key
            )

        ChangeCardWidth cardWidth ->
            case model.common.glossaryForUi of
                Ok glossaryForUi ->
                    let
                        changelist : GlossaryChangelist
                        changelist =
                            GlossaryChangelist.create
                                (GlossaryForUi.versionNumber glossaryForUi)
                                [ GlossaryChange.SetCardWidth cardWidth ]

                        ( saving, cmd ) =
                            Save.changeAndSave model.common.editability
                                glossaryForUi
                                changelist
                                (PageMsg.Internal << FailedToChangeSettings)
                                (\( _, updatedGlossary ) ->
                                    PageMsg.Internal <| ChangedSettings updatedGlossary
                                )
                    in
                    ( { model
                        | confirmDeleteId = Nothing
                        , deleting = NotCurrentlySaving
                        , savingSettings = saving
                      }
                    , cmd
                    )

                _ ->
                    ( model, Cmd.none )

        ChangeDefaultTheme defaultTheme ->
            case model.common.glossaryForUi of
                Ok glossaryForUi ->
                    let
                        changelist : GlossaryChangelist
                        changelist =
                            GlossaryChangelist.create
                                (GlossaryForUi.versionNumber glossaryForUi)
                                [ GlossaryChange.SetDefaultTheme defaultTheme ]

                        ( saving, cmd ) =
                            Save.changeAndSave model.common.editability
                                glossaryForUi
                                changelist
                                (PageMsg.Internal << FailedToChangeSettings)
                                (\( _, updatedGlossary ) ->
                                    PageMsg.Internal <| ChangedSettings updatedGlossary
                                )
                    in
                    ( { model
                        | confirmDeleteId = Nothing
                        , deleting = NotCurrentlySaving
                        , savingSettings = saving
                      }
                    , cmd
                    )

                _ ->
                    ( model, Cmd.none )

        ToggleEnableExportMenu ->
            case model.common.glossaryForUi of
                Ok glossaryForUi ->
                    let
                        changelist : GlossaryChangelist
                        changelist =
                            GlossaryChangelist.create
                                (GlossaryForUi.versionNumber glossaryForUi)
                                [ GlossaryChange.ToggleEnableExportMenu ]

                        ( saving, cmd ) =
                            Save.changeAndSave model.common.editability
                                glossaryForUi
                                changelist
                                (PageMsg.Internal << FailedToChangeSettings)
                                (\( itemWithFocus, updatedGlossaryForUi ) ->
                                    let
                                        common0 : CommonModel
                                        common0 =
                                            model.common
                                    in
                                    PageMsg.NavigateToListAll
                                        { common0 | glossaryForUi = Ok updatedGlossaryForUi }
                                        itemWithFocus
                                )
                    in
                    ( { model
                        | confirmDeleteId = Nothing
                        , deleting = NotCurrentlySaving
                        , savingSettings = saving
                      }
                    , cmd
                    )

                _ ->
                    ( model, Cmd.none )

        ToggleEnableOrderItemsButtons ->
            case model.common.glossaryForUi of
                Ok glossaryForUi ->
                    let
                        changelist : GlossaryChangelist
                        changelist =
                            GlossaryChangelist.create
                                (GlossaryForUi.versionNumber glossaryForUi)
                                [ GlossaryChange.ToggleEnableOrderItemsButtons ]

                        ( saving, cmd ) =
                            Save.changeAndSave model.common.editability
                                glossaryForUi
                                changelist
                                (PageMsg.Internal << FailedToChangeSettings)
                                (\( itemWithFocus, updatedGlossaryForUi ) ->
                                    let
                                        common0 : CommonModel
                                        common0 =
                                            model.common
                                    in
                                    PageMsg.NavigateToListAll
                                        { common0 | glossaryForUi = Ok updatedGlossaryForUi }
                                        itemWithFocus
                                )
                    in
                    ( { model
                        | confirmDeleteId = Nothing
                        , deleting = NotCurrentlySaving
                        , savingSettings = saving
                      }
                    , cmd
                    )

                _ ->
                    ( model, Cmd.none )

        ToggleEnableLastUpdatedDates ->
            case model.common.glossaryForUi of
                Ok glossaryForUi ->
                    let
                        changelist : GlossaryChangelist
                        changelist =
                            GlossaryChangelist.create
                                (GlossaryForUi.versionNumber glossaryForUi)
                                [ GlossaryChange.ToggleEnableLastUpdatedDates ]

                        ( saving, cmd ) =
                            Save.changeAndSave model.common.editability
                                glossaryForUi
                                changelist
                                (PageMsg.Internal << FailedToChangeSettings)
                                (\( itemWithFocus, updatedGlossaryForUi ) ->
                                    let
                                        common0 : CommonModel
                                        common0 =
                                            model.common
                                    in
                                    PageMsg.NavigateToListAll
                                        { common0 | glossaryForUi = Ok updatedGlossaryForUi }
                                        itemWithFocus
                                )
                    in
                    ( { model
                        | confirmDeleteId = Nothing
                        , deleting = NotCurrentlySaving
                        , savingSettings = saving
                      }
                    , cmd
                    )

                _ ->
                    ( model, Cmd.none )

        ChangedSettings updatedGlossaryForUi ->
            let
                common : CommonModel
                common =
                    model.common
            in
            ( { model
                | common = { common | glossaryForUi = Ok <| updatedGlossaryForUi }
                , savingSettings = NotCurrentlySaving
              }
            , if common.editability == Editability.EditingInMemory then
                Cmd.none

              else
                Navigation.reload
            )

        FailedToChangeSettings error ->
            ( { model
                | savingSettings = SavingFailed <| Extras.Http.httpErrorDescriptionAskingToReloadOnUnauthorisedOrConflict error
              }
            , Cmd.none
            )

        DownloadMarkdown ->
            ( { model | exportDropdownMenu = Components.DropdownMenu.hidden model.exportDropdownMenu }
            , model.common.glossaryForUi
                |> Result.map Export.Markdown.download
                |> Result.withDefault Cmd.none
            )

        DownloadAnki ->
            ( { model | exportDropdownMenu = Components.DropdownMenu.hidden model.exportDropdownMenu }
            , model.common.glossaryForUi
                |> Result.map (Export.Anki.download model.common.enableMathSupport)
                |> Result.withDefault Cmd.none
            )

        DownloadJson ->
            ( { model | exportDropdownMenu = Components.DropdownMenu.hidden model.exportDropdownMenu }
            , case model.common.glossaryForUi of
                Ok glossaryForUi ->
                    Export.Json.download glossaryForUi

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

                common1 : CommonModel
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

                common1 : CommonModel
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


filterByTagId : Model -> Maybe TagId
filterByTagId model =
    case model.common.glossaryForUi of
        Ok glossaryForUi ->
            let
                queryParametersTag : Maybe Tag
                queryParametersTag =
                    model.common.queryParameters
                        |> QueryParameters.filterByTag
            in
            queryParametersTag
                |> Maybe.andThen
                    (\tag -> glossaryForUi |> GlossaryForUi.items |> GlossaryItemsForUi.tagIdFromTag tag)

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

        command : String
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
                [ class "mb-1" ]
                [ I18n.webInterfaceDescription
                , I18n.runTheFollowingCommand tabbable
                , div
                    [ class "mt-3 flex rounded-md" ]
                    [ div
                        [ class "block w-full max-w-2xl flex grow items-stretch focus-within:z-10" ]
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


viewSettings : GlossaryForUi -> Editability -> Saving -> { tabbable : Bool, enableMathSupport : Bool } -> Html Msg
viewSettings glossaryForUi editability savingSettings { tabbable, enableMathSupport } =
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
            [ Accessibility.Key.tabbable tabbable
            , class "relative"
            ]
            [ Extras.Html.showIf (savingSettings == SavingInProgress) <|
                div
                    [ class "absolute top-1/2 bottom-1/2 left-1/2 right-1/2" ]
                    [ Components.Spinner.view
                        [ Svg.Attributes.class "w-12 h-12" ]
                        (savingSettings == SavingInProgress)
                    ]
            , summary
                [ class "pb-1 text-lg leading-6 items-center font-medium text-gray-900 dark:text-gray-100 select-none" ]
                [ span
                    [ class "ml-2" ]
                    [ text I18n.settings ]
                ]
            , div
                [ class "space-y-8 py-4"
                , Extras.HtmlAttribute.showIf (savingSettings == SavingInProgress) <|
                    class "opacity-25"
                ]
                [ Extras.Html.showIf (editability == EditingWithIncludedBackend) <|
                    p
                        [ class "mt-3 max-w-xl" ]
                        [ text I18n.theseSettingsAreUpdatedInTheHtmlFile
                        ]
                , Extras.Html.showIf (editability == EditingWithIncludedBackend) <|
                    viewSelectInputSyntax enableMathSupport
                , viewSelectCardWidth glossaryForUi tabbable
                , viewSelectDefaultTheme glossaryForUi tabbable
                , Components.Button.toggle
                    (GlossaryForUi.enableExportMenu glossaryForUi)
                    ElementIds.showExportMenuLabel
                    [ Html.Events.onClick <| PageMsg.Internal ToggleEnableExportMenu ]
                    [ span
                        [ class "font-medium text-gray-900 dark:text-gray-300" ]
                        [ text I18n.showExportMenu ]
                    ]
                , Components.Button.toggle
                    (GlossaryForUi.enableOrderItemsButtons glossaryForUi)
                    ElementIds.showOrderItemsButtons
                    [ Html.Events.onClick <| PageMsg.Internal ToggleEnableOrderItemsButtons ]
                    [ span
                        [ class "font-medium text-gray-900 dark:text-gray-300" ]
                        [ text I18n.showOrderItemsButtons ]
                    ]
                , Components.Button.toggle
                    (GlossaryForUi.enableLastUpdatedDates glossaryForUi)
                    ElementIds.showLastUpdatedDatesLabel
                    [ Html.Events.onClick <| PageMsg.Internal ToggleEnableLastUpdatedDates ]
                    [ span
                        [ class "font-medium text-gray-900 dark:text-gray-300" ]
                        [ text I18n.showLastUpdatedDates ]
                    ]
                , case savingSettings of
                    SavingNotAttempted errorMessage ->
                        errorDiv <| I18n.failedToSave ++ " — " ++ errorMessage ++ "."

                    SavingFailed errorMessage ->
                        errorDiv <| I18n.failedToSave ++ " — " ++ errorMessage ++ "."

                    _ ->
                        Extras.Html.nothing
                ]
            ]
        ]


viewTermIndexItem : Bool -> IndexOfTerms.Entry -> List (Html Msg)
viewTermIndexItem enableMathSupport entry =
    case entry of
        IndexOfTerms.PreferredTerm itemId disambiguatedTerm ->
            let
                term : Term
                term =
                    DisambiguatedTerm.toTerm disambiguatedTerm
            in
            [ li []
                [ Html.a
                    [ class "group block border-l pl-4 -ml-px border-transparent hover:border-slate-400 dark:hover:border-slate-400 font-medium text-slate-700 hover:text-slate-900 dark:text-slate-400 dark:hover:text-slate-300"
                    , Html.Attributes.href <| Extras.Url.fragmentOnly <| Term.id term
                    , Html.Attributes.target "_self"
                    , Html.Events.onClick <| PageMsg.Internal <| JumpToItem itemId
                    ]
                    [ Term.view enableMathSupport [] term ]
                ]
            ]

        IndexOfTerms.AlternativeTerm term disambiguatedPreferredTerms ->
            let
                preferredTerms : List ( GlossaryItemId, Term )
                preferredTerms =
                    List.map (Tuple.mapSecond DisambiguatedTerm.toTerm) disambiguatedPreferredTerms
            in
            li
                [ Html.Attributes.attribute "style" "margin-top: 1rem" ]
                [ Html.span
                    [ class "block border-l pl-4 -ml-px border-transparent select-none" ]
                    [ Term.view enableMathSupport [] term
                    ]
                ]
                :: List.indexedMap
                    (\index ( itemId, preferredTerm ) ->
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
                                , Html.Attributes.href <| Extras.Url.fragmentOnly <| Term.id preferredTerm
                                , Html.Attributes.target "_self"
                                , Html.Events.onClick <| PageMsg.Internal <| JumpToItem itemId
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


viewTermIndexGroup : Bool -> Bool -> TermGroup -> Html Msg
viewTermIndexGroup enableMathSupport staticSidebar { label, entries } =
    li
        [ id <| ElementIds.termIndexGroupLabel staticSidebar label
        , class "mt-6"
        ]
        [ h5
            [ class "mb-8 lg:mb-3 font-semibold" ]
            [ span
                [ class "inline-flex items-center rounded-md bg-gray-50 dark:bg-gray-400/10 px-2 py-1 text-gray-600 dark:text-gray-400 ring-1 ring-inset ring-gray-500/10 dark:ring-gray-400/20" ]
                [ text label ]
            ]
        , ul
            [ class "space-y-6 lg:space-y-2 border-l border-slate-200 dark:border-slate-600" ]
            (List.concatMap (viewTermIndexItem enableMathSupport) entries)
        ]


viewIndexOfTerms : Bool -> Bool -> IndexOfTerms -> Html Msg
viewIndexOfTerms enableMathSupport staticSidebar indexOfTerms =
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
                    Just <| viewTermIndexGroup enableMathSupport staticSidebar termIndexGroup
            )
            termGroups
        )


viewGlossaryItem :
    { enableMathSupport : Bool
    , editable : Bool
    , enableLastUpdatedDates : Bool
    , shownAsSingle : Bool
    }
    -> Maybe GlossaryItemId
    -> Maybe Tag
    -> GlossaryItemWithPreviousAndNext
    -> Html Msg
viewGlossaryItem { enableMathSupport, editable, enableLastUpdatedDates, shownAsSingle } itemWithFocus tagBeingFilteredBy itemWithPreviousAndNext =
    Extras.Html.showMaybe
        (\item ->
            Components.GlossaryItemCard.view
                { enableMathSupport = enableMathSupport, enableLastUpdatedDates = enableLastUpdatedDates }
                (Components.GlossaryItemCard.Normal
                    { onClickViewFull = PageMsg.Internal <| ChangeLayoutToShowSingle <| GlossaryItemForUi.id item
                    , onClickEdit = PageMsg.NavigateToCreateOrEdit <| Just <| GlossaryItemForUi.id item
                    , onClickDelete = PageMsg.Internal <| ConfirmDelete <| GlossaryItemForUi.id item
                    , onClickTag = PageMsg.Internal << FilterByTag
                    , onClickItem = PageMsg.Internal << ChangeLayoutToShowSingle
                    , onClickRelatedTerm = PageMsg.Internal << ShowRelatedTermAsSingle
                    , editable = editable
                    , shownAsSingle = shownAsSingle
                    }
                )
                tagBeingFilteredBy
                itemWithFocus
                itemWithPreviousAndNext
        )
        itemWithPreviousAndNext.item


itemWithPreviousAndNextForId : GlossaryItemId -> List ( GlossaryItemId, GlossaryItemForUi ) -> GlossaryItemWithPreviousAndNext
itemWithPreviousAndNextForId id indexedGlossaryItems =
    let
        indexedGlossaryItemsArray : Array.Array ( GlossaryItemId, GlossaryItemForUi )
        indexedGlossaryItemsArray =
            Array.fromList indexedGlossaryItems

        indexed : Array.Array ( Int, ( GlossaryItemId, GlossaryItemForUi ) )
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
        |> (\result ->
                { previous = Maybe.map Tuple.second result.previous
                , item = Maybe.map Tuple.second result.item
                , next = Maybe.map Tuple.second result.next
                }
           )


viewSingleItemModalDialog :
    Maybe GlossaryItemId
    -> Bool
    -> Bool
    -> Bool
    -> Maybe DescribedTag
    -> OrderItemsBy
    -> GlossaryItemsForUi
    -> Maybe GlossaryItemId
    -> Html Msg
viewSingleItemModalDialog itemWithFocus enableMathSupport editable enableLastUpdatedDates tagBeingFilteredBy orderItemsBy items =
    let
        filterByTagId_ : Maybe TagId
        filterByTagId_ =
            Maybe.map DescribedTag.id tagBeingFilteredBy

        ( indexedGlossaryItems_, otherIndexedGlossaryItems_ ) =
            items
                |> (case orderItemsBy of
                        Alphabetically ->
                            GlossaryItemsForUi.orderedAlphabetically filterByTagId_
                                >> (\lhs -> ( lhs, [] ))

                        MostMentionedFirst ->
                            GlossaryItemsForUi.orderedByMostMentionedFirst filterByTagId_
                                >> (\lhs -> ( lhs, [] ))

                        FocusedOn termId ->
                            let
                                itemId : Maybe GlossaryItemId
                                itemId =
                                    GlossaryItemsForUi.itemIdFromRawDisambiguatedPreferredTerm termId items
                            in
                            case itemId of
                                Just itemId_ ->
                                    GlossaryItemsForUi.orderedFocusedOn filterByTagId_ itemId_

                                Nothing ->
                                    always
                                        (items
                                            |> GlossaryItemsForUi.orderedAlphabetically filterByTagId_
                                            |> (\lhs -> ( lhs, [] ))
                                        )
                   )

        combinedIndexedGlossaryItems : List ( GlossaryItemId, GlossaryItemForUi )
        combinedIndexedGlossaryItems =
            List.append indexedGlossaryItems_ otherIndexedGlossaryItems_
    in
    Maybe.map
        (\id ->
            let
                itemWithPreviousAndNext : GlossaryItemWithPreviousAndNext
                itemWithPreviousAndNext =
                    itemWithPreviousAndNextForId id combinedIndexedGlossaryItems
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
                            [ Html.Events.onClick <| PageMsg.Internal ChangeLayoutToShowAll
                            ]
                            [ Icons.xMark
                                [ Svg.Attributes.class "h-5 w-5 text-gray-400 dark:text-gray-300 hover:text-gray-500 dark:hover:text-gray-400" ]
                            ]
                        ]
                    , Html.dl
                        [ Html.Attributes.style "display" "block" ]
                        [ viewGlossaryItem
                            { enableMathSupport = enableMathSupport
                            , editable = editable
                            , enableLastUpdatedDates = enableLastUpdatedDates
                            , shownAsSingle = True
                            }
                            itemWithFocus
                            (Maybe.map DescribedTag.tag tagBeingFilteredBy)
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
viewConfirmDeleteModal editability itemToDelete deleting =
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
                SavingNotAttempted errorMessage ->
                    div
                        [ class "flex justify-end mt-2" ]
                        [ p
                            [ class "text-red-600 dark:text-red-400" ]
                            [ text <| I18n.failedToSave ++ " — " ++ errorMessage ++ "." ]
                        ]

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
                        itemToDelete
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
        (itemToDelete /= Nothing)


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
                    [ class "ml-2 inline-flex items-center rounded-xs border border-gray-700 dark:border-gray-300 px-1 font-sans text-xs" ]
                    [ text "e" ]
                ]
            ]
        , Extras.Html.showIf (editability == Editability.EditingInMemory) <|
            div
                [ class "my-4 text-sm text-gray-500 dark:text-gray-400" ]
                [ text I18n.savingChangesInMemoryMessage ]
        ]


viewEditTitleAndAboutButton : Bool -> Html Msg
viewEditTitleAndAboutButton tabbable =
    div
        [ class "pb-3 print:hidden" ]
        [ Components.Button.text
            [ Html.Events.onClick <| PageMsg.NavigateToEditTitleAndAbout
            , Accessibility.Key.tabbable tabbable
            ]
            [ Icons.pencil
                [ Svg.Attributes.class "h-5 w-5 text-gray-400 dark:text-gray-300 hover:text-gray-500 dark:hover:text-gray-400" ]
            , span
                [ class "ml-2" ]
                [ text I18n.editTitleAndAboutSectionButton ]
            ]
        ]


viewCreateGlossaryItemButtonForEmptyState : Html Msg
viewCreateGlossaryItemButtonForEmptyState =
    div
        [ class "pt-4 print:hidden" ]
        [ Components.Button.emptyState
            [ class "p-9"
            , Html.Events.onClick <| PageMsg.NavigateToCreateOrEdit Nothing
            ]
            [ Icons.viewGridAdd
                [ Svg.Attributes.class "mx-auto h-12 w-12 text-gray-400" ]
            , span
                [ class "mt-2 inline-flex items-center font-medium text-gray-900 dark:text-gray-200" ]
                [ text I18n.createANewGlossaryItem
                , Html.kbd
                    [ class "ml-2 rounded-xs border border-indigo-700 dark:border-indigo-300 px-1 font-sans text-xs" ]
                    [ text "n" ]
                ]
            ]
        ]


viewCreateGlossaryItemButton : Html Msg
viewCreateGlossaryItemButton =
    div
        [ class "pb-2 print:hidden" ]
        [ Components.Button.secondary
            [ Html.Events.onClick <| PageMsg.NavigateToCreateOrEdit Nothing
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
                    [ class "ml-2 inline-flex items-center rounded-xs border border-indigo-700 dark:border-indigo-300 px-1 font-sans text-xs" ]
                    [ text "n" ]
                ]
            ]
        ]


viewCards :
    { enableMathSupport : Bool
    , enableOrderItemsButtons : Bool
    , editable : Bool
    , enableLastUpdatedDates : Bool
    , editing : Bool
    }
    ->
        { filterByTagId_ : Maybe TagId
        , tags : List Tag
        , filterByDescribedTag_ : Maybe DescribedTag
        }
    -> QueryParameters
    -> Maybe GlossaryItemId
    -> Maybe RawTerm
    -> GlossaryItemsForUi
    -> ( List ( GlossaryItemId, GlossaryItemForUi ), List ( GlossaryItemId, GlossaryItemForUi ) )
    -> Html Msg
viewCards { enableMathSupport, enableOrderItemsButtons, editable, enableLastUpdatedDates, editing } { filterByTagId_, tags, filterByDescribedTag_ } queryParameters itemWithFocus mostRecentRawTermForOrderingItemsFocusedOn glossaryItemsForUi ( indexedGlossaryItems, otherIndexedGlossaryItems ) =
    let
        filterByTag : Maybe Tag
        filterByTag =
            Maybe.map DescribedTag.tag filterByDescribedTag_

        combinedIndexedGlossaryItems : List ( GlossaryItemId, GlossaryItemForUi )
        combinedIndexedGlossaryItems =
            List.append indexedGlossaryItems otherIndexedGlossaryItems

        disambiguatedPreferredTermsWithDefinitions : List DisambiguatedTerm
        disambiguatedPreferredTermsWithDefinitions =
            GlossaryItemsForUi.disambiguatedPreferredTermsWhichHaveDefinitions
                filterByTagId_
                glossaryItemsForUi

        orderItemsFocusedOnTerm : Maybe DisambiguatedTerm
        orderItemsFocusedOnTerm =
            case QueryParameters.orderItemsBy queryParameters of
                FocusedOn termId ->
                    GlossaryItemsForUi.disambiguatedPreferredTermFromRaw termId glossaryItemsForUi

                _ ->
                    Nothing

        viewIndexedItem : GlossaryItemForUi -> Html Msg
        viewIndexedItem item =
            viewGlossaryItem
                { enableMathSupport = enableMathSupport
                , editable = editable
                , enableLastUpdatedDates = enableLastUpdatedDates
                , shownAsSingle = False
                }
                itemWithFocus
                filterByTag
                { previous = Nothing, item = Just item, next = Nothing }

        viewIndexedItemKeyed : ( GlossaryItemId, GlossaryItemForUi ) -> ( String, Html Msg )
        viewIndexedItemKeyed ( itemId, item ) =
            ( GlossaryItemId.toString itemId
            , Html.Lazy.lazy viewIndexedItem item
            )

        totalNumberOfItems : Int
        totalNumberOfItems =
            List.length combinedIndexedGlossaryItems

        recommendedMaximumNumberOfItems : Int
        recommendedMaximumNumberOfItems =
            1000
    in
    div
        []
        [ div
            [ class "mb-4" ]
            [ Extras.Html.showMaybe
                (viewCurrentTagFilter { enableMathSupport = enableMathSupport })
                filterByDescribedTag_
            , Extras.Html.showIf (filterByDescribedTag_ == Nothing) <|
                viewAllTagFilters { enableMathSupport = enableMathSupport } tags
            , Extras.Html.showIf editing <|
                div
                    [ class "flex-none mt-4" ]
                    [ viewManageTagsButton ]
            ]
        , div
            []
            [ Extras.Html.showIf editable <|
                div
                    [ class "pt-2" ]
                    [ if List.isEmpty combinedIndexedGlossaryItems then
                        viewCreateGlossaryItemButtonForEmptyState

                      else
                        viewCreateGlossaryItemButton
                    ]
            ]
        , Extras.Html.showIf (editable && totalNumberOfItems > recommendedMaximumNumberOfItems) <|
            I18n.glossaryContainsTooManyItems recommendedMaximumNumberOfItems
        , Extras.Html.showIf
            (List.isEmpty combinedIndexedGlossaryItems && filterByTagId_ /= Nothing)
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
                totalNumberOfItems
                { enableMathSupport = enableMathSupport }
                disambiguatedPreferredTermsWithDefinitions
                orderItemsFocusedOnTerm
                queryParameters
                mostRecentRawTermForOrderingItemsFocusedOn
        , Html.Keyed.node "dl"
            [ class "mt-4" ]
            (List.map viewIndexedItemKeyed indexedGlossaryItems)
        , Extras.Html.showIf
            ((not <| List.isEmpty indexedGlossaryItems)
                && (not <| List.isEmpty otherIndexedGlossaryItems)
            )
          <|
            Components.Dividers.withLabel
                [ class "my-10" ]
                I18n.otherItems
        , Extras.Html.showIf (not <| List.isEmpty otherIndexedGlossaryItems) <|
            Html.Keyed.node "dl"
                []
                (List.map viewIndexedItemKeyed otherIndexedGlossaryItems)
        ]


viewMenuForMobileAndStaticSidebarForDesktop :
    MenuForMobileVisibility
    -> Bool
    -> String
    -> Maybe TagId
    -> GlossaryItemsForUi
    -> Html Msg
viewMenuForMobileAndStaticSidebarForDesktop menuForMobileVisibility enableMathSupport indexFilterString filterByTag items =
    let
        indexOfTerms : IndexOfTerms
        indexOfTerms =
            IndexOfTerms.fromGlossaryItems filterByTag items
    in
    div []
        [ Html.Lazy.lazy3 viewMenuForMobile
            menuForMobileVisibility
            enableMathSupport
            indexOfTerms
        , Html.Lazy.lazy3 viewStaticSidebarForDesktop enableMathSupport indexFilterString indexOfTerms
        ]


viewMenuForMobile : MenuForMobileVisibility -> Bool -> IndexOfTerms -> Html Msg
viewMenuForMobile menuForMobileVisibility enableMathSupport termIndex =
    div
        [ class "invisible" |> Extras.HtmlAttribute.showIf (menuForMobileVisibility == GradualVisibility.Invisible)
        , class "fixed inset-0 flex z-40 lg:hidden"
        , Accessibility.Role.dialog
        , Accessibility.Aria.modal True
        ]
        [ Html.div
            [ class "fixed inset-0 bg-gray-600 bg-black/75"
            , if menuForMobileVisibility == GradualVisibility.Visible then
                class "transition-opacity motion-reduce:transition-none ease-linear duration-300 opacity-100"

              else
                class "transition-opacity motion-reduce:transition-none ease-linear duration-300 opacity-0"
            , Html.Events.onClick <| PageMsg.Internal StartHidingMenuForMobile
            , Accessibility.Aria.hidden True
            ]
            []
        , div
            [ class "relative flex-1 flex flex-col max-w-xs w-full bg-white dark:bg-gray-900"
            , if menuForMobileVisibility == GradualVisibility.Visible then
                class "transition motion-reduce:transition-none ease-in-out duration-300 transform motion-reduce:transform-none translate-x-0"

              else
                class "transition motion-reduce:transition-none ease-in-out duration-300 transform motion-reduce:transform-none -translate-x-full"
            ]
            [ div
                [ class "absolute top-0 right-0 -mr-12 pt-2"
                , if menuForMobileVisibility == GradualVisibility.Visible then
                    class "motion-reduce:transition-none ease-in-out duration-300 opacity-100"

                  else
                    class "motion-reduce:transition-none ease-in-out duration-300 opacity-0"
                ]
                [ button
                    [ Html.Attributes.type_ "button"
                    , class "ml-1 flex items-center justify-center h-10 w-10 rounded-full focus:outline-hidden focus:ring-2 focus:ring-inset focus:ring-white dark:focus:ring-gray-500"
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
                , class "flex-1 h-0 overflow-y-scroll"
                ]
                [ nav
                    [ class "px-4 pt-5 pb-6" ]
                    [ Html.Lazy.lazy2 viewTermIndexFirstCharacterGrid False termIndex
                    , Html.Lazy.lazy3 viewIndexOfTerms enableMathSupport False termIndex
                    ]
                ]
            ]
        , div
            [ class "shrink-0 w-14", Accessibility.Aria.hidden True ]
            []
        ]


backToTopLinkVisibilityCounter : BackToTopLinkVisibility -> Int
backToTopLinkVisibilityCounter backToTopLinkVisibility =
    case backToTopLinkVisibility of
        Visible counter ->
            counter

        Disappearing counter ->
            counter

        Invisible counter ->
            counter


viewBackToTopLink : Bool -> BackToTopLinkVisibility -> Html Msg
viewBackToTopLink staticSidebar visibility =
    div
        [ class "z-50 fixed bottom-0 right-0 p-4 print:hidden"
        , class "hidden"
            |> Extras.HtmlAttribute.showIf
                (case visibility of
                    Invisible _ ->
                        True

                    _ ->
                        False
                )
        , case visibility of
            Visible _ ->
                class "transition motion-reduce:transition-none ease-out duration-300 transform motion-reduce:transform-none opacity-100 scale-100"

            _ ->
                class "transition motion-reduce:transition-none ease-in duration-300 transform motion-reduce:transform-none opacity-0 scale-95"
        ]
        [ Html.a
            [ href <| Extras.Url.fragmentOnly ElementIds.container
            , Extras.HtmlEvents.onClickStopPropagation <|
                PageMsg.Internal <|
                    BackToTop staticSidebar (backToTopLinkVisibilityCounter visibility)
            , Accessibility.Key.tabbable staticSidebar
            ]
            [ span
                [ class "inline-flex bg-white/95 dark:bg-gray-800/95 border border-gray-600 dark:border-gray-400 rounded-md p-3 text-lg" ]
                [ text I18n.backToTop
                , Icons.arrowUp
                    [ Svg.Attributes.class "w-6 h-6 ml-2"
                    , Accessibility.Aria.hidden True
                    ]
                ]
            ]
        ]


viewQuickSearchButton : Bool -> Html Msg
viewQuickSearchButton runningOnMacOs =
    div
        []
        [ div
            [ class "bg-gray-50 dark:bg-slate-900 relative pointer-events-auto" ]
            [ button
                [ Html.Attributes.type_ "button"
                , class "w-full flex items-center text-sm leading-6 text-slate-500 dark:text-slate-400 rounded-md ring-1 ring-slate-900/10 dark:ring-slate-600 shadow-xs py-1.5 pl-2 pr-3 hover:ring-slate-400 dark:hover:ring-slate-400 dark:bg-slate-800 dark:highlight-white/5 dark:hover:bg-slate-800 select-none"
                , Html.Events.onClick <| PageMsg.Internal <| SearchDialogMsg Components.SearchDialog.show
                , Accessibility.Aria.hidden True
                ]
                [ Icons.search
                    [ width "24"
                    , height "24"
                    , Svg.Attributes.class "mr-3 flex-none"
                    ]
                , text I18n.searchPlaceholder
                , span
                    [ class "ml-auto pl-3 flex-none text-xs font-semibold" ]
                    [ text <|
                        if runningOnMacOs then
                            I18n.commandK

                        else
                            I18n.controlK
                    ]
                ]
            ]
        ]


viewTermIndexFirstCharacter : Bool -> String -> Bool -> Html Msg
viewTermIndexFirstCharacter staticSidebar firstCharacter enabled =
    Components.Button.white enabled
        [ class "m-0.5 px-3 py-2 leading-4"
        , Html.Events.onClick <|
            PageMsg.Internal <|
                if enabled then
                    JumpToTermIndexGroup staticSidebar firstCharacter

                else
                    NoOp
        ]
        [ text firstCharacter ]


viewTermIndexFirstCharacterGrid : Bool -> IndexOfTerms -> Html Msg
viewTermIndexFirstCharacterGrid staticSidebar indexOfTerms =
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
                    termIndexGroup.label
                    (not <| List.isEmpty termIndexGroup.entries)
            )
            termGroups
        )


viewIndexFilterInputField : String -> Html Msg
viewIndexFilterInputField indexFilterString =
    div
        [ class "pb-4" ]
        [ div []
            [ div
                [ class "mt-2 grid grid-cols-1"
                ]
                [ Html.input
                    [ Html.Attributes.type_ "search"
                    , id ElementIds.indexFilterInputField
                    , class "col-start-1 row-start-1 block w-full pl-9 rounded-md focus:ring-indigo-500 focus:border-indigo-500 border-gray-300 dark:border-gray-500 dark:bg-gray-700 dark:text-white placeholder-gray-500 dark:placeholder-gray-400"
                    , Html.Attributes.placeholder "Filter"
                    , Html.Attributes.autocomplete False
                    , Html.Attributes.attribute "autocorrect" "off"
                    , Html.Attributes.attribute "autocapitalize" "off"
                    , Html.Attributes.spellcheck False
                    , Html.Attributes.value indexFilterString
                    , Html.Events.onInput (PageMsg.Internal << UpdateIndexFilterString)
                    ]
                    []
                , Icons.filter
                    [ Svg.Attributes.class "pointer-events-none col-start-1 row-start-1 ml-3 size-5 self-center text-gray-400 dark:text-gray-500"
                    , Html.Attributes.attribute "aria-hidden" "true"
                    , Html.Attributes.attribute "data-slot" "icon"
                    ]
                ]
            ]
        ]


viewLetterGrid : Bool -> String -> IndexOfTerms -> Html Msg
viewLetterGrid staticSidebar indexFilterString indexOfTerms =
    div
        [ id ElementIds.letterGrid
        , class "z-10 -mb-6 sticky top-0 -ml-0.5"
        ]
        [ div
            [ class "pt-5 px-3 bg-white dark:bg-slate-900" ]
            [ viewIndexFilterInputField indexFilterString
            , viewTermIndexFirstCharacterGrid staticSidebar indexOfTerms
            ]
        , div
            [ class "h-8 bg-linear-to-b from-white dark:from-slate-900" ]
            []
        ]


viewStaticSidebarForDesktop : Bool -> String -> IndexOfTerms -> Html Msg
viewStaticSidebarForDesktop enableMathSupport indexFilterString termIndex =
    let
        filteredTermIndex : IndexOfTerms
        filteredTermIndex =
            IndexOfTerms.filterByString indexFilterString termIndex
    in
    div
        [ class "hidden print:hidden lg:flex lg:flex-col lg:w-64 lg:fixed lg:inset-y-0 lg:border-r lg:border-gray-200 lg:bg-white lg:dark:border-gray-800 lg:dark:bg-gray-900"
        ]
        [ div
            [ id ElementIds.staticSidebarForDesktop
            , class "h-0 flex-1 flex flex-col overflow-y-scroll"
            ]
            [ viewLetterGrid True indexFilterString filteredTermIndex
            , nav
                [ class "px-3" ]
                [ viewIndexOfTerms enableMathSupport True filteredTermIndex ]
            ]
        ]


viewTopBar : Bool -> Bool -> Theme -> Components.DropdownMenu.Model -> Maybe Components.DropdownMenu.Model -> Html Msg
viewTopBar tabbable runningOnMacOs theme themeDropdownMenu maybeExportDropdownMenu =
    div
        [ class "sticky top-0 z-20 shrink-0 flex justify-between h-16 bg-white dark:bg-gray-900 border-b border-gray-200 dark:border-gray-800 lg:hidden print:hidden items-center" ]
        [ div
            [ class "flex-1" ]
            [ button
                [ Html.Attributes.type_ "button"
                , class "px-4 border-r border-gray-200 dark:border-gray-700 text-gray-500 focus:outline-hidden lg:hidden"
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
            [ class "hidden sm:block pr-4" ]
            [ viewQuickSearchButton runningOnMacOs ]
        , div
            [ class "pr-4 sm:hidden" ]
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


viewSelectCardWidth : GlossaryForUi -> Bool -> Html Msg
viewSelectCardWidth glossaryForUi tabbable =
    let
        cardWidth : CardWidth
        cardWidth =
            GlossaryForUi.cardWidth glossaryForUi
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


viewSelectDefaultTheme : GlossaryForUi -> Bool -> Html Msg
viewSelectDefaultTheme glossaryForUi tabbable =
    let
        defaultTheme : Theme
        defaultTheme =
            GlossaryForUi.defaultTheme glossaryForUi
    in
    div
        []
        [ fieldset []
            [ legend
                [ class "mb-4 font-medium text-gray-900 dark:text-gray-100" ]
                [ text I18n.defaultTheme ]
            , div
                [ class "space-y-4 sm:flex sm:items-center sm:space-y-0 sm:space-x-6" ]
                [ div
                    [ class "flex items-center" ]
                    [ Components.Button.radio
                        "default-theme"
                        "default-theme-light"
                        (defaultTheme == Light)
                        tabbable
                        [ id ElementIds.defaultThemeLight
                        , Html.Events.onClick <| PageMsg.Internal <| ChangeDefaultTheme Light
                        ]
                    , label
                        [ class "ml-3 block font-medium text-gray-700 dark:text-gray-300 select-none"
                        , for ElementIds.defaultThemeLight
                        ]
                        [ text I18n.themeLight ]
                    ]
                , div
                    [ class "flex items-center" ]
                    [ Components.Button.radio
                        "default-theme"
                        "default-theme-dark"
                        (defaultTheme == Dark)
                        tabbable
                        [ id ElementIds.defaultThemeDark
                        , Html.Events.onClick <| PageMsg.Internal <| ChangeDefaultTheme Dark
                        ]
                    , label
                        [ class "ml-3 block font-medium text-gray-700 dark:text-gray-300 select-none"
                        , for ElementIds.defaultThemeDark
                        ]
                        [ text I18n.themeDark ]
                    ]
                , div
                    [ class "flex items-center" ]
                    [ Components.Button.radio
                        "default-theme"
                        "default-theme-system"
                        (defaultTheme == System)
                        tabbable
                        [ id ElementIds.defaultThemeSystem
                        , Html.Events.onClick <| PageMsg.Internal <| ChangeDefaultTheme System
                        ]
                    , label
                        [ class "ml-3 block font-medium text-gray-700 dark:text-gray-300 select-none"
                        , for ElementIds.defaultThemeSystem
                        ]
                        [ text I18n.themeSystem ]
                    ]
                ]
            ]
        ]


viewCurrentTagFilter : { enableMathSupport : Bool } -> DescribedTag -> Html Msg
viewCurrentTagFilter { enableMathSupport } describedTag =
    div
        [ class "pt-3" ]
        [ span
            [ class "print:hidden mr-2 font-medium text-gray-900 dark:text-gray-100" ]
            [ text I18n.filteringByTag ]
        , Components.Badge.indigoWithBorderAndRemoveButton
            True
            [ class "print:hidden mt-2" ]
            (PageMsg.Internal DoNotFilterByTag)
            [ Tag.view enableMathSupport [] <| DescribedTag.tag describedTag ]
        ]


viewAllTagFilters : { enableMathSupport : Bool } -> List Tag -> Html Msg
viewAllTagFilters { enableMathSupport } tags =
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
                                    True
                                    [ class "mr-2 mt-2"
                                    , Html.Events.onClick <| PageMsg.Internal <| FilterByTag tag
                                    ]
                                    [ Tag.view enableMathSupport [] tag ]
                            )
                   )
            )


viewManageTagsButton : Html Msg
viewManageTagsButton =
    div
        [ class "pb-3 print:hidden" ]
        [ Components.Button.text
            [ Html.Events.onClick <| PageMsg.NavigateToManageTags
            ]
            [ Icons.pencil
                [ Svg.Attributes.class "h-5 w-5 text-gray-400 dark:text-gray-300 hover:text-gray-500 dark:hover:text-gray-400" ]
            , span
                [ class "ml-2" ]
                [ text I18n.manageTags ]
            ]
        ]


viewOrderItemsBy :
    Int
    -> { enableMathSupport : Bool }
    -> List DisambiguatedTerm
    -> Maybe DisambiguatedTerm
    -> QueryParameters
    -> Maybe RawTerm
    -> Html Msg
viewOrderItemsBy numberOfItems { enableMathSupport } disambiguatedPreferredTermsWithDefinitions orderItemsFocusedOnTerm queryParameters mostRecentRawTermForOrderingItemsFocusedOn =
    div
        [ class "print:hidden pt-4 pb-2" ]
        [ fieldset []
            [ legend
                [ class "mb-4 font-medium text-gray-900 dark:text-gray-100" ]
                [ text I18n.orderItems
                , span
                    [ class "ml-2 text-gray-600 dark:text-gray-400" ]
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
                        (QueryParameters.orderItemsBy queryParameters == Alphabetically)
                        True
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
                        (QueryParameters.orderItemsBy queryParameters == MostMentionedFirst)
                        True
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
                        (case QueryParameters.orderItemsBy queryParameters of
                            FocusedOn _ ->
                                True

                            _ ->
                                False
                        )
                        True
                        [ id ElementIds.orderItemsFocusedOn
                        , Html.Attributes.disabled <| mostRecentRawTermForOrderingItemsFocusedOn == Nothing
                        , Extras.HtmlAttribute.showMaybe
                            (\termId ->
                                Html.Events.onClick <| PageMsg.Internal <| ChangeOrderItemsBy <| FocusedOn termId
                            )
                            mostRecentRawTermForOrderingItemsFocusedOn
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
                            , Components.SelectMenu.onChange (PageMsg.Internal << ChangeOrderItemsBy << FocusedOn << RawTerm.fromString)
                            , Components.SelectMenu.enabled True
                            ]
                            (disambiguatedPreferredTermsWithDefinitions
                                |> List.map
                                    (\disambiguatedPreferredTerm ->
                                        let
                                            preferredRawTerm : RawTerm
                                            preferredRawTerm =
                                                disambiguatedPreferredTerm
                                                    |> DisambiguatedTerm.toTerm
                                                    |> Term.raw
                                        in
                                        Components.SelectMenu.Choice
                                            (RawTerm.toString preferredRawTerm)
                                            [ text <| Term.inlineText <| DisambiguatedTerm.toTerm disambiguatedPreferredTerm ]
                                            (mostRecentRawTermForOrderingItemsFocusedOn == Just preferredRawTerm)
                                    )
                            )
                        ]
                    ]
                ]
            ]
        , Extras.Html.showIf (QueryParameters.orderItemsBy queryParameters == MostMentionedFirst) <|
            p
                [ class "mt-2 text-gray-700 dark:text-gray-300" ]
                [ text I18n.explanationForMostMentionedFirst ]
        , Extras.Html.showMaybe
            (DisambiguatedTerm.toTerm
                >> Term.view enableMathSupport []
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

    else if model.menuForMobileVisibility == GradualVisibility.Visible then
        MenuForMobileShown

    else if model.layout == ShowSingleItem then
        model.itemWithFocus
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


pageTitle : Model -> GlossaryForUi -> String
pageTitle model glossaryForUi =
    let
        glossaryTitle : String
        glossaryTitle =
            glossaryForUi |> GlossaryForUi.title |> GlossaryTitle.inlineText
    in
    case ( model.layout, model.itemWithFocus ) of
        ( ShowSingleItem, Just id ) ->
            let
                disambiguatedPreferredTerm : Maybe String
                disambiguatedPreferredTerm =
                    glossaryForUi
                        |> GlossaryForUi.items
                        |> GlossaryItemsForUi.get id
                        |> Maybe.map
                            (\item ->
                                item
                                    |> GlossaryItemForUi.disambiguatedPreferredTerm
                                    |> DisambiguatedTerm.toTerm
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
            model
                |> filterByTagId
                |> Maybe.andThen (filterByTagWithDescription glossaryForUi)
                |> Maybe.map
                    (\describedTag ->
                        Tag.inlineText (DescribedTag.tag describedTag) ++ " · " ++ glossaryTitle
                    )
                |> Maybe.withDefault glossaryTitle


filterByTagWithDescription : GlossaryForUi -> TagId -> Maybe DescribedTag
filterByTagWithDescription glossaryForUi tagId =
    let
        items : GlossaryItemsForUi
        items =
            GlossaryForUi.items glossaryForUi
    in
    GlossaryItemsForUi.tagFromId tagId items
        |> Maybe.andThen
            (\tag ->
                GlossaryItemsForUi.tagDescriptionFromId tagId items
                    |> Maybe.map (DescribedTag.create tagId tag)
            )


isControlOrCommandK : Bool -> (Extras.HtmlEvents.KeyDownEvent -> Bool)
isControlOrCommandK runningOnMacOs =
    if runningOnMacOs then
        Extras.HtmlEvents.isMetaK

    else
        Extras.HtmlEvents.isControlK


viewAboutSection :
    Maybe DescribedTag
    -> { enableMathSupport : Bool, noModalDialogShown_ : Bool }
    -> GlossaryForUi
    -> Html Msg
viewAboutSection filterByTagWithDescription_ { enableMathSupport } glossaryForUi =
    filterByTagWithDescription_
        |> Maybe.map
            (DescribedTag.description
                >> TagDescription.view
                    { enableMathSupport = enableMathSupport }
                    []
            )
        |> Maybe.withDefault
            (glossaryForUi
                |> GlossaryForUi.aboutSection
                |> Components.AboutSection.view
                    { enableMathSupport = enableMathSupport }
            )


viewOrderItemsButtonsAndItemCards :
    Maybe DescribedTag
    -> Bool
    -> Editability
    -> QueryParameters
    -> Maybe GlossaryItemId
    -> Maybe RawTerm
    -> GlossaryForUi
    -> Html Msg
viewOrderItemsButtonsAndItemCards filterByTagWithDescription_ enableMathSupport editability queryParameters itemWithFocus mostRecentRawTermForOrderingItemsFocusedOn glossaryForUi =
    let
        items : GlossaryItemsForUi
        items =
            GlossaryForUi.items glossaryForUi

        filterByTagId_ : Maybe TagId
        filterByTagId_ =
            Maybe.map DescribedTag.id filterByTagWithDescription_
    in
    items
        |> (case QueryParameters.orderItemsBy queryParameters of
                Alphabetically ->
                    GlossaryItemsForUi.orderedAlphabetically filterByTagId_
                        >> (\lhs -> ( lhs, [] ))

                MostMentionedFirst ->
                    GlossaryItemsForUi.orderedByMostMentionedFirst filterByTagId_
                        >> (\lhs -> ( lhs, [] ))

                FocusedOn termId ->
                    let
                        itemId : Maybe GlossaryItemId
                        itemId =
                            GlossaryItemsForUi.itemIdFromRawDisambiguatedPreferredTerm termId items
                    in
                    case itemId of
                        Just itemId_ ->
                            GlossaryItemsForUi.orderedFocusedOn filterByTagId_ itemId_

                        Nothing ->
                            always
                                (items
                                    |> GlossaryItemsForUi.orderedAlphabetically filterByTagId_
                                    |> (\lhs -> ( lhs, [] ))
                                )
           )
        |> viewCards
            { enableMathSupport = enableMathSupport
            , enableOrderItemsButtons = GlossaryForUi.enableOrderItemsButtons glossaryForUi
            , editable = Editability.editing editability
            , enableLastUpdatedDates = GlossaryForUi.enableLastUpdatedDates glossaryForUi
            , editing = Editability.editing editability
            }
            { filterByTagId_ = filterByTagId_
            , tags = GlossaryItemsForUi.tags items
            , filterByDescribedTag_ = filterByTagWithDescription_
            }
            queryParameters
            itemWithFocus
            mostRecentRawTermForOrderingItemsFocusedOn
            items


viewSearchDialog : Maybe DescribedTag -> Bool -> SearchDialog -> Html Msg
viewSearchDialog filterByTagWithDescription_ enableMathSupport searchDialog =
    Components.SearchDialog.view
        (PageMsg.Internal << SearchDialogMsg)
        searchDialog.model
        searchDialog.term
        (filterByTagWithDescription_
            |> Maybe.map
                (\describedTag ->
                    span
                        [ class "inline-flex flex-wrap items-center" ]
                        [ Icons.exclamation
                            [ Svg.Attributes.class "h-6 w-6 text-red-600 dark:text-red-800 mr-1.5"
                            , Accessibility.Aria.hidden True
                            ]
                        , span
                            [ class "mr-1" ]
                            [ text I18n.filteringByTag ]
                        , span []
                            [ text " \""
                            , Tag.view enableMathSupport [] <| DescribedTag.tag describedTag
                            , text "\""
                            ]
                        ]
                )
        )
        searchDialog.results


viewMain :
    Maybe DescribedTag
    -> { enableMathSupport : Bool, noModalDialogShown_ : Bool }
    -> Editability
    -> QueryParameters
    -> Maybe GlossaryItemId
    -> Maybe RawTerm
    -> SearchDialog
    -> Maybe GlossaryItemId
    -> Layout
    -> Saving
    -> GlossaryForUi
    -> Html Msg
viewMain filterByTagWithDescription_ { enableMathSupport, noModalDialogShown_ } editability queryParameters itemWithFocus mostRecentRawTermForOrderingItemsFocusedOn searchDialog confirmDeleteId layout deleting glossaryForUi =
    Html.main_
        []
        [ div
            [ Extras.HtmlAttribute.showIf (not noModalDialogShown_) <| Extras.HtmlAttribute.inert ]
            [ viewAboutSection
                filterByTagWithDescription_
                { enableMathSupport = enableMathSupport, noModalDialogShown_ = noModalDialogShown_ }
                glossaryForUi
            ]
        , Extras.Html.showIf (Editability.editing editability && filterByTagWithDescription_ == Nothing) <|
            div
                [ class "flex-none mt-2"
                , Extras.HtmlAttribute.showIf (not noModalDialogShown_) <| Extras.HtmlAttribute.inert
                ]
                [ viewEditTitleAndAboutButton noModalDialogShown_ ]
        , Html.article
            [ Html.Attributes.id ElementIds.items
            , Extras.HtmlAttribute.showIf (GlossaryForUi.enableOrderItemsButtons glossaryForUi) <| class "mt-3 pt-2 border-t border-gray-300 dark:border-gray-700"
            ]
            [ div
                [ Extras.HtmlAttribute.showIf (not noModalDialogShown_) Extras.HtmlAttribute.inert ]
                [ Html.Lazy.lazy7 viewOrderItemsButtonsAndItemCards
                    filterByTagWithDescription_
                    enableMathSupport
                    editability
                    queryParameters
                    (if noModalDialogShown_ then
                        itemWithFocus

                     else
                        Nothing
                    )
                    mostRecentRawTermForOrderingItemsFocusedOn
                    glossaryForUi
                ]
            , Html.Lazy.lazy3 viewSearchDialog filterByTagWithDescription_ enableMathSupport searchDialog
            , Html.Lazy.lazy3 viewConfirmDeleteModal editability confirmDeleteId deleting
            , Html.Lazy.lazy8 viewSingleItemModalDialog
                itemWithFocus
                enableMathSupport
                (Editability.editing editability)
                (GlossaryForUi.enableLastUpdatedDates glossaryForUi)
                filterByTagWithDescription_
                (QueryParameters.orderItemsBy queryParameters)
                (GlossaryForUi.items glossaryForUi)
              <|
                case ( layout, itemWithFocus ) of
                    ( ShowSingleItem, Just id ) ->
                        Just id

                    _ ->
                        Nothing
            ]
        ]


view : Model -> Document Msg
view model =
    case model.common.glossaryForUi of
        Ok glossaryForUi ->
            let
                noModalDialogShown_ : Bool
                noModalDialogShown_ =
                    noModalDialogShown model

                items : GlossaryItemsForUi
                items =
                    GlossaryForUi.items glossaryForUi

                filterByTagId_ : Maybe TagId
                filterByTagId_ =
                    filterByTagId model

                filterByTagWithDescription_ : Maybe DescribedTag
                filterByTagWithDescription_ =
                    Maybe.andThen
                        (filterByTagWithDescription glossaryForUi)
                        filterByTagId_

                isControlOrCommandK_ : Extras.HtmlEvents.KeyDownEvent -> Bool
                isControlOrCommandK_ =
                    isControlOrCommandK model.common.runningOnMacOs
            in
            { title = pageTitle model glossaryForUi
            , body =
                [ Html.div
                    [ class "min-h-full focus:outline-hidden"
                    , Html.Attributes.id ElementIds.outer
                    , Accessibility.Key.tabbable True
                    , Html.Events.preventDefaultOn "keydown"
                        (Extras.HtmlEvents.preventDefaultOnDecoder
                            (\event ->
                                case menuOrDialogShown model of
                                    ConfirmDeleteModalDialogShown index ->
                                        if Extras.HtmlEvents.isEscape event then
                                            Just <| ( PageMsg.Internal CancelDelete, True )

                                        else if Extras.HtmlEvents.isEnter event then
                                            Just <| ( PageMsg.Internal <| Delete index, True )

                                        else
                                            Nothing

                                    SearchDialogShown ->
                                        if Extras.HtmlEvents.isEscape event || isControlOrCommandK_ event then
                                            Just <| ( PageMsg.Internal <| SearchDialogMsg Components.SearchDialog.hide, True )

                                        else
                                            Nothing

                                    MenuForMobileShown ->
                                        if Extras.HtmlEvents.isEscape event then
                                            Just <| ( PageMsg.Internal StartHidingMenuForMobile, True )

                                        else
                                            Nothing

                                    ViewSingleItemModalDialogShown id ->
                                        if Extras.HtmlEvents.isEscape event then
                                            Just <| ( PageMsg.Internal ChangeLayoutToShowAll, True )

                                        else
                                            let
                                                itemWithPreviousAndNext : GlossaryItemWithPreviousAndNext
                                                itemWithPreviousAndNext =
                                                    items
                                                        |> (case QueryParameters.orderItemsBy model.common.queryParameters of
                                                                Alphabetically ->
                                                                    GlossaryItemsForUi.orderedAlphabetically filterByTagId_

                                                                MostMentionedFirst ->
                                                                    GlossaryItemsForUi.orderedByMostMentionedFirst filterByTagId_

                                                                FocusedOn termId ->
                                                                    \items_ ->
                                                                        GlossaryItemsForUi.itemIdFromRawDisambiguatedPreferredTerm termId items_
                                                                            |> Maybe.map
                                                                                (\itemId -> GlossaryItemsForUi.orderedFocusedOn filterByTagId_ itemId items_)
                                                                            |> Maybe.withDefault ( [], [] )
                                                                            |> (\( lhs, rhs ) -> List.append lhs rhs)
                                                           )
                                                        |> itemWithPreviousAndNextForId id
                                            in
                                            if Extras.HtmlEvents.isLeftArrow event then
                                                itemWithPreviousAndNext.previous
                                                    |> Maybe.map
                                                        (\newItem -> ( PageMsg.Internal <| ChangeLayoutToShowSingle <| GlossaryItemForUi.id newItem, True ))

                                            else if Extras.HtmlEvents.isRightArrow event then
                                                itemWithPreviousAndNext.next
                                                    |> Maybe.map
                                                        (\newItem -> ( PageMsg.Internal <| ChangeLayoutToShowSingle <| GlossaryItemForUi.id newItem, True ))

                                            else
                                                Nothing

                                    NoMenuOrDialogShown ->
                                        if isControlOrCommandK_ event then
                                            Just <| ( PageMsg.Internal <| SearchDialogMsg Components.SearchDialog.show, True )

                                        else if Editability.canEdit model.common.editability && Extras.HtmlEvents.isE event && not event.isFormField then
                                            Just <| ( PageMsg.Internal MakeChanges, True )

                                        else if Editability.editing model.common.editability && Extras.HtmlEvents.isN event && not event.isFormField then
                                            Just <| ( PageMsg.NavigateToCreateOrEdit Nothing, True )

                                        else
                                            Nothing
                            )
                        )
                    ]
                    [ div
                        [ Extras.HtmlAttribute.showIf (not noModalDialogShown_) <| Extras.HtmlAttribute.inert
                        ]
                        [ Html.Lazy.lazy5 viewMenuForMobileAndStaticSidebarForDesktop
                            model.menuForMobileVisibility
                            model.common.enableMathSupport
                            model.indexFilterString
                            filterByTagId_
                            items
                        ]
                    , div
                        [ class "hidden lg:block" ]
                        [ Html.Lazy.lazy2 viewBackToTopLink True model.backToTopLinkVisibility ]
                    , div
                        [ class "lg:hidden" ]
                        [ Html.Lazy.lazy2 viewBackToTopLink False model.backToTopLinkVisibility ]
                    , div
                        [ class "lg:pl-64 flex flex-col" ]
                        [ Html.Lazy.lazy5 viewTopBar
                            noModalDialogShown_
                            model.common.runningOnMacOs
                            model.common.theme
                            model.themeDropdownMenu
                            (if GlossaryForUi.enableExportMenu glossaryForUi then
                                Just model.exportDropdownMenu

                             else
                                Nothing
                            )
                        , div
                            [ Html.Attributes.id ElementIds.container
                            , class "relative"
                            , Extras.HtmlAttribute.fromBool "data-markdown-rendered" True
                            , glossaryForUi |> GlossaryForUi.cardWidth |> CardWidth.toHtmlTreeAttribute |> HtmlTree.attributeToHtmlAttribute
                            ]
                            [ header [] <|
                                let
                                    showExportButton : Bool
                                    showExportButton =
                                        GlossaryForUi.enableExportMenu glossaryForUi
                                in
                                [ div
                                    [ class "lg:border-b border-gray-300 dark:border-gray-700 lg:mb-4" ]
                                    [ div
                                        [ class "flex flex-row justify-start lg:justify-end" ]
                                        [ Extras.Html.showIf (Editability.canEdit model.common.editability) <|
                                            viewMakeChangesButton model.common.editability noModalDialogShown_
                                        , div
                                            [ class "hidden lg:block ml-auto pt-0.5" ]
                                            [ viewQuickSearchButton model.common.runningOnMacOs
                                            ]
                                        , div
                                            [ class "hidden lg:block pl-4 pb-3 pt-0.5" ]
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
                                , Extras.Html.showIf (Editability.editing model.common.editability) <|
                                    viewSettings glossaryForUi
                                        model.common.editability
                                        model.savingSettings
                                        { tabbable = noModalDialogShown model
                                        , enableMathSupport = model.common.enableMathSupport
                                        }
                                , h1
                                    [ id ElementIds.title ]
                                    [ filterByTagWithDescription_
                                        |> Maybe.map
                                            (DescribedTag.tag
                                                >> Tag.view
                                                    model.common.enableMathSupport
                                                    [ class "text-3xl font-bold leading-tight" ]
                                            )
                                        |> Maybe.withDefault
                                            (glossaryForUi
                                                |> GlossaryForUi.title
                                                |> GlossaryTitle.view model.common.enableMathSupport [ class "text-3xl font-bold leading-tight" ]
                                            )
                                    ]
                                , Extras.Html.showIf (filterByTagWithDescription_ /= Nothing) <|
                                    h2
                                        [ class "mt-2 font-bold leading-tight" ]
                                        [ glossaryForUi |> GlossaryForUi.title |> GlossaryTitle.view model.common.enableMathSupport [ class "text-xl font-medium text-gray-700 dark:text-gray-300" ]
                                        ]
                                ]
                            , viewMain
                                filterByTagWithDescription_
                                { enableMathSupport = model.common.enableMathSupport
                                , noModalDialogShown_ = noModalDialogShown_
                                }
                                model.common.editability
                                model.common.queryParameters
                                model.itemWithFocus
                                model.mostRecentRawTermForOrderingItemsFocusedOn
                                model.searchDialog
                                model.confirmDeleteId
                                model.layout
                                model.deleting
                                glossaryForUi
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
        , scrollingUpWhileFarAwayFromTheTop (always <| PageMsg.Internal ScrollingUpWhileFarAwayFromTheTop)
        ]
