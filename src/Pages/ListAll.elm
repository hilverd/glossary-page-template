port module Pages.ListAll exposing (InternalMsg, ItemSearchDialog, Layout, MenuForMobileVisibility, Model, Msg, init, subscriptions, update, view)

import Accessibility
    exposing
        ( Html
        , aside
        , button
        , details
        , div
        , fieldset
        , h1
        , h2
        , h3
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
import Components.Combobox
import Components.Dividers
import Components.DropdownMenu
import Components.GlossaryItemCard
import Components.IncubatingGlossaryItemCard
import Components.ModalDialog
import Components.Notifications
import Components.SearchDialog
import Components.Spinner
import Data.CardWidth as CardWidth exposing (CardWidth)
import Data.DescribedTag as DescribedTag exposing (DescribedTag)
import Data.Editability as Editability exposing (Editability(..))
import Data.GlossaryChange as GlossaryChange exposing (GlossaryChange)
import Data.GlossaryChangeWithChecksum exposing (GlossaryChangeWithChecksum)
import Data.GlossaryChangelist as GlossaryChangelist exposing (GlossaryChangelist)
import Data.GlossaryForUi as GlossaryForUi exposing (GlossaryForUi, enableThreeColumnLayout)
import Data.GlossaryItem.DisambiguatedTerm as DisambiguatedTerm exposing (DisambiguatedTerm)
import Data.GlossaryItem.RawTerm as RawTerm
import Data.GlossaryItem.Tag as Tag exposing (Tag)
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItemForUi as GlossaryItemForUi exposing (GlossaryItemForUi)
import Data.GlossaryItemId as GlossaryItemId exposing (GlossaryItemId)
import Data.GlossaryItemWithPreviousAndNext exposing (GlossaryItemWithPreviousAndNext)
import Data.GlossaryItemsForUi as GlossaryItemsForUi exposing (GlossaryItemsForUi)
import Data.GlossaryTitle as GlossaryTitle exposing (GlossaryTitle)
import Data.GradualVisibility as GradualVisibility exposing (GradualVisibility)
import Data.IndexOfTerms as IndexOfTerms exposing (IndexOfTerms, TermGroup)
import Data.Notification exposing (Notification)
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


type alias ItemSearchDialog =
    { term : String
    , results : { totalNumberOfResults : Int, results : List Components.SearchDialog.SearchResult }
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
    , itemSearchDialog : ItemSearchDialog
    , layout : Layout
    , startingItemCombobox : Components.Combobox.Model
    , startingItemComboboxInput : String
    , itemWithFocus : Maybe GlossaryItemId
    , itemWithFocusCombobox : Components.Combobox.Model
    , itemWithFocusComboboxInput : String
    , confirmDeleteId : Maybe GlossaryItemId
    , deleting : Saving
    , savingSettings : Saving
    , resultOfAttemptingToCopyEditorCommandToClipboard : Maybe Bool
    , resultOfAttemptingToCopyItemTextToClipboard : Maybe ( GlossaryItemId, Bool )
    , notifications : Components.Notifications.Model
    }


type InternalMsg
    = NoOp
    | StartEditing
    | StopEditing
    | ShowMenuForMobile
    | StartHidingMenuForMobile
    | CompleteHidingMenuForMobile
    | BackToTop Bool Int
    | ThemeDropdownMenuMsg Components.DropdownMenu.Msg
    | ExportDropdownMenuMsg Components.DropdownMenu.Msg
    | ItemSearchDialogMsg Components.SearchDialog.Msg
    | ItemSearchDialogWasHidden
    | UpdateIndexFilterString String
    | UpdateItemSearchString String
    | ChangeTheme Theme
    | ScrollingUpWhileFarAwayFromTheTop
    | StartHidingBackToTopLink Int
    | CompleteHidingBackToTopLink Int
    | ImmediatelyHideBackToTopLink
    | JumpToItem GlossaryItemId
    | ChangeLayoutToShowSingle GlossaryItemId
    | CopyItemTextToClipboard GlossaryItemId
    | ShowRelatedTermAsSingle Term
    | ChangeLayoutToShowAll
    | StartingItemComboboxMsg Components.Combobox.Msg
    | UpdateStartingItemComboboxInput String
    | UpdateStartingItemComboboxInputToCurrent
    | ChangeStartingItem DisambiguatedTerm
    | ClearStartingItem
    | ItemWithFocusComboboxMsg Components.Combobox.Msg
    | UpdateItemWithFocusComboboxInput Bool String
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
    | AttemptedToCopyItemTextToClipboard ( String, Bool )
    | ClearResultOfAttemptingToCopyItemTextToClipboard
    | FilterByTag Tag
    | DoNotFilterByTag
    | NotificationsMsg Components.Notifications.Msg
    | RemoveTagFilterButKeepSearchDialogOpen


type alias Msg =
    PageMsg InternalMsg


init :
    CommonModel
    -> Maybe GlossaryItemId
    -> Maybe Components.Notifications.Model
    -> Maybe Notification
    -> ( Model, Cmd Msg )
init commonModel itemWithFocus notifications notification =
    let
        notifications0 : Components.Notifications.Model
        notifications0 =
            Maybe.withDefault Components.Notifications.init notifications

        ( notifications1, notificationsCmd ) =
            case notification of
                Just notification_ ->
                    Components.Notifications.addNotification notification_ notifications0

                Nothing ->
                    ( notifications0, Cmd.none )

        startingItemComboboxInput : String
        startingItemComboboxInput =
            commonModel.glossaryForUi
                |> Result.toMaybe
                |> Maybe.map (\glossaryForUi -> GlossaryForUi.items glossaryForUi)
                |> Maybe.andThen
                    (\items ->
                        GlossaryItemsForUi.startingItem items
                    )
                |> Maybe.map
                    (GlossaryItemForUi.disambiguatedPreferredTerm
                        >> DisambiguatedTerm.toTerm
                        >> Term.raw
                        >> RawTerm.toString
                    )
                |> Maybe.withDefault ""
    in
    ( { common = commonModel
      , menuForMobileVisibility = GradualVisibility.Invisible
      , backToTopLinkVisibility = Invisible 0
      , layout = ShowAllItems
      , startingItemCombobox = Components.Combobox.init
      , startingItemComboboxInput = startingItemComboboxInput
      , itemWithFocus = itemWithFocus
      , itemWithFocusCombobox = Components.Combobox.init
      , itemWithFocusComboboxInput =
            case QueryParameters.orderItemsBy commonModel.queryParameters of
                FocusedOn rawTerm ->
                    RawTerm.toString rawTerm

                _ ->
                    ""
      , confirmDeleteId = Nothing
      , themeDropdownMenu =
            Components.DropdownMenu.init
                [ Components.DropdownMenu.id ElementIds.themeDropdownButton ]
      , exportDropdownMenu =
            Components.DropdownMenu.init
                [ Components.DropdownMenu.id ElementIds.exportDropdownButton ]
      , indexFilterString = ""
      , itemSearchDialog =
            { term = ""
            , results = { totalNumberOfResults = 0, results = [] }
            , model =
                Components.SearchDialog.init ElementIds.itemSearchDialog
                    [ Components.SearchDialog.onChangeSearchString (PageMsg.Internal << UpdateItemSearchString)
                    , Components.SearchDialog.onShow <|
                        Cmd.batch
                            [ preventBackgroundScrolling ()
                            , Extras.Task.messageToCommand <| PageMsg.Internal ImmediatelyHideBackToTopLink
                            ]
                    , Components.SearchDialog.onHide <| Extras.Task.messageToCommand <| PageMsg.Internal ItemSearchDialogWasHidden
                    ]
            }
      , deleting = NotCurrentlySaving
      , savingSettings = NotCurrentlySaving
      , resultOfAttemptingToCopyEditorCommandToClipboard = Nothing
      , resultOfAttemptingToCopyItemTextToClipboard = Nothing
      , notifications = notifications1
      }
    , Cmd.batch
        [ notificationsCmd |> Cmd.map (PageMsg.Internal << NotificationsMsg)
        , case itemWithFocus of
            Just id ->
                scrollGlossaryItemIntoView id

            Nothing ->
                commonModel.fragment
                    |> Maybe.map (Extras.BrowserDom.scrollElementIntoView <| PageMsg.Internal NoOp)
                    |> Maybe.withDefault Cmd.none
        ]
    )



-- PORTS


port allowBackgroundScrolling : () -> Cmd msg


port preventBackgroundScrolling : () -> Cmd msg


port changeTheme : Maybe String -> Cmd msg


port scrollElementIntoView : String -> Cmd msg


port copyEditorCommandToClipboard : String -> Cmd msg


port attemptedToCopyEditorCommandToClipboard : (Bool -> msg) -> Sub msg


port copyItemTextToClipboard : ( String, String ) -> Cmd msg


port attemptedToCopyItemTextToClipboard : (( String, Bool ) -> msg) -> Sub msg


port selectAllInTextFieldWithCommandToRunEditor : () -> Cmd msg


port scrollingUpWhileFarAwayFromTheTop : (() -> msg) -> Sub msg



-- UPDATE


maximumNumberOfResultsForItemSearchDialog : Int
maximumNumberOfResultsForItemSearchDialog =
    10


maximumNumberOfResultsForItemWithFocusCombobox : Int
maximumNumberOfResultsForItemWithFocusCombobox =
    10


maximumNumberOfResultsForStartingItemCombobox : Int
maximumNumberOfResultsForStartingItemCombobox =
    10


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        StartEditing ->
            let
                common0 : CommonModel
                common0 =
                    model.common
            in
            ( { model
                | common =
                    { common0
                        | editability = Editability.startEditing model.common.editability
                    }
              }
            , Cmd.none
            )

        StopEditing ->
            let
                common0 : CommonModel
                common0 =
                    model.common
            in
            ( { model
                | common =
                    { common0
                        | editability = Editability.stopEditing model.common.editability
                    }
              }
            , Cmd.none
            )

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

        ItemSearchDialogMsg msg_ ->
            Components.SearchDialog.update
                (\x ->
                    let
                        itemSearchDialog0 : ItemSearchDialog
                        itemSearchDialog0 =
                            model.itemSearchDialog
                    in
                    { model | itemSearchDialog = { itemSearchDialog0 | model = x } }
                )
                (PageMsg.Internal << ItemSearchDialogMsg)
                msg_
                model.itemSearchDialog.model

        ItemSearchDialogWasHidden ->
            ( let
                itemSearchDialog0 : ItemSearchDialog
                itemSearchDialog0 =
                    model.itemSearchDialog
              in
              { model
                | itemSearchDialog =
                    { itemSearchDialog0
                        | term = ""
                        , results = { totalNumberOfResults = 0, results = [] }
                    }
              }
            , allowBackgroundScrolling ()
            )

        UpdateIndexFilterString string ->
            ( { model | indexFilterString = string }, Cmd.none )

        UpdateItemSearchString itemSearchString ->
            let
                itemSearchDialog0 : ItemSearchDialog
                itemSearchDialog0 =
                    model.itemSearchDialog

                resultsToShowInDialog : { totalNumberOfResults : Int, results : List Components.SearchDialog.SearchResult }
                resultsToShowInDialog =
                    case model.common.glossaryForUi of
                        Ok glossaryForUi ->
                            glossaryForUi
                                |> GlossaryForUi.items
                                |> Search.resultsForItems (filterByTagId model) (always True) maximumNumberOfResultsForItemSearchDialog itemSearchString
                                |> (\{ totalNumberOfResults, results } ->
                                        { totalNumberOfResults = totalNumberOfResults
                                        , results =
                                            results
                                                |> List.map
                                                    (\({ disambiguatedPreferredTerm } as searchResult) ->
                                                        Components.SearchDialog.searchResult
                                                            (Extras.Url.fragmentOnly <| Term.id <| DisambiguatedTerm.toTerm disambiguatedPreferredTerm)
                                                            [ Search.viewItemSearchResult model.common.enableMathSupport [] searchResult ]
                                                    )
                                        }
                                   )

                        Err _ ->
                            { totalNumberOfResults = 0, results = [] }

                model1 : Model
                model1 =
                    { model
                        | itemSearchDialog =
                            { itemSearchDialog0
                                | term = itemSearchString
                                , results = resultsToShowInDialog
                            }
                    }
            in
            update (ItemSearchDialogMsg Components.SearchDialog.searchStringWasJustUpdated) model1

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

        CopyItemTextToClipboard glossaryItemId ->
            (case model.common.glossaryForUi of
                Ok glossaryForUi ->
                    glossaryForUi
                        |> GlossaryForUi.items
                        |> GlossaryItemsForUi.get glossaryItemId

                _ ->
                    Nothing
            )
                |> Maybe.map
                    (\glossaryItemForUi ->
                        let
                            textToCopy =
                                Export.Markdown.itemToMarkdown glossaryItemForUi
                        in
                        ( model
                        , copyItemTextToClipboard
                            ( GlossaryItemId.toString glossaryItemId
                            , textToCopy
                            )
                        )
                    )
                |> Maybe.withDefault ( model, Cmd.none )

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

        StartingItemComboboxMsg msg_ ->
            Components.Combobox.update
                (\x -> { model | startingItemCombobox = x })
                (PageMsg.Internal << StartingItemComboboxMsg)
                msg_
                model.startingItemCombobox

        UpdateStartingItemComboboxInput input ->
            ( { model
                | startingItemComboboxInput = input
                , startingItemCombobox = Components.Combobox.showChoices model.startingItemCombobox
              }
            , Cmd.none
            )

        UpdateStartingItemComboboxInputToCurrent ->
            let
                currentStartingItem =
                    model.common.glossaryForUi
                        |> Result.toMaybe
                        |> Maybe.map (\glossaryForUi -> GlossaryForUi.items glossaryForUi)
                        |> Maybe.andThen
                            (\items ->
                                GlossaryItemsForUi.startingItem items
                            )
                        |> Maybe.map
                            (GlossaryItemForUi.disambiguatedPreferredTerm
                                >> DisambiguatedTerm.toTerm
                                >> Term.raw
                                >> RawTerm.toString
                            )
                        |> Maybe.withDefault ""
            in
            ( { model
                | startingItemComboboxInput = currentStartingItem
                , startingItemCombobox = Components.Combobox.hideChoices model.startingItemCombobox
              }
            , Cmd.none
            )

        ChangeStartingItem disambiguatedPreferredTerm ->
            case model.common.glossaryForUi of
                Ok glossaryForUi ->
                    let
                        glossaryChange : GlossaryChange
                        glossaryChange =
                            disambiguatedPreferredTerm
                                |> DisambiguatedTerm.toTerm
                                |> Term.toTermFromDom
                                |> GlossaryChange.SetStartingItem

                        glossaryChangeWithChecksum : GlossaryChangeWithChecksum
                        glossaryChangeWithChecksum =
                            { glossaryChange = glossaryChange
                            , checksum = GlossaryForUi.checksumForChange glossaryForUi glossaryChange
                            }

                        changelist : GlossaryChangelist
                        changelist =
                            GlossaryChangelist.create
                                (GlossaryForUi.versionNumber glossaryForUi)
                                [ glossaryChangeWithChecksum ]

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
                                        (Just yourChangesHaveBeenSavedNotification)
                                )
                    in
                    ( { model
                        | confirmDeleteId = Nothing
                        , deleting = NotCurrentlySaving
                        , savingSettings = saving
                        , startingItemComboboxInput =
                            disambiguatedPreferredTerm
                                |> DisambiguatedTerm.toTerm
                                |> Term.raw
                                |> RawTerm.toString
                      }
                    , cmd
                    )

                _ ->
                    ( model, Cmd.none )

        ClearStartingItem ->
            case model.common.glossaryForUi of
                Ok glossaryForUi ->
                    let
                        glossaryChange : GlossaryChange
                        glossaryChange =
                            GlossaryChange.ClearStartingItem

                        glossaryChangeWithChecksum : GlossaryChangeWithChecksum
                        glossaryChangeWithChecksum =
                            { glossaryChange = glossaryChange
                            , checksum = GlossaryForUi.checksumForChange glossaryForUi glossaryChange
                            }

                        changelist : GlossaryChangelist
                        changelist =
                            GlossaryChangelist.create
                                (GlossaryForUi.versionNumber glossaryForUi)
                                [ glossaryChangeWithChecksum ]

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
                                        (Just yourChangesHaveBeenSavedNotification)
                                )
                    in
                    ( { model
                        | confirmDeleteId = Nothing
                        , deleting = NotCurrentlySaving
                        , savingSettings = saving
                        , startingItemComboboxInput = ""
                      }
                    , cmd
                    )

                _ ->
                    ( model, Cmd.none )

        ItemWithFocusComboboxMsg msg_ ->
            Components.Combobox.update
                (\x -> { model | itemWithFocusCombobox = x })
                (PageMsg.Internal << ItemWithFocusComboboxMsg)
                msg_
                model.itemWithFocusCombobox

        UpdateItemWithFocusComboboxInput hideChoices input ->
            ( { model
                | itemWithFocusComboboxInput = input
                , itemWithFocusCombobox =
                    if hideChoices then
                        Components.Combobox.hideChoices model.itemWithFocusCombobox

                    else
                        Components.Combobox.showChoices model.itemWithFocusCombobox
              }
            , Cmd.none
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
                        glossaryChange : GlossaryChange
                        glossaryChange =
                            GlossaryChange.Remove id

                        glossaryChangeWithChecksum : GlossaryChangeWithChecksum
                        glossaryChangeWithChecksum =
                            { glossaryChange = glossaryChange
                            , checksum = GlossaryForUi.checksumForChange glossaryForUi glossaryChange
                            }

                        changelist : GlossaryChangelist
                        changelist =
                            GlossaryChangelist.create
                                (GlossaryForUi.versionNumber glossaryForUi)
                                [ glossaryChangeWithChecksum ]

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
                ( notifications_, notificationsCmd ) =
                    Components.Notifications.addNotification
                        { title = text I18n.deleted
                        , body = text I18n.theItemHasBeenDeleted
                        }
                        model.notifications

                enableThreeColumnLayout : Bool
                enableThreeColumnLayout =
                    GlossaryForUi.enableThreeColumnLayout updatedGlossaryForUi

                common : CommonModel
                common =
                    model.common

                cmd : Cmd Msg
                cmd =
                    Cmd.batch
                        [ allowBackgroundScrolling ()
                        , giveFocusToOuter
                        , Cmd.map (PageMsg.Internal << NotificationsMsg) notificationsCmd
                        , if enableThreeColumnLayout then
                            Cmd.map PageMsg.Internal <| Extras.BrowserDom.scrollToTop NoOp

                          else
                            Cmd.none
                        ]
            in
            ( { model
                | common = { common | glossaryForUi = Ok <| updatedGlossaryForUi }
                , notifications = notifications_
                , itemWithFocus =
                    updatedGlossaryForUi
                        |> GlossaryForUi.items
                        |> GlossaryItemsForUi.startingItem
                        |> Maybe.map GlossaryItemForUi.id
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
                                        ElementIds.letterLinksGrid
                                            |> Dom.getElement
                                            |> Task.andThen
                                                (\letterLinksGridElement ->
                                                    let
                                                        height : Float
                                                        height =
                                                            letterLinksGridElement.element.height
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

                updatedQueryParameters : QueryParameters
                updatedQueryParameters =
                    QueryParameters.setOrderItemsBy orderItemsBy model.common.queryParameters

                common1 : CommonModel
                common1 =
                    { common | queryParameters = updatedQueryParameters }
            in
            ( { model
                | common = common1
                , itemWithFocusComboboxInput =
                    case orderItemsBy of
                        FocusedOn rawTerm ->
                            RawTerm.toString rawTerm

                        _ ->
                            ""
                , itemWithFocusCombobox = model.itemWithFocusCombobox |> Components.Combobox.hideChoices
              }
            , common1
                |> CommonModel.relativeUrl
                |> Navigation.pushUrl model.common.key
            )

        ChangeCardWidth cardWidth ->
            case model.common.glossaryForUi of
                Ok glossaryForUi ->
                    let
                        glossaryChange : GlossaryChange
                        glossaryChange =
                            GlossaryChange.SetCardWidth cardWidth

                        glossaryChangeWithChecksum : GlossaryChangeWithChecksum
                        glossaryChangeWithChecksum =
                            { glossaryChange = glossaryChange
                            , checksum = GlossaryForUi.checksumForChange glossaryForUi glossaryChange
                            }

                        changelist : GlossaryChangelist
                        changelist =
                            GlossaryChangelist.create
                                (GlossaryForUi.versionNumber glossaryForUi)
                                [ glossaryChangeWithChecksum ]

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
                        glossaryChange : GlossaryChange
                        glossaryChange =
                            GlossaryChange.SetDefaultTheme defaultTheme

                        glossaryChangeWithChecksum : GlossaryChangeWithChecksum
                        glossaryChangeWithChecksum =
                            { glossaryChange = glossaryChange
                            , checksum = GlossaryForUi.checksumForChange glossaryForUi glossaryChange
                            }

                        changelist : GlossaryChangelist
                        changelist =
                            GlossaryChangelist.create
                                (GlossaryForUi.versionNumber glossaryForUi)
                                [ glossaryChangeWithChecksum ]

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
                        glossaryChange : GlossaryChange
                        glossaryChange =
                            GlossaryChange.ToggleEnableExportMenu

                        glossaryChangeWithChecksum : GlossaryChangeWithChecksum
                        glossaryChangeWithChecksum =
                            { glossaryChange = glossaryChange
                            , checksum = GlossaryForUi.checksumForChange glossaryForUi glossaryChange
                            }

                        changelist : GlossaryChangelist
                        changelist =
                            GlossaryChangelist.create
                                (GlossaryForUi.versionNumber glossaryForUi)
                                [ glossaryChangeWithChecksum ]

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
                                        (Just yourChangesHaveBeenSavedNotification)
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
                        glossaryChange : GlossaryChange
                        glossaryChange =
                            GlossaryChange.ToggleEnableOrderItemsButtons

                        glossaryChangeWithChecksum : GlossaryChangeWithChecksum
                        glossaryChangeWithChecksum =
                            { glossaryChange = glossaryChange
                            , checksum = GlossaryForUi.checksumForChange glossaryForUi glossaryChange
                            }

                        changelist : GlossaryChangelist
                        changelist =
                            GlossaryChangelist.create
                                (GlossaryForUi.versionNumber glossaryForUi)
                                [ glossaryChangeWithChecksum ]

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
                                        (Just yourChangesHaveBeenSavedNotification)
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
                        glossaryChange : GlossaryChange
                        glossaryChange =
                            GlossaryChange.ToggleEnableLastUpdatedDates

                        glossaryChangeWithChecksum : GlossaryChangeWithChecksum
                        glossaryChangeWithChecksum =
                            { glossaryChange = glossaryChange
                            , checksum = GlossaryForUi.checksumForChange glossaryForUi glossaryChange
                            }

                        changelist : GlossaryChangelist
                        changelist =
                            GlossaryChangelist.create
                                (GlossaryForUi.versionNumber glossaryForUi)
                                [ glossaryChangeWithChecksum ]

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
                                        (Just yourChangesHaveBeenSavedNotification)
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

                ( notifications1, notificationsCmd ) =
                    Components.Notifications.addNotification
                        yourChangesHaveBeenSavedNotification
                        model.notifications
            in
            ( { model
                | common = { common | glossaryForUi = Ok <| updatedGlossaryForUi }
                , savingSettings = NotCurrentlySaving
                , notifications = notifications1
              }
            , Cmd.batch
                [ notificationsCmd |> Cmd.map (PageMsg.Internal << NotificationsMsg)
                , if common.editability == Editability.EditingInMemory then
                    Cmd.none

                  else
                    Navigation.reload
                ]
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

        AttemptedToCopyItemTextToClipboard ( glossaryItemId, success ) ->
            ( { model
                | resultOfAttemptingToCopyItemTextToClipboard = Just ( GlossaryItemId.create glossaryItemId, success )
              }
            , Process.sleep 1000 |> Task.perform (always <| PageMsg.Internal ClearResultOfAttemptingToCopyItemTextToClipboard)
            )

        ClearResultOfAttemptingToCopyItemTextToClipboard ->
            ( { model | resultOfAttemptingToCopyItemTextToClipboard = Nothing }, Cmd.none )

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

        RemoveTagFilterButKeepSearchDialogOpen ->
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

                -- Re-run the search with the current search string to update results
                model1 : Model
                model1 =
                    { model | common = common1 }

                urlCmd =
                    common1
                        |> CommonModel.relativeUrl
                        |> Navigation.pushUrl model.common.key

                focusCmd =
                    Task.attempt
                        (always NoOp)
                        (Dom.focus <| Components.SearchDialog.searchStringFieldId model.itemSearchDialog.model)
                        |> Cmd.map PageMsg.Internal
            in
            model1
                |> update (UpdateItemSearchString model.itemSearchDialog.term)
                |> (\( model2, c ) -> ( model2, Cmd.batch [ c, urlCmd, focusCmd ] ))

        NotificationsMsg notificationsMsg ->
            let
                ( notifications_, cmd ) =
                    Components.Notifications.update notificationsMsg model.notifications
            in
            ( { model | notifications = notifications_ }
            , Cmd.map (PageMsg.Internal << NotificationsMsg) cmd
            )


yourChangesHaveBeenSavedNotification : Notification
yourChangesHaveBeenSavedNotification =
    { title = text I18n.saved
    , body = text I18n.yourChangesHaveBeenSaved
    }


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


viewSettings : GlossaryForUi -> Editability -> Saving -> { tabbable : Bool, enableMathSupport : Bool } -> Components.Combobox.Model -> String -> Html Msg
viewSettings glossaryForUi editability savingSettings { tabbable, enableMathSupport } startingItemCombobox startingItemComboboxInput =
    let
        enableThreeColumnLayout : Bool
        enableThreeColumnLayout =
            GlossaryForUi.enableThreeColumnLayout glossaryForUi

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
        [ class "pt-4 pr-4 pl-4 pb-2 rounded-md max-w-xl overflow-x-auto"
        , Extras.HtmlAttribute.showUnless enableThreeColumnLayout <| class "mb-5"
        , if enableThreeColumnLayout then
            class "bg-gray-100 dark:bg-gray-900"

          else
            class "bg-amber-50 dark:bg-gray-700 text-gray-700 dark:text-gray-300 print:hidden"
        ]
        [ details
            [ Accessibility.Key.tabbable tabbable
            , class "relative"
            , Extras.HtmlAttribute.showIf (Components.Combobox.choicesVisible startingItemCombobox) <| class "sm:pb-20"
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
                , viewSelectStartingItem
                    enableMathSupport
                    (GlossaryForUi.items glossaryForUi)
                    startingItemCombobox
                    startingItemComboboxInput
                , Extras.Html.showUnless enableThreeColumnLayout <|
                    viewSelectCardWidth glossaryForUi tabbable
                , viewSelectDefaultTheme glossaryForUi tabbable
                , Components.Button.toggle
                    (GlossaryForUi.enableExportMenu glossaryForUi)
                    ElementIds.showExportMenuLabel
                    [ Html.Events.onClick <| PageMsg.Internal ToggleEnableExportMenu ]
                    [ span
                        [ class "font-medium text-gray-900 dark:text-gray-300" ]
                        [ text I18n.showExportMenu ]
                    ]
                , Extras.Html.showUnless enableThreeColumnLayout <|
                    Components.Button.toggle
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


viewSelectStartingItem :
    Bool
    -> GlossaryItemsForUi
    -> Components.Combobox.Model
    -> String
    -> Html Msg
viewSelectStartingItem enableMathSupport glossaryItemsForUi startingItemCombobox startingItemComboboxInput =
    let
        comboboxChoices : { totalNumberOfResults : Int, results : List (Components.Combobox.Choice DisambiguatedTerm (PageMsg InternalMsg)) }
        comboboxChoices =
            Search.resultsForItems
                Nothing
                (always True)
                maximumNumberOfResultsForItemWithFocusCombobox
                startingItemComboboxInput
                glossaryItemsForUi
                |> (\{ totalNumberOfResults, results } ->
                        { totalNumberOfResults = totalNumberOfResults
                        , results =
                            results
                                |> List.map
                                    (\({ disambiguatedPreferredTerm } as result) ->
                                        Components.Combobox.choice
                                            disambiguatedPreferredTerm
                                            (\additionalAttributes ->
                                                Search.viewItemSearchResult
                                                    enableMathSupport
                                                    additionalAttributes
                                                    result
                                            )
                                    )
                        }
                   )
    in
    div
        []
        [ fieldset []
            [ legend
                [ class "mb-4 font-medium text-gray-900 dark:text-gray-100" ]
                [ text I18n.startingItemForNewLayout ]
            , Components.Combobox.view
                (PageMsg.Internal << StartingItemComboboxMsg)
                startingItemCombobox
                [ Components.Combobox.id ElementIds.startingItemCombobox
                , Components.Combobox.onSelect (PageMsg.Internal << ChangeStartingItem)
                , Components.Combobox.onInput (PageMsg.Internal << UpdateStartingItemComboboxInput)
                , Components.Combobox.onBlur
                    (PageMsg.Internal <|
                        UpdateStartingItemComboboxInputToCurrent
                    )
                ]
                Nothing
                comboboxChoices.results
                (if comboboxChoices.totalNumberOfResults > maximumNumberOfResultsForStartingItemCombobox then
                    Just <| I18n.showingXOfYMatches (String.fromInt maximumNumberOfResultsForStartingItemCombobox) (String.fromInt comboboxChoices.totalNumberOfResults)

                 else if startingItemComboboxInput /= "" && comboboxChoices.totalNumberOfResults == 0 then
                    Just I18n.noMatchesFound

                 else
                    Nothing
                )
                startingItemComboboxInput
            ]
        , Extras.Html.showIf
            (GlossaryItemsForUi.startingItem glossaryItemsForUi /= Nothing)
          <|
            viewSwitchBackToOldLayout
        ]


viewSwitchBackToOldLayout : Html Msg
viewSwitchBackToOldLayout =
    div
        [ class "mt-4" ]
        [ Components.Button.white True
            [ Html.Events.onClick <| PageMsg.Internal ClearStartingItem ]
            [ text I18n.switchBackToOldLayout ]
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
        [ h3
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
    -> Maybe Bool
    -> GlossaryItemWithPreviousAndNext
    -> Html Msg
viewGlossaryItem { enableMathSupport, editable, enableLastUpdatedDates, shownAsSingle } itemWithFocus tagBeingFilteredBy resultOfAttemptingToCopyItemTextToClipboard itemWithPreviousAndNext =
    Extras.Html.showMaybe
        (\item ->
            Components.GlossaryItemCard.view
                { enableMathSupport = enableMathSupport, enableLastUpdatedDates = enableLastUpdatedDates }
                (Components.GlossaryItemCard.Normal
                    { onClickViewFull = PageMsg.Internal <| ChangeLayoutToShowSingle <| GlossaryItemForUi.id item
                    , onClickCopyToClipboard = PageMsg.Internal <| CopyItemTextToClipboard <| GlossaryItemForUi.id item
                    , onClickEdit = PageMsg.NavigateToCreateOrEdit <| Just <| GlossaryItemForUi.id item
                    , onClickDelete = PageMsg.Internal <| ConfirmDelete <| GlossaryItemForUi.id item
                    , onClickTag = PageMsg.Internal << FilterByTag
                    , onClickItem = PageMsg.Internal << ChangeLayoutToShowSingle
                    , onClickRelatedTerm = PageMsg.Internal << ShowRelatedTermAsSingle
                    , resultOfAttemptingToCopyItemTextToClipboard = resultOfAttemptingToCopyItemTextToClipboard
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
    -> Maybe DescribedTag
    -> OrderItemsBy
    -> Maybe Bool
    -> GlossaryForUi
    -> Maybe GlossaryItemId
    -> Html Msg
viewSingleItemModalDialog itemWithFocus enableMathSupport editable tagBeingFilteredBy orderItemsBy resultOfAttemptingToCopyItemTextToClipboard glossaryForUi =
    let
        items : GlossaryItemsForUi
        items =
            GlossaryForUi.items glossaryForUi

        enableLastUpdatedDates : Bool
        enableLastUpdatedDates =
            GlossaryForUi.enableLastUpdatedDates glossaryForUi

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
                            resultOfAttemptingToCopyItemTextToClipboard
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


viewStartEditingButton : Bool -> Html Msg
viewStartEditingButton tabbable =
    Components.Button.white True
        [ Html.Events.onClick <| PageMsg.Internal StartEditing
        , Accessibility.Key.tabbable tabbable
        ]
        [ Icons.pencil
            [ Svg.Attributes.class "h-5 w-5" ]
        , span
            [ class "hidden sm:ml-2 sm:inline-flex items-center" ]
            [ text I18n.startEditing
            , Html.kbd
                [ class "ml-2 inline-flex items-center rounded-xs border border-gray-700 dark:border-gray-300 px-1 font-sans text-xs" ]
                [ text "e" ]
            ]
        ]


viewStopEditingButton : Bool -> Html Msg
viewStopEditingButton tabbable =
    div
        []
        [ Components.Button.white True
            [ Html.Events.onClick <| PageMsg.Internal StopEditing
            , Accessibility.Key.tabbable tabbable
            ]
            [ text I18n.stopEditing ]
        ]


viewEditTitleAndAboutButton : Bool -> Html Msg
viewEditTitleAndAboutButton tabbable =
    div
        [ class "pb-3 print:hidden" ]
        [ Components.Button.text
            [ Html.Events.onClick <| PageMsg.NavigateToEditTitleAndAbout
            , Accessibility.Key.tabbable tabbable
            , class "group"
            ]
            [ Icons.pencil
                [ Svg.Attributes.class "h-5 w-5 text-gray-400 dark:text-gray-300 group-hover:text-gray-500 dark:group-hover:text-gray-100" ]
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


viewTagFilterAndOrderItemsByAndItemCards :
    { enableMathSupport : Bool
    , enableOrderItemsButtons : Bool
    , editable : Bool
    , enableLastUpdatedDates : Bool
    , editing : Bool
    }
    ->
        { tags : List Tag
        , filterByDescribedTag : Maybe DescribedTag
        }
    -> QueryParameters
    -> Maybe GlossaryItemId
    -> Maybe ( GlossaryItemId, Bool )
    -> Components.Combobox.Model
    -> String
    -> GlossaryItemsForUi
    -> ( List ( GlossaryItemId, GlossaryItemForUi ), List ( GlossaryItemId, GlossaryItemForUi ) )
    -> Html Msg
viewTagFilterAndOrderItemsByAndItemCards { enableMathSupport, enableOrderItemsButtons, editable, enableLastUpdatedDates, editing } { tags, filterByDescribedTag } queryParameters itemWithFocus resultOfAttemptingToCopyItemTextToClipboard itemWithFocusCombobox itemWithFocusComboboxInput glossaryItemsForUi ( indexedGlossaryItems, otherIndexedGlossaryItems ) =
    let
        totalNumberOfItems : Int
        totalNumberOfItems =
            List.length indexedGlossaryItems + List.length otherIndexedGlossaryItems
    in
    div
        []
        [ div
            []
            (viewTagFilterAndOrderItemsBy
                { enableMathSupport = enableMathSupport
                , enableOrderItemsButtons = enableOrderItemsButtons
                , editable = editable
                , editing = editing
                }
                { tags = tags, filterByDescribedTag = filterByDescribedTag }
                queryParameters
                itemWithFocusCombobox
                itemWithFocusComboboxInput
                glossaryItemsForUi
                totalNumberOfItems
            )
        , div []
            (viewItemCards
                enableMathSupport
                editable
                enableLastUpdatedDates
                filterByDescribedTag
                itemWithFocus
                resultOfAttemptingToCopyItemTextToClipboard
                indexedGlossaryItems
                otherIndexedGlossaryItems
            )
        ]


viewTagFilterAndOrderItemsBy :
    { enableMathSupport : Bool
    , enableOrderItemsButtons : Bool
    , editable : Bool
    , editing : Bool
    }
    ->
        { tags : List Tag
        , filterByDescribedTag : Maybe DescribedTag
        }
    -> QueryParameters
    -> Components.Combobox.Model
    -> String
    -> GlossaryItemsForUi
    -> Int
    -> List (Html Msg)
viewTagFilterAndOrderItemsBy { enableMathSupport, enableOrderItemsButtons, editable, editing } { tags, filterByDescribedTag } queryParameters itemWithFocusCombobox itemWithFocusComboboxInput glossaryItemsForUi totalNumberOfItems =
    let
        orderItemsFocusedOnTerm : Maybe DisambiguatedTerm
        orderItemsFocusedOnTerm =
            case QueryParameters.orderItemsBy queryParameters of
                FocusedOn termId ->
                    GlossaryItemsForUi.disambiguatedPreferredTermFromRaw termId glossaryItemsForUi

                _ ->
                    Nothing

        recommendedMaximumNumberOfItems : Int
        recommendedMaximumNumberOfItems =
            1000
    in
    [ div
        [ class "mb-4" ]
        [ Extras.Html.showMaybe
            (viewCurrentTagFilter
                [ class "pt-3 inline-flex items-center flex-wrap" ]
                enableMathSupport
            )
            filterByDescribedTag
        , Extras.Html.showIf (filterByDescribedTag == Nothing) <|
            viewAllTagFilters enableMathSupport tags
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
                [ if totalNumberOfItems == 0 then
                    viewCreateGlossaryItemButtonForEmptyState

                  else
                    viewCreateGlossaryItemButton
                ]
        ]
    , Extras.Html.showIf (editable && totalNumberOfItems > recommendedMaximumNumberOfItems) <|
        I18n.glossaryContainsTooManyItems recommendedMaximumNumberOfItems
    , Extras.Html.showIf
        (totalNumberOfItems == 0 && filterByDescribedTag /= Nothing)
      <|
        div
            [ class "mt-4" ]
            [ text I18n.noMatchingItemsFound ]
    , Extras.Html.showIf (enableOrderItemsButtons && (totalNumberOfItems > 0)) <|
        viewOrderItemsBy
            totalNumberOfItems
            enableMathSupport
            (Maybe.map DescribedTag.id filterByDescribedTag)
            itemWithFocusCombobox
            itemWithFocusComboboxInput
            glossaryItemsForUi
            orderItemsFocusedOnTerm
            queryParameters
    ]


viewItemCards :
    Bool
    -> Bool
    -> Bool
    -> Maybe DescribedTag
    -> Maybe GlossaryItemId
    -> Maybe ( GlossaryItemId, Bool )
    -> List ( GlossaryItemId, GlossaryItemForUi )
    -> List ( GlossaryItemId, GlossaryItemForUi )
    -> List (Html Msg)
viewItemCards enableMathSupport editable enableLastUpdatedDates filterByDescribedTag itemWithFocus resultOfAttemptingToCopyItemTextToClipboard indexedGlossaryItems otherIndexedGlossaryItems =
    let
        filterByTag : Maybe Tag
        filterByTag =
            Maybe.map DescribedTag.tag filterByDescribedTag

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
                (resultOfAttemptingToCopyItemTextToClipboard
                    |> Maybe.map (\( itemId, _ ) -> itemId == GlossaryItemForUi.id item)
                )
                { previous = Nothing, item = Just item, next = Nothing }

        viewIndexedItemKeyed : ( GlossaryItemId, GlossaryItemForUi ) -> ( String, Html Msg )
        viewIndexedItemKeyed ( itemId, item ) =
            ( GlossaryItemId.toString itemId
            , Html.Lazy.lazy viewIndexedItem item
            )
    in
    [ Html.Keyed.node "dl"
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
    Bool
    -> MenuForMobileVisibility
    -> Bool
    -> String
    -> Maybe DescribedTag
    -> GlossaryTitle
    -> GlossaryItemsForUi
    -> String
    -> Html Msg
viewMenuForMobileAndStaticSidebarForDesktop enableThreeColumnLayout menuForMobileVisibility enableMathSupport indexFilterString filterByTagWithDescription_ glossaryTitle items initialUrlWithoutQueryOrFragment =
    let
        indexOfTerms : IndexOfTerms
        indexOfTerms =
            IndexOfTerms.fromGlossaryItems
                (Maybe.map DescribedTag.id filterByTagWithDescription_)
                items
    in
    div []
        [ Html.Lazy.lazy7 viewMenuForMobile
            menuForMobileVisibility
            enableMathSupport
            enableThreeColumnLayout
            glossaryTitle
            filterByTagWithDescription_
            indexOfTerms
            initialUrlWithoutQueryOrFragment
        , Html.Lazy.lazy7 viewStaticSidebarForDesktop
            enableThreeColumnLayout
            enableMathSupport
            glossaryTitle
            filterByTagWithDescription_
            indexFilterString
            indexOfTerms
            initialUrlWithoutQueryOrFragment
        ]


viewMenuForMobile : MenuForMobileVisibility -> Bool -> Bool -> GlossaryTitle -> Maybe DescribedTag -> IndexOfTerms -> String -> Html Msg
viewMenuForMobile menuForMobileVisibility enableMathSupport enableThreeColumnLayout glossaryTitle filterByTagWithDescription_ termIndex initialUrlWithoutQueryOrFragment =
    div
        [ class "invisible" |> Extras.HtmlAttribute.showIf (menuForMobileVisibility == GradualVisibility.Invisible)
        , class "fixed inset-0 flex z-40 lg:hidden"
        , Accessibility.Role.dialog
        , Accessibility.Aria.modal True
        , Accessibility.Aria.label I18n.sidebar
        ]
        [ Html.div
            [ class "fixed inset-0 bg-black/75 dark:bg-black/70"
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
                [ Extras.Html.showIf enableThreeColumnLayout <|
                    h2
                        [ class "px-4 mt-5 font-bold leading-tight" ]
                        [ Html.a
                            [ href initialUrlWithoutQueryOrFragment ]
                            [ glossaryTitle
                                |> GlossaryTitle.view
                                    enableMathSupport
                                    [ class "text-xl font-medium text-gray-700 dark:text-gray-300" ]
                            ]
                        ]
                , Extras.Html.showMaybe
                    (\describedTag ->
                        div
                            [ class "px-4 pt-5" ]
                            [ span
                                [ class "text-gray-900 dark:text-white" ]
                                [ text <| I18n.filteringByTag ++ ":" ]
                            , Html.br [] []
                            , Components.Badge.withRemoveButtonAndWrappingText
                                (PageMsg.Internal DoNotFilterByTag)
                                [ class "print:hidden mt-2 mb-1 overflow-hidden" ]
                                [ Icons.tag
                                    [ Svg.Attributes.class "h-5 w-5 mr-1 text-gray-400 dark:text-gray-300 group-hover:text-gray-500 dark:group-hover:text-gray-100 flex-shrink-0" ]
                                , Tag.view enableMathSupport [] <| DescribedTag.tag describedTag
                                ]
                            ]
                    )
                    filterByTagWithDescription_
                , nav
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


viewQuickItemSearchButton : Bool -> Html Msg
viewQuickItemSearchButton runningOnMacOs =
    div
        [ class "w-full" ]
        [ div
            [ class "bg-gray-50 dark:bg-slate-900 relative pointer-events-auto" ]
            [ button
                [ Html.Attributes.type_ "button"
                , class "w-full flex items-center leading-6 text-slate-500 dark:text-slate-400 rounded-md ring-1 ring-slate-900/10 dark:ring-slate-600 shadow-xs py-1.5 pl-2 pr-3 hover:ring-slate-400 dark:hover:ring-slate-400 dark:bg-slate-800 dark:highlight-white/5 dark:hover:bg-slate-800 select-none"
                , Html.Events.onClick <| PageMsg.Internal <| ItemSearchDialogMsg Components.SearchDialog.show
                ]
                [ Icons.search
                    [ width "24"
                    , height "24"
                    , Svg.Attributes.class "mr-3 flex-none"
                    ]
                , text I18n.searchEllipsis
                , span
                    [ class "ml-auto pl-3 pr-1 flex-none text-sm font-semibold" ]
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
    if staticSidebar then
        Components.Button.soft enabled
            [ class "m-0.5 px-3 py-2 leading-4"
            , Html.Events.onClick <|
                PageMsg.Internal <|
                    if enabled then
                        JumpToTermIndexGroup staticSidebar firstCharacter

                    else
                        NoOp
            ]
            [ text firstCharacter ]

    else
        Components.Button.softLarge enabled
            [ class "m-1 leading-4"
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


viewIndexFilterInputField : Bool -> Maybe DescribedTag -> String -> Html Msg
viewIndexFilterInputField enableMathSupport filterByTagWithDescription_ indexFilterString =
    div
        [ class "pb-4" ]
        [ Extras.Html.showMaybe
            (\describedTag ->
                div
                    []
                    [ span
                        [ class "ml-1.5 text-gray-900 dark:text-white" ]
                        [ text <| I18n.filteringByTag ++ ":" ]
                    , Html.br [] []
                    , Components.Badge.withRemoveButtonAndWrappingText
                        (PageMsg.Internal DoNotFilterByTag)
                        [ class "print:hidden ml-1.5 mt-2 mb-2 overflow-hidden" ]
                        [ Icons.tag
                            [ Svg.Attributes.class "h-5 w-5 mr-1 text-gray-400 dark:text-gray-300 group-hover:text-gray-500 dark:group-hover:text-gray-100 flex-shrink-0" ]
                        , Tag.view enableMathSupport [] <| DescribedTag.tag describedTag
                        ]
                    ]
            )
            filterByTagWithDescription_
        , div
            [ class "mt-2 grid grid-cols-1"
            ]
            [ Html.input
                [ Html.Attributes.type_ "search"
                , id ElementIds.indexFilterInputField
                , class "col-start-1 row-start-1 block w-full pl-9 rounded-md focus:ring-indigo-500 focus:border-indigo-500 border-gray-300 dark:border-gray-500 dark:bg-gray-700 dark:text-white placeholder-gray-500 dark:placeholder-gray-400"
                , Html.Attributes.placeholder I18n.filter
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


viewLetterLinksGrid : Bool -> Bool -> Bool -> GlossaryTitle -> Maybe DescribedTag -> String -> IndexOfTerms -> String -> Html Msg
viewLetterLinksGrid enableMathSupport enableThreeColumnLayout staticSidebar glossaryTitle filterByTagWithDescription_ indexFilterString indexOfTerms initialUrlWithoutQueryOrFragment =
    div
        [ id ElementIds.letterLinksGrid
        , class "z-10 -mb-6 sticky top-0 -ml-0.5"
        ]
        [ div
            [ class "pt-5 px-3 bg-white dark:bg-slate-900" ]
            [ Extras.Html.showIf enableThreeColumnLayout <|
                h2
                    [ class "ml-1.5 mb-3 font-bold leading-tight" ]
                    [ Html.a
                        [ href initialUrlWithoutQueryOrFragment ]
                        [ glossaryTitle
                            |> GlossaryTitle.view
                                enableMathSupport
                                [ class "text-xl font-medium text-gray-700 dark:text-gray-300" ]
                        ]
                    ]
            , viewIndexFilterInputField enableMathSupport filterByTagWithDescription_ indexFilterString
            , viewTermIndexFirstCharacterGrid staticSidebar indexOfTerms
            ]
        , div
            [ class "h-8 bg-linear-to-b from-white dark:from-slate-900" ]
            []
        ]


viewStaticSidebarForDesktop : Bool -> Bool -> GlossaryTitle -> Maybe DescribedTag -> String -> IndexOfTerms -> String -> Html Msg
viewStaticSidebarForDesktop enableThreeColumnLayout enableMathSupport glossaryTitle filterByTagWithDescription_ indexFilterString termIndex initialUrlWithoutQueryOrFragment =
    let
        filteredTermIndex : IndexOfTerms
        filteredTermIndex =
            IndexOfTerms.filterByString indexFilterString termIndex
    in
    div
        [ class "hidden print:hidden lg:flex lg:flex-col lg:fixed lg:inset-y-0 lg:border-r lg:border-gray-200 lg:bg-white lg:dark:border-gray-800 lg:dark:bg-gray-900"
        , if enableThreeColumnLayout then
            class "lg:w-78"

          else
            class "lg:w-64"
        ]
        [ div
            [ id ElementIds.staticSidebarForDesktop
            , class "h-0 flex-1 flex flex-col overflow-y-scroll"
            ]
            [ viewLetterLinksGrid enableMathSupport enableThreeColumnLayout True glossaryTitle filterByTagWithDescription_ indexFilterString filteredTermIndex initialUrlWithoutQueryOrFragment
            , nav
                [ class "px-3" ]
                [ viewIndexOfTerms enableMathSupport True filteredTermIndex ]
            ]
        ]


viewTopBar : Bool -> Bool -> Editability -> Theme -> Components.DropdownMenu.Model -> Maybe Components.DropdownMenu.Model -> Html Msg
viewTopBar tabbable runningOnMacOs editability theme themeDropdownMenu maybeExportDropdownMenu =
    div
        [ class "sticky top-0 z-20 shrink-0 flex flex-row justify-between h-16 bg-white dark:bg-gray-900 border-b border-gray-200 dark:border-gray-800 lg:hidden print:hidden items-center" ]
        [ div
            [ class "flex items-center" ]
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
            [ class "hidden sm:block flex-1 pr-4" ]
            [ viewQuickItemSearchButton runningOnMacOs ]
        , div
            [ class "pr-4 sm:hidden flex-1" ]
            [ button
                [ Html.Attributes.type_ "button"
                , class "ml-auto text-slate-500 w-8 h-8 -my-1 flex items-center justify-center hover:text-slate-600 lg:hidden dark:text-slate-400 dark:hover:text-slate-300"
                , Html.Events.onClick <| PageMsg.Internal <| ItemSearchDialogMsg Components.SearchDialog.show
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
        , Extras.Html.showIf (Editability.canEdit editability) <|
            div
                [ class "mr-4" ]
                [ viewStartEditingButton tabbable ]
        , div
            [ class "flex pr-4" ]
            [ viewThemeButton True tabbable theme themeDropdownMenu ]
        , Extras.Html.showMaybe
            (\exportDropdownMenu ->
                div
                    [ class "flex pr-4" ]
                    [ viewExportButton True tabbable exportDropdownMenu ]
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


viewThemeButton : Bool -> Bool -> Theme -> Components.DropdownMenu.Model -> Html Msg
viewThemeButton forTopBar enabled theme themeDropdownMenu =
    Components.DropdownMenu.view
        (PageMsg.Internal << ThemeDropdownMenuMsg)
        themeDropdownMenu
        (if forTopBar then
            Just ElementIds.themeDropdownButtonForTopBar

         else
            Nothing
        )
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


viewExportButton : Bool -> Bool -> Components.DropdownMenu.Model -> Html Msg
viewExportButton forTopBar enabled exportDropdownMenu =
    Components.DropdownMenu.view
        (PageMsg.Internal << ExportDropdownMenuMsg)
        exportDropdownMenu
        (if forTopBar then
            Just ElementIds.exportDropdownButtonForTopBar

         else
            Nothing
        )
        enabled
        (Components.DropdownMenu.Chevron
            [ Icons.documentDownload
                [ Svg.Attributes.class "h-5 w-5" ]
            , span
                [ class "hidden sm:inline ml-2" ]
                [ text I18n.export ]
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


viewCurrentTagFilter : List (Accessibility.Attribute Never) -> Bool -> DescribedTag -> Html Msg
viewCurrentTagFilter additionalAttributes enableMathSupport describedTag =
    div
        additionalAttributes
        [ span
            [ class "print:hidden mr-2 mb-1 font-medium text-gray-900 dark:text-gray-100" ]
            [ text I18n.filteringByTag ]
        , Components.Badge.withRemoveButton
            (PageMsg.Internal DoNotFilterByTag)
            [ class "print:hidden" ]
            [ Icons.tag
                [ Svg.Attributes.class "h-5 w-5 mr-1 text-gray-400 dark:text-gray-300 group-hover:text-gray-500 dark:group-hover:text-gray-100 flex-shrink-0" ]
            , Tag.view enableMathSupport
                [ class "select-none" ]
              <|
                DescribedTag.tag describedTag
            ]
        ]


viewAllTagFilters : Bool -> List Tag -> Html Msg
viewAllTagFilters enableMathSupport tags =
    Extras.Html.showIf (not <| List.isEmpty tags) <|
        div
            [ class "print:hidden pt-3 inline-flex items-center flex-wrap" ]
            (span
                [ class "mr-2 mt-3 font-medium text-gray-900 dark:text-gray-100" ]
                [ text <| I18n.tags ++ ":"
                ]
                :: (tags
                        |> List.map
                            (\tag ->
                                Components.Button.soft
                                    True
                                    [ class "mr-2 mt-2"
                                    , Html.Events.onClick <| PageMsg.Internal <| FilterByTag tag
                                    ]
                                    [ Icons.tag
                                        [ Svg.Attributes.class "h-5 w-5 mr-1 text-gray-400 dark:text-gray-300 group-hover:text-gray-500 dark:group-hover:text-gray-100 flex-shrink-0" ]
                                    , Tag.view enableMathSupport [] tag
                                    ]
                            )
                   )
            )


viewManageTagsButton : Html Msg
viewManageTagsButton =
    div
        [ class "pb-3 print:hidden" ]
        [ Components.Button.text
            [ Html.Events.onClick <| PageMsg.NavigateToManageTags
            , class "group"
            ]
            [ Icons.pencil
                [ Svg.Attributes.class "h-5 w-5 text-gray-400 dark:text-gray-300 group-hover:text-gray-500 dark:group-hover:text-gray-100" ]
            , span
                [ class "ml-2" ]
                [ text I18n.manageTags ]
            ]
        ]


viewOrderItemsBy :
    Int
    -> Bool
    -> Maybe TagId
    -> Components.Combobox.Model
    -> String
    -> GlossaryItemsForUi
    -> Maybe DisambiguatedTerm
    -> QueryParameters
    -> Html Msg
viewOrderItemsBy numberOfItems enableMathSupport filterByTagId_ itemWithFocusCombobox itemWithFocusComboboxInput glossaryItemsForUi orderItemsFocusedOnTerm queryParameters =
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
                    [ class "flex flex-initial items-center" ]
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
                    [ class "flex flex-initial items-center" ]
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
                    [ class "flex flex-1 items-center max-w-md" ]
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
                        , Html.Attributes.disabled
                            (case QueryParameters.orderItemsBy queryParameters of
                                FocusedOn _ ->
                                    False

                                _ ->
                                    True
                            )
                        ]
                    , label
                        [ class "ml-3 inline-flex items-center font-medium text-gray-700 dark:text-gray-300 select-none"
                        , for ElementIds.orderItemsFocusedOn
                        ]
                        [ span
                            [ class "mr-2 text-nowrap" ]
                            [ text I18n.focusedOn
                            ]
                        ]
                    , let
                        comboboxChoices : { totalNumberOfResults : Int, results : List (Components.Combobox.Choice Term (PageMsg InternalMsg)) }
                        comboboxChoices =
                            Search.resultsForItems
                                filterByTagId_
                                (always True)
                                maximumNumberOfResultsForItemWithFocusCombobox
                                itemWithFocusComboboxInput
                                glossaryItemsForUi
                                |> (\{ totalNumberOfResults, results } ->
                                        { totalNumberOfResults = totalNumberOfResults
                                        , results =
                                            results
                                                |> List.map
                                                    (\({ disambiguatedPreferredTerm } as result) ->
                                                        Components.Combobox.choice
                                                            (disambiguatedPreferredTerm |> DisambiguatedTerm.toTerm)
                                                            (\additionalAttributes ->
                                                                Search.viewItemSearchResult
                                                                    enableMathSupport
                                                                    additionalAttributes
                                                                    result
                                                            )
                                                    )
                                        }
                                   )
                      in
                      Components.Combobox.view
                        (PageMsg.Internal << ItemWithFocusComboboxMsg)
                        itemWithFocusCombobox
                        [ Components.Combobox.id ElementIds.orderItemsFocusedOnCombobox
                        , Components.Combobox.onSelect (PageMsg.Internal << ChangeOrderItemsBy << FocusedOn << Term.raw)
                        , Components.Combobox.onInput (PageMsg.Internal << UpdateItemWithFocusComboboxInput False)
                        , Components.Combobox.onBlur
                            (PageMsg.Internal <|
                                UpdateItemWithFocusComboboxInput True itemWithFocusComboboxInput
                            )
                        ]
                        Nothing
                        comboboxChoices.results
                        (if comboboxChoices.totalNumberOfResults > maximumNumberOfResultsForItemWithFocusCombobox then
                            Just <| I18n.showingXOfYMatches (String.fromInt maximumNumberOfResultsForItemWithFocusCombobox) (String.fromInt comboboxChoices.totalNumberOfResults)

                         else if itemWithFocusComboboxInput /= "" && comboboxChoices.totalNumberOfResults == 0 then
                            Just I18n.noMatchesFound

                         else
                            Nothing
                        )
                        itemWithFocusComboboxInput
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
    | ItemSearchDialogShown
    | ConfirmDeleteModalDialogShown GlossaryItemId
    | ViewSingleItemModalDialogShown GlossaryItemId
    | NoMenuOrDialogShown


menuOrDialogShown : Model -> MenuOrDialogShown
menuOrDialogShown model =
    if Components.SearchDialog.visible model.itemSearchDialog.model then
        ItemSearchDialogShown

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

        ItemSearchDialogShown ->
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

        glossaryItemsForUi : GlossaryItemsForUi
        glossaryItemsForUi =
            GlossaryForUi.items glossaryForUi

        viewingSingleItem : Bool
        viewingSingleItem =
            model.layout
                == ShowSingleItem
                || GlossaryForUi.enableThreeColumnLayout glossaryForUi
    in
    case ( viewingSingleItem, model.itemWithFocus ) of
        ( True, Just id ) ->
            let
                disambiguatedPreferredTerm : Maybe String
                disambiguatedPreferredTerm =
                    glossaryItemsForUi
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
                |> Maybe.andThen (filterByTagWithDescription glossaryItemsForUi)
                |> Maybe.map
                    (\describedTag ->
                        Tag.inlineText (DescribedTag.tag describedTag) ++ " · " ++ glossaryTitle
                    )
                |> Maybe.withDefault glossaryTitle


filterByTagWithDescription : GlossaryItemsForUi -> TagId -> Maybe DescribedTag
filterByTagWithDescription glossaryItemsForUi tagId =
    GlossaryItemsForUi.tagFromId tagId glossaryItemsForUi
        |> Maybe.andThen
            (\tag ->
                GlossaryItemsForUi.tagDescriptionFromId tagId glossaryItemsForUi
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
    Bool
    -> Editability
    -> QueryParameters
    -> Maybe GlossaryItemId
    -> Maybe ( GlossaryItemId, Bool )
    -> Components.Combobox.Model
    -> String
    -> GlossaryForUi
    -> Html Msg
viewOrderItemsButtonsAndItemCards enableMathSupport editability queryParameters itemWithFocus resultOfAttemptingToCopyItemTextToClipboard itemWithFocusCombobox itemWithFocusComboboxInput glossaryForUi =
    let
        glossaryItemsForUi : GlossaryItemsForUi
        glossaryItemsForUi =
            GlossaryForUi.items glossaryForUi

        filterByTagId_ : Maybe TagId
        filterByTagId_ =
            queryParameters
                |> QueryParameters.filterByTag
                |> Maybe.andThen
                    (\tag -> GlossaryItemsForUi.tagIdFromTag tag glossaryItemsForUi)

        filterByTagWithDescription_ : Maybe DescribedTag
        filterByTagWithDescription_ =
            Maybe.andThen
                (filterByTagWithDescription glossaryItemsForUi)
                filterByTagId_
    in
    glossaryItemsForUi
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
                            GlossaryItemsForUi.itemIdFromRawDisambiguatedPreferredTerm termId glossaryItemsForUi
                    in
                    case itemId of
                        Just itemId_ ->
                            GlossaryItemsForUi.orderedFocusedOn filterByTagId_ itemId_

                        Nothing ->
                            always
                                (glossaryItemsForUi
                                    |> GlossaryItemsForUi.orderedAlphabetically filterByTagId_
                                    |> (\lhs -> ( lhs, [] ))
                                )
           )
        |> viewTagFilterAndOrderItemsByAndItemCards
            { enableMathSupport = enableMathSupport
            , enableOrderItemsButtons = GlossaryForUi.enableOrderItemsButtons glossaryForUi
            , editable = Editability.editing editability
            , enableLastUpdatedDates = GlossaryForUi.enableLastUpdatedDates glossaryForUi
            , editing = Editability.editing editability
            }
            { tags = GlossaryItemsForUi.tags glossaryItemsForUi
            , filterByDescribedTag = filterByTagWithDescription_
            }
            queryParameters
            itemWithFocus
            resultOfAttemptingToCopyItemTextToClipboard
            itemWithFocusCombobox
            itemWithFocusComboboxInput
            glossaryItemsForUi


viewItemSearchDialog : Maybe DescribedTag -> Bool -> ItemSearchDialog -> Html Msg
viewItemSearchDialog filterByTagWithDescription_ enableMathSupport itemSearchDialog =
    let
        totalNumberOfResults : Int
        totalNumberOfResults =
            itemSearchDialog.results.totalNumberOfResults
    in
    Components.SearchDialog.view
        (PageMsg.Internal << ItemSearchDialogMsg)
        itemSearchDialog.model
        itemSearchDialog.term
        (filterByTagWithDescription_
            |> Maybe.map
                (\describedTag ->
                    span [ class "pt-3 flex items-center" ]
                        [ Icons.exclamation
                            [ Svg.Attributes.class "h-6 w-6 text-red-600 dark:text-red-500 mr-1.5 shrink-0"
                            , Accessibility.Aria.hidden True
                            ]
                        , span
                            [ class "print:hidden mr-2 font-medium text-gray-900 dark:text-gray-100" ]
                            [ text I18n.filteringByTag ]
                        , Components.Badge.withRemoveButton
                            (PageMsg.Internal RemoveTagFilterButKeepSearchDialogOpen)
                            [ class "print:hidden" ]
                            [ Icons.tag
                                [ Svg.Attributes.class "h-5 w-5 mr-1 text-gray-400 dark:text-gray-300 group-hover:text-gray-500 dark:group-hover:text-gray-100 flex-shrink-0" ]
                            , Tag.view enableMathSupport [] <| DescribedTag.tag describedTag
                            ]
                        ]
                )
        )
        (if totalNumberOfResults > maximumNumberOfResultsForItemSearchDialog then
            Just <| I18n.showingXOfYMatches (String.fromInt maximumNumberOfResultsForItemSearchDialog) (String.fromInt totalNumberOfResults)

         else if itemSearchDialog.term /= "" && totalNumberOfResults == 0 then
            Just I18n.noMatchesFound

         else
            Nothing
        )
        itemSearchDialog.results.results


viewMain :
    Maybe DescribedTag
    -> { enableMathSupport : Bool, noModalDialogShown_ : Bool }
    -> Editability
    -> QueryParameters
    -> Maybe GlossaryItemId
    -> ItemSearchDialog
    -> Maybe GlossaryItemId
    -> Layout
    -> Saving
    -> Maybe ( GlossaryItemId, Bool )
    -> Components.Combobox.Model
    -> String
    -> GlossaryForUi
    -> Html Msg
viewMain filterByTagWithDescription_ { enableMathSupport, noModalDialogShown_ } editability queryParameters itemWithFocus itemSearchDialog confirmDeleteId layout deleting resultOfAttemptingToCopyItemTextToClipboard itemWithFocusCombobox itemWithFocusComboboxInput glossaryForUi =
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
                [ Html.Lazy.lazy8 viewOrderItemsButtonsAndItemCards
                    enableMathSupport
                    editability
                    queryParameters
                    (if noModalDialogShown_ then
                        itemWithFocus

                     else
                        Nothing
                    )
                    (if layout == ShowSingleItem then
                        Nothing

                     else
                        resultOfAttemptingToCopyItemTextToClipboard
                    )
                    itemWithFocusCombobox
                    itemWithFocusComboboxInput
                    glossaryForUi
                ]
            , Html.Lazy.lazy3 viewItemSearchDialog filterByTagWithDescription_ enableMathSupport itemSearchDialog
            , Html.Lazy.lazy3 viewConfirmDeleteModal editability confirmDeleteId deleting
            , Html.Lazy.lazy8 viewSingleItemModalDialog
                itemWithFocus
                enableMathSupport
                (Editability.editing editability)
                filterByTagWithDescription_
                (QueryParameters.orderItemsBy queryParameters)
                (resultOfAttemptingToCopyItemTextToClipboard
                    |> Maybe.map
                        (\( glossaryItemId, _ ) ->
                            Just glossaryItemId == itemWithFocus
                        )
                )
                glossaryForUi
              <|
                case ( layout, itemWithFocus ) of
                    ( ShowSingleItem, Just id ) ->
                        Just id

                    _ ->
                        Nothing
            ]
        ]


viewMainThreeColumnLayout :
    Maybe DescribedTag
    -> { enableMathSupport : Bool, noModalDialogShown_ : Bool }
    -> Editability
    -> QueryParameters
    -> Maybe GlossaryItemId
    -> ItemSearchDialog
    -> Maybe GlossaryItemId
    -> Saving
    -> Maybe ( GlossaryItemId, Bool )
    -> Maybe String
    -> GlossaryForUi
    -> Html Msg
viewMainThreeColumnLayout filterByTagWithDescription_ { enableMathSupport, noModalDialogShown_ } editability queryParameters itemWithFocus itemSearchDialog confirmDeleteId deleting resultOfAttemptingToCopyItemTextToClipboard currentFragment glossaryForUi =
    let
        items : GlossaryItemsForUi
        items =
            GlossaryForUi.items glossaryForUi

        filterByTagId_ : Maybe TagId
        filterByTagId_ =
            queryParameters
                |> QueryParameters.filterByTag
                |> Maybe.andThen
                    (\tag -> GlossaryItemsForUi.tagIdFromTag tag items)

        itemAndRelatedItems : Maybe ( GlossaryItemForUi, List GlossaryItemForUi )
        itemAndRelatedItems =
            itemWithFocus
                |> Maybe.andThen
                    (\itemWithFocus_ ->
                        GlossaryItemsForUi.getWithRelatedTermsFilteredByTagId filterByTagId_ itemWithFocus_ items
                            |> Maybe.map
                                (\item_ ->
                                    ( item_
                                    , GlossaryItemsForUi.relatedItems
                                        (GlossaryItemForUi.id item_)
                                        filterByTagId_
                                        items
                                    )
                                )
                    )
    in
    Extras.Html.showMaybe
        (\( item, relatedItems ) ->
            div
                []
                [ Extras.Html.showIf (Editability.editing editability) <|
                    div
                        [ class "flex-none mt-8 px-6 lg:px-8" ]
                        [ viewManageTagsButton ]
                , Extras.Html.showMaybe
                    (viewCurrentTagFilter
                        [ class "inline-flex items-center mt-4 mb-4 px-6 lg:px-8 mb-6" ]
                        enableMathSupport
                    )
                    filterByTagWithDescription_
                , Extras.Html.showIf (Editability.editing editability) <|
                    div
                        [ class "px-6 lg:px-8 pt-2" ]
                        [ if GlossaryItemsForUi.isEmpty items then
                            viewCreateGlossaryItemButtonForEmptyState

                          else
                            viewCreateGlossaryItemButton
                        ]
                , div
                    [ class "xl:flex xl:gap-8 px-6 lg:px-8 mt-2 lg:mt-6" ]
                    [ div
                        [ class "xl:shrink min-w-0 overflow-x-hidden max-w-[70ch]" ]
                        [ Html.main_
                            [ class "three-column-layout" ]
                            [ Html.article
                                []
                                [ div
                                    [ Extras.HtmlAttribute.showIf (not noModalDialogShown_) Extras.HtmlAttribute.inert ]
                                    [ Components.IncubatingGlossaryItemCard.view
                                        { enableMathSupport = enableMathSupport
                                        , enableLastUpdatedDates = GlossaryForUi.enableLastUpdatedDates glossaryForUi
                                        , onClickCopyToClipboard = PageMsg.Internal <| CopyItemTextToClipboard <| GlossaryItemForUi.id item
                                        , onClickEdit = PageMsg.NavigateToCreateOrEdit <| Just <| GlossaryItemForUi.id item
                                        , onClickDelete = PageMsg.Internal <| ConfirmDelete <| GlossaryItemForUi.id item
                                        , resultOfAttemptingToCopyItemTextToClipboard =
                                            resultOfAttemptingToCopyItemTextToClipboard
                                                |> Maybe.map
                                                    (\( glossaryItemId, _ ) ->
                                                        Just glossaryItemId == itemWithFocus
                                                    )
                                        , editable = Editability.editing editability
                                        }
                                        (Maybe.map DescribedTag.tag filterByTagWithDescription_)
                                        currentFragment
                                        item
                                    ]
                                , Html.Lazy.lazy3 viewItemSearchDialog filterByTagWithDescription_ enableMathSupport itemSearchDialog
                                , Html.Lazy.lazy3 viewConfirmDeleteModal editability confirmDeleteId deleting
                                ]
                            ]
                        ]
                    , aside
                        [ class "xl:flex-1 min-w-0 pt-8 xl:pt-0" ]
                        [ Extras.Html.showIf (not <| List.isEmpty relatedItems) <|
                            h3
                                [ class "text-xl font-medium text-gray-700 dark:text-gray-300 mb-4" ]
                                [ text I18n.relatedItems ]
                        , div
                            [ class "space-y-4" ]
                            (List.map
                                (Components.IncubatingGlossaryItemCard.viewRelatedItem enableMathSupport)
                                relatedItems
                            )
                        ]
                    ]
                ]
        )
        itemAndRelatedItems


view : Model -> Document Msg
view model =
    case model.common.glossaryForUi of
        Ok glossaryForUi ->
            let
                noModalDialogShown_ : Bool
                noModalDialogShown_ =
                    noModalDialogShown model

                glossaryItemsForUi : GlossaryItemsForUi
                glossaryItemsForUi =
                    GlossaryForUi.items glossaryForUi

                enableThreeColumnLayout : Bool
                enableThreeColumnLayout =
                    GlossaryForUi.enableThreeColumnLayout glossaryForUi

                filterByTagId_ : Maybe TagId
                filterByTagId_ =
                    filterByTagId model

                filterByTagWithDescription_ : Maybe DescribedTag
                filterByTagWithDescription_ =
                    Maybe.andThen
                        (filterByTagWithDescription glossaryItemsForUi)
                        filterByTagId_

                isControlOrCommandK_ : Extras.HtmlEvents.KeyDownEvent -> Bool
                isControlOrCommandK_ =
                    isControlOrCommandK model.common.runningOnMacOs
            in
            { title = pageTitle model glossaryForUi
            , body =
                [ Html.div
                    [ class "min-h-full focus:outline-hidden"
                    , Extras.HtmlAttribute.showIf enableThreeColumnLayout <| class "bg-white dark:bg-black print:bg-white"
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

                                    ItemSearchDialogShown ->
                                        if Extras.HtmlEvents.isEscape event || isControlOrCommandK_ event then
                                            Just <| ( PageMsg.Internal <| ItemSearchDialogMsg Components.SearchDialog.hide, True )

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
                                                    glossaryItemsForUi
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
                                            Just <| ( PageMsg.Internal <| ItemSearchDialogMsg Components.SearchDialog.show, True )

                                        else if Editability.canEdit model.common.editability && Extras.HtmlEvents.isE event && not event.isFormField then
                                            Just <| ( PageMsg.Internal StartEditing, True )

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
                        [ Html.Lazy.lazy8 viewMenuForMobileAndStaticSidebarForDesktop
                            enableThreeColumnLayout
                            model.menuForMobileVisibility
                            model.common.enableMathSupport
                            model.indexFilterString
                            filterByTagWithDescription_
                            (glossaryForUi |> GlossaryForUi.title)
                            glossaryItemsForUi
                            (CommonModel.initialUrlWithoutQueryOrFragment model.common)
                        ]
                    , div
                        [ class "hidden lg:block" ]
                        [ Html.Lazy.lazy2 viewBackToTopLink True model.backToTopLinkVisibility ]
                    , div
                        [ class "lg:hidden" ]
                        [ Extras.Html.showIf (model.menuForMobileVisibility /= GradualVisibility.Visible) <|
                            Html.Lazy.lazy2 viewBackToTopLink False model.backToTopLinkVisibility
                        ]
                    , div
                        [ class "flex flex-col"
                        , if enableThreeColumnLayout then
                            class "lg:pl-78"

                          else
                            class "lg:pl-64"
                        ]
                        [ Html.Lazy.lazy6 viewTopBar
                            noModalDialogShown_
                            model.common.runningOnMacOs
                            model.common.editability
                            model.common.theme
                            model.themeDropdownMenu
                            (if GlossaryForUi.enableExportMenu glossaryForUi then
                                Just model.exportDropdownMenu

                             else
                                Nothing
                            )
                        , div
                            [ Html.Attributes.id ElementIds.container
                            , class "relative print:bg-white"
                            , Extras.HtmlAttribute.showIf enableThreeColumnLayout <| class "three-column-layout"
                            , Extras.HtmlAttribute.fromBool "data-markdown-rendered" True
                            , glossaryForUi |> GlossaryForUi.cardWidth |> CardWidth.toHtmlTreeAttribute |> HtmlTree.attributeToHtmlAttribute
                            ]
                            [ div
                                [ class "pt-4 px-4 sm:px-6 lg:px-8 print:px-0 print:max-w-full lg:sticky lg:top-0 lg:z-20 lg:border-b lg:border-gray-200 lg:dark:border-gray-800 print:bg-white"
                                , if enableThreeColumnLayout then
                                    class "bg-white dark:bg-black"

                                  else
                                    class "bg-gray-100 dark:bg-gray-900 lg:bg-white lg:dark:bg-gray-900"
                                ]
                              <|
                                let
                                    showExportButton : Bool
                                    showExportButton =
                                        GlossaryForUi.enableExportMenu glossaryForUi
                                in
                                [ div
                                    [ class "flex flex-row gap-4" ]
                                    [ div
                                        [ class "hidden lg:flex lg:flex-1 pt-0.5" ]
                                        [ viewQuickItemSearchButton model.common.runningOnMacOs
                                        ]
                                    , Extras.Html.showIf (Editability.canEdit model.common.editability) <|
                                        div
                                            [ class "hidden lg:block flex-none print:hidden" ]
                                            [ viewStartEditingButton noModalDialogShown_
                                            ]
                                    , Extras.Html.showIf (Editability.editing model.common.editability) <|
                                        div
                                            [ class "ml-auto flex-none print:hidden" ]
                                            [ viewStopEditingButton noModalDialogShown_
                                            ]
                                    , div
                                        [ class "hidden lg:block pb-3 pt-0.5" ]
                                        [ viewThemeButton False noModalDialogShown_ model.common.theme model.themeDropdownMenu
                                        ]
                                    , Extras.Html.showIf showExportButton <|
                                        div
                                            [ class "hidden lg:block" ]
                                            [ span
                                                [ class "pb-3" ]
                                                [ viewExportButton False noModalDialogShown_ model.exportDropdownMenu ]
                                            ]
                                    ]
                                ]
                            , Extras.Html.showIf
                                ((model.common.editability == ReadOnlyWithHelpForMakingChanges)
                                    || Editability.editing model.common.editability
                                    || not enableThreeColumnLayout
                                )
                              <|
                                div
                                    [ class "mt-6 px-4 sm:px-6 lg:px-8 print:px-0 print:max-w-full" ]
                                    [ viewMakingChangesHelp model.resultOfAttemptingToCopyEditorCommandToClipboard model.common.filename noModalDialogShown_
                                        |> Extras.Html.showIf (model.common.editability == ReadOnlyWithHelpForMakingChanges)
                                    , Extras.Html.showIf (Editability.editing model.common.editability) <|
                                        viewSettings
                                            glossaryForUi
                                            model.common.editability
                                            model.savingSettings
                                            { tabbable = noModalDialogShown model
                                            , enableMathSupport = model.common.enableMathSupport
                                            }
                                            model.startingItemCombobox
                                            model.startingItemComboboxInput
                                    ]
                            , Extras.Html.showUnless enableThreeColumnLayout <|
                                header
                                    [ class "mt-0" ]
                                    [ h1
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
                                                    |> GlossaryTitle.view model.common.enableMathSupport [ class "text-3xl font-bold leading-tight print:text-black" ]
                                                )
                                        ]
                                    , Extras.Html.showIf (filterByTagWithDescription_ /= Nothing) <|
                                        h2
                                            [ class "mt-2 font-bold leading-tight" ]
                                            [ glossaryForUi
                                                |> GlossaryForUi.title
                                                |> GlossaryTitle.view
                                                    model.common.enableMathSupport
                                                    [ class "text-xl font-medium text-gray-700 dark:text-gray-300 print:text-black" ]
                                            ]
                                    ]
                            , if enableThreeColumnLayout then
                                viewMainThreeColumnLayout
                                    filterByTagWithDescription_
                                    { enableMathSupport = model.common.enableMathSupport
                                    , noModalDialogShown_ = noModalDialogShown_
                                    }
                                    model.common.editability
                                    model.common.queryParameters
                                    model.itemWithFocus
                                    model.itemSearchDialog
                                    model.confirmDeleteId
                                    model.deleting
                                    model.resultOfAttemptingToCopyItemTextToClipboard
                                    model.common.fragment
                                    glossaryForUi

                              else
                                viewMain
                                    filterByTagWithDescription_
                                    { enableMathSupport = model.common.enableMathSupport
                                    , noModalDialogShown_ = noModalDialogShown_
                                    }
                                    model.common.editability
                                    model.common.queryParameters
                                    model.itemWithFocus
                                    model.itemSearchDialog
                                    model.confirmDeleteId
                                    model.layout
                                    model.deleting
                                    model.resultOfAttemptingToCopyItemTextToClipboard
                                    model.itemWithFocusCombobox
                                    model.itemWithFocusComboboxInput
                                    glossaryForUi
                            , Html.footer
                                [ class "px-6 lg:px-8 print:px-0 pt-16 pb-6 mt-auto" ]
                                [ div
                                    [ class "pt-6 w-full flex justify-center text-sm text-gray-600 dark:text-gray-300 print:text-black" ]
                                    [ I18n.builtUsingGlossaryPageTemplate noModalDialogShown_ ]
                                ]
                            ]
                        ]
                    ]
                , model.notifications
                    |> Components.Notifications.view
                    |> Html.map (PageMsg.Internal << NotificationsMsg)
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
        , attemptedToCopyItemTextToClipboard (AttemptedToCopyItemTextToClipboard >> PageMsg.Internal)
        , scrollingUpWhileFarAwayFromTheTop (always <| PageMsg.Internal ScrollingUpWhileFarAwayFromTheTop)
        , model.startingItemCombobox
            |> Components.Combobox.subscriptions
            |> Sub.map (StartingItemComboboxMsg >> PageMsg.Internal)
        , model.itemWithFocusCombobox
            |> Components.Combobox.subscriptions
            |> Sub.map (ItemWithFocusComboboxMsg >> PageMsg.Internal)
        , Components.Notifications.subscriptions model.notifications
            |> Sub.map (NotificationsMsg >> PageMsg.Internal)
        ]
