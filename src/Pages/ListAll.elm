port module Pages.ListAll exposing (Model, Msg, init, subscriptions, update, view)

import Accessibility exposing (..)
import Accessibility.Aria
import Accessibility.Key exposing (tabbable)
import Accessibility.Role
import Browser exposing (Document)
import Browser.Dom as Dom
import CommonModel exposing (CommonModel)
import Components.Button
import Components.Copy
import Components.DropdownMenu
import Components.SearchDialog
import Data.AboutLink as AboutLink
import Data.AboutParagraph as AboutParagraph
import Data.AboutSection exposing (AboutSection(..))
import Data.Glossary as Glossary exposing (Glossary(..))
import Data.GlossaryItem as GlossaryItem exposing (GlossaryItem)
import Data.GlossaryItemIndex exposing (GlossaryItemIndex)
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Data.GlossaryTitle as GlossaryTitle
import Dict exposing (Dict)
import ElementIds
import Export.Anki
import Export.Markdown
import Extras.BrowserDom
import Extras.Html
import Extras.HtmlAttribute
import Extras.HtmlEvents
import Extras.HtmlTree as HtmlTree exposing (HtmlTree(..))
import Extras.Http
import Extras.Task
import Extras.Url exposing (fragmentOnly)
import Html
import Html.Attributes exposing (class, for, href, id, target)
import Html.Events
import Http
import Icons
import Json.Decode as Decode
import PageMsg exposing (PageMsg)
import Process
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


type alias MakingChanges =
    Bool


type alias SearchDialog =
    { term : String
    , results : List Components.SearchDialog.SearchResult
    , model : Components.SearchDialog.Model (PageMsg InternalMsg)
    }


type alias Model =
    { common : CommonModel
    , makingChanges : MakingChanges
    , menuForMobileVisibility : MenuForMobileVisibility
    , exportDropdownMenu : Components.DropdownMenu.Model
    , searchDialog : SearchDialog
    , confirmDeleteIndex : Maybe GlossaryItemIndex
    , errorWhileDeleting : Maybe ( GlossaryItemIndex, String )
    }


type InternalMsg
    = NoOp
    | MakeChanges
    | ShowMenuForMobile
    | StartHidingMenuForMobile
    | CompleteHidingMenuForMobile
    | BackToTop Bool
    | ExportDropdownMenuMsg Components.DropdownMenu.Msg
    | SearchDialogMsg Components.SearchDialog.Msg
    | HideSearchDialog
    | UpdateSearchTerm String
    | ConfirmDelete GlossaryItemIndex
    | CancelDelete
    | Delete GlossaryItemIndex
    | Deleted GlossaryItems
    | FailedToDelete GlossaryItemIndex Http.Error
    | JumpToTermIndexGroup Bool String
    | ChangeOrderItemsBy CommonModel.OrderItemsBy
    | DownloadMarkdown
    | DownloadAnki


type alias Msg =
    PageMsg InternalMsg


init : Bool -> CommonModel -> ( Model, Cmd Msg )
init editorIsRunning commonModel =
    ( { makingChanges = editorIsRunning
      , common = commonModel
      , menuForMobileVisibility = Invisible
      , confirmDeleteIndex = Nothing
      , exportDropdownMenu =
            Components.DropdownMenu.init
                [ Components.DropdownMenu.id ElementIds.exportDropdownButton ]
      , searchDialog =
            { term = ""
            , results = []
            , model =
                Components.SearchDialog.init ElementIds.searchDialog
                    [ Components.SearchDialog.onChangeSearchTerm (PageMsg.Internal << UpdateSearchTerm)
                    , Components.SearchDialog.onShow <| preventBackgroundScrolling ()
                    , Components.SearchDialog.onHide <| Extras.Task.messageToCommand <| PageMsg.Internal HideSearchDialog
                    ]
            }
      , errorWhileDeleting = Nothing
      }
    , case commonModel.maybeIndex of
        Just index ->
            scrollGlossaryItemIntoView index

        Nothing ->
            commonModel.fragment
                |> Maybe.map (Extras.BrowserDom.scrollElementIntoView <| PageMsg.Internal NoOp)
                |> Maybe.withDefault Cmd.none
    )



-- PORTS


port allowBackgroundScrolling : () -> Cmd msg


port preventBackgroundScrolling : () -> Cmd msg



-- UPDATE


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        MakeChanges ->
            ( { model | makingChanges = True }, Cmd.none )

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

        ExportDropdownMenuMsg msg_ ->
            Components.DropdownMenu.update
                (\x -> { model | exportDropdownMenu = x })
                (PageMsg.Internal << ExportDropdownMenuMsg)
                msg_
                model.exportDropdownMenu

        SearchDialogMsg msg_ ->
            Components.SearchDialog.update
                (\x ->
                    let
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
                searchDialog0 =
                    model.searchDialog
              in
              { model | searchDialog = { searchDialog0 | term = "", results = [] } }
            , allowBackgroundScrolling ()
            )

        UpdateSearchTerm searchTerm ->
            ( let
                searchDialog0 =
                    model.searchDialog

                results =
                    Search.search searchTerm <|
                        case model.common.glossary of
                            Ok (PlaintextGlossary { items }) ->
                                items

                            Ok MarkdownGlossary ->
                                GlossaryItems.fromList []

                            Err _ ->
                                GlossaryItems.fromList []
              in
              { model
                | searchDialog =
                    { searchDialog0
                        | term = searchTerm
                        , results = results
                    }
              }
            , Cmd.none
            )

        ConfirmDelete index ->
            ( { model | confirmDeleteIndex = Just index }, preventBackgroundScrolling () )

        CancelDelete ->
            if model.confirmDeleteIndex /= Nothing then
                ( { model | confirmDeleteIndex = Nothing }, allowBackgroundScrolling () )

            else
                ( model, Cmd.none )

        Delete index ->
            case model.common.glossary of
                Ok (PlaintextGlossary { items }) ->
                    let
                        updatedGlossaryItems =
                            GlossaryItems.remove index items
                    in
                    ( { model | confirmDeleteIndex = Nothing }
                    , Cmd.batch
                        [ patchHtmlFile model.common index updatedGlossaryItems
                        , allowBackgroundScrolling ()
                        ]
                    )

                Ok MarkdownGlossary ->
                    ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Deleted updatedGlossaryItems ->
            let
                common =
                    model.common
            in
            case common.glossary of
                Ok (PlaintextGlossary glossary) ->
                    ( { model | common = { common | glossary = Ok <| PlaintextGlossary { glossary | items = updatedGlossaryItems } } }
                    , Cmd.none
                    )

                Ok MarkdownGlossary ->
                    ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        FailedToDelete indexOfItemBeingDeleted error ->
            ( { model
                | errorWhileDeleting =
                    Just ( indexOfItemBeingDeleted, Extras.Http.errorToHumanReadable error )
              }
            , Cmd.none
            )

        JumpToTermIndexGroup staticSidebar termIndexGroupLabel ->
            let
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
                common =
                    model.common
            in
            ( { model | common = { common | orderItemsBy = orderItemsBy } }
            , Cmd.none
            )

        DownloadMarkdown ->
            ( { model | exportDropdownMenu = Components.DropdownMenu.hidden model.exportDropdownMenu }
            , case model.common.glossary of
                Ok (PlaintextGlossary { title, aboutSection, items }) ->
                    Export.Markdown.download title aboutSection items

                Ok MarkdownGlossary ->
                    Cmd.none

                _ ->
                    Cmd.none
            )

        DownloadAnki ->
            ( { model | exportDropdownMenu = Components.DropdownMenu.hidden model.exportDropdownMenu }
            , case model.common.glossary of
                Ok (PlaintextGlossary { title, aboutSection, items }) ->
                    Export.Anki.download title aboutSection items

                Ok MarkdownGlossary ->
                    Cmd.none

                _ ->
                    Cmd.none
            )


patchHtmlFile : CommonModel -> GlossaryItemIndex -> GlossaryItems -> Cmd Msg
patchHtmlFile common indexOfItemBeingDeleted glossaryItems =
    let
        msg =
            PageMsg.Internal <| Deleted glossaryItems
    in
    if common.enableSavingChangesInMemory then
        Extras.Task.messageToCommand msg

    else
        case common.glossary of
            Ok (PlaintextGlossary glossary0) ->
                let
                    glossary =
                        PlaintextGlossary { glossary0 | items = glossaryItems }
                in
                Http.request
                    { method = "PATCH"
                    , headers = []
                    , url = "/"
                    , body =
                        glossary
                            |> Glossary.toHtmlTree
                            |> HtmlTree.toHtml
                            |> Http.stringBody "text/html"
                    , expect =
                        Http.expectWhatever
                            (\result ->
                                case result of
                                    Ok _ ->
                                        msg

                                    Err error ->
                                        PageMsg.Internal <| FailedToDelete indexOfItemBeingDeleted error
                            )
                    , timeout = Nothing
                    , tracker = Nothing
                    }

            _ ->
                Cmd.none


scrollGlossaryItemIntoView : GlossaryItemIndex -> Cmd Msg
scrollGlossaryItemIntoView =
    ElementIds.glossaryItemDiv >> (Extras.BrowserDom.scrollElementIntoView <| PageMsg.Internal NoOp)



-- VIEW


viewMakingChangesHelp : Maybe String -> Bool -> Html Msg
viewMakingChangesHelp filename tabbable =
    div
        [ class "mb-5 rounded-md overflow-x-auto bg-amber-50 dark:bg-gray-700 text-gray-700 dark:text-gray-300 print:hidden"
        , class "pt-4 pr-4 pl-4 pb-2"
        ]
        [ details
            []
            [ summary
                [ class "mb-1 text-lg leading-6 items-center font-medium text-gray-900 dark:text-gray-100 select-none" ]
                [ span
                    [ class "ml-2" ]
                    [ text "How to Make Changes" ]
                ]
            , div
                [ class "mb-1" ]
                [ p
                    [ class "mt-3 max-w-xl" ]
                    [ text "This page includes a web interface for making changes that are saved back to the HTML file itself."
                    , text " This is meant to be used "
                    , span [ class "font-semibold" ] [ text "locally" ]
                    , text " by a "
                    , span [ class "font-semibold" ] [ text "single user" ]
                    , text " at a time and works best if the file is kept under version control."
                    ]
                , p []
                    [ text "If you're on macOS or Linux and have "
                    , a
                        [ href "https://nodejs.org/"
                        , Html.Attributes.target "_blank"
                        , Accessibility.Key.tabbable tabbable
                        ]
                        [ text "Node.js" ]
                    , text " installed, then just run"
                    ]
                , pre
                    [ class "mt-5" ]
                    [ code [] <|
                        let
                            defaultFilename =
                                "glossary.html"
                        in
                        [ text "sed -n '/START OF editor.js$/,$p' "
                        , text <| Maybe.withDefault defaultFilename filename
                        , if filename == Just defaultFilename then
                            text " | node"

                          else
                            text <| " | FILE=" ++ (filename |> Maybe.withDefault defaultFilename) ++ " node"
                        ]
                    ]
                , p
                    [ class "mt-3 max-w-xl" ]
                    [ text "You can hide these instructions altogether by setting the "
                    , Extras.Html.inlineCode "data-enable-help-for-making-changes"
                    , text " attribute to "
                    , Extras.Html.inlineCode "false"
                    , text " on the "
                    , code [] [ text <| "<div id=\"" ++ ElementIds.container ++ "\">" ]
                    , text " element."
                    ]
                ]
            ]
        ]


viewTermIndexItem : Bool -> GlossaryItem.Term -> Html Msg
viewTermIndexItem tabbable term =
    li []
        [ Html.a
            [ class "block border-l pl-4 -ml-px border-transparent hover:border-slate-400 dark:hover:border-slate-400 text-slate-700 hover:text-slate-900 dark:text-slate-400 dark:hover:text-slate-300"
            , Html.Attributes.href <| fragmentOnly term.id
            , Accessibility.Key.tabbable tabbable
            , Html.Events.onClick <| PageMsg.Internal StartHidingMenuForMobile
            ]
            [ text term.body ]
        ]


viewTermIndexGroup : Bool -> Bool -> TermIndexGroup -> Html Msg
viewTermIndexGroup tabbable staticSidebar { label, terms } =
    li
        [ id <| ElementIds.termIndexGroupLabel staticSidebar label
        , class "mt-6"
        ]
        [ h5
            [ class "mb-8 lg:mb-3 font-semibold text-slate-900 dark:text-slate-200" ]
            [ text label ]
        , ul
            [ class "space-y-6 lg:space-y-2 border-l border-slate-200 dark:border-slate-600" ]
            (List.map (viewTermIndexItem tabbable) terms)
        ]


type alias TermIndexGroup =
    { label : String
    , terms : List GlossaryItem.Term
    }


type alias TermIndex =
    List TermIndexGroup


termIndexFromGlossaryItems : GlossaryItems -> TermIndex
termIndexFromGlossaryItems glossaryItems =
    let
        termListsByFirstCharacter : Dict String (List GlossaryItem.Term)
        termListsByFirstCharacter =
            glossaryItems
                |> GlossaryItems.orderedAlphabetically
                |> List.concatMap (Tuple.second >> .terms)
                |> List.foldl
                    (\term result ->
                        let
                            firstCharacterUpper =
                                term.body |> String.toUpper |> String.left 1
                        in
                        Dict.update
                            firstCharacterUpper
                            (\termList ->
                                termList
                                    |> Maybe.map (\terms -> term :: terms)
                                    |> Maybe.withDefault [ term ]
                                    |> Just
                            )
                            result
                    )
                    Dict.empty

        alphabet : List String
        alphabet =
            List.range (Char.toCode 'A') (Char.toCode 'Z')
                |> List.map (Char.fromCode >> String.fromChar)

        termListsByFirstCharacterIncludingAlphabet : Dict String (List GlossaryItem.Term)
        termListsByFirstCharacterIncludingAlphabet =
            List.foldl
                (\letter result ->
                    Dict.update letter
                        (\maybeTermList ->
                            if maybeTermList == Nothing then
                                Just []

                            else
                                maybeTermList
                        )
                        result
                )
                termListsByFirstCharacter
                alphabet

        termIndex : TermIndex
        termIndex =
            termListsByFirstCharacterIncludingAlphabet
                |> Dict.toList
                |> List.map (Tuple.mapSecond <| List.sortBy <| .body >> String.toLower)
                |> List.map (\( label, terms ) -> TermIndexGroup label terms)
    in
    termIndex


viewTermsIndex : Bool -> Bool -> TermIndex -> Html Msg
viewTermsIndex tabbable staticSidebar termIndex =
    ul
        [ id <| ElementIds.termsIndex staticSidebar
        , class "mb-10"
        ]
        (List.filterMap
            (\termIndexGroup ->
                if List.isEmpty termIndexGroup.terms then
                    Nothing

                else
                    Just <| viewTermIndexGroup tabbable staticSidebar termIndexGroup
            )
            termIndex
        )


viewGlossaryTerm : Bool -> GlossaryItem.Term -> Html Msg
viewGlossaryTerm tabbable term =
    Html.dt
        [ class "group" ]
        [ Html.dfn
            [ Html.Attributes.id term.id ]
            [ if term.isAbbreviation then
                Html.abbr [] [ text term.body ]

              else
                text term.body
            ]
        , span
            [ class "silcrow invisible group-hover:visible hover:visible" ]
            [ Html.a
                [ fragmentOnly term.id |> Html.Attributes.href
                , Accessibility.Key.tabbable tabbable
                ]
                [ text "§" ]
            ]
        ]


viewGlossaryItemDetails : String -> Html Msg
viewGlossaryItemDetails details =
    Html.dd
        []
        [ text details ]


viewGlossaryItemRelatedTerms : Bool -> Bool -> List GlossaryItem.RelatedTerm -> List (Html Msg)
viewGlossaryItemRelatedTerms tabbable itemHasSomeDetails relatedTerms =
    if List.isEmpty relatedTerms then
        []

    else
        [ Html.dd
            [ class "related-terms" ]
            (text
                (if itemHasSomeDetails then
                    "See also: "

                 else
                    "See: "
                )
                :: (relatedTerms
                        |> List.map
                            (\relatedTerm ->
                                Html.a
                                    [ fragmentOnly relatedTerm.idReference |> Html.Attributes.href
                                    , Accessibility.Key.tabbable tabbable
                                    ]
                                    [ text relatedTerm.body ]
                            )
                        |> List.intersperse (text ", ")
                   )
            )
        ]


viewGlossaryItem : GlossaryItemIndex -> Bool -> Model -> Bool -> Maybe ( GlossaryItemIndex, String ) -> GlossaryItem -> Html Msg
viewGlossaryItem index tabbable model editable errorWhileDeleting glossaryItem =
    let
        errorDiv message =
            div
                [ class "flex justify-end mt-2" ]
                [ p
                    [ class "text-red-600" ]
                    [ text message ]
                ]

        itemHasSomeDetails =
            GlossaryItem.hasSomeDetails glossaryItem

        common =
            model.common
    in
    if editable then
        div
            [ class "flex flex-col justify-items-end"
            , id <| ElementIds.glossaryItemDiv index
            ]
            [ div
                []
                (List.map (viewGlossaryTerm tabbable) glossaryItem.terms
                    ++ List.map viewGlossaryItemDetails glossaryItem.details
                    ++ viewGlossaryItemRelatedTerms tabbable itemHasSomeDetails glossaryItem.relatedTerms
                )
            , div
                [ class "print:hidden mt-3 flex flex-col flex-grow justify-end" ]
                [ div
                    [ class "flex justify-between" ]
                    [ span
                        [ class "inline-flex items-center" ]
                        [ Components.Button.text
                            [ Html.Events.onClick <| PageMsg.NavigateToCreateOrEdit { common | maybeIndex = Just index }
                            , Accessibility.Key.tabbable tabbable
                            ]
                            [ Icons.pencil
                                [ Svg.Attributes.class "h-5 w-5" ]
                            , span
                                [ class "font-medium text-gray-600 dark:text-gray-300 hover:text-gray-700 dark:hover:text-gray-400" ]
                                [ text "Edit" ]
                            ]
                        ]
                    , span
                        [ class "ml-3 inline-flex items-center" ]
                        [ Components.Button.text
                            [ Html.Events.onClick <| PageMsg.Internal <| ConfirmDelete index
                            , Accessibility.Key.tabbable tabbable
                            ]
                            [ Icons.trash
                                [ Svg.Attributes.class "h-5 w-5" ]
                            , span
                                [ class "font-medium text-gray-600 dark:text-gray-300 hover:text-gray-700 dark:hover:text-gray-400" ]
                                [ text "Delete" ]
                            ]
                        ]
                    ]
                , errorWhileDeleting
                    |> Extras.Html.showMaybe
                        (\( indexOfItemBeingDeleted, errorMessage ) ->
                            if index == indexOfItemBeingDeleted then
                                errorDiv <| "Failed to save — " ++ errorMessage ++ "."

                            else
                                Extras.Html.nothing
                        )
                ]
            ]

    else
        div []
            (List.map (viewGlossaryTerm tabbable) glossaryItem.terms
                ++ List.map viewGlossaryItemDetails glossaryItem.details
                ++ viewGlossaryItemRelatedTerms tabbable itemHasSomeDetails glossaryItem.relatedTerms
            )


viewConfirmDeleteModal : Bool -> Maybe GlossaryItemIndex -> Html Msg
viewConfirmDeleteModal enableSavingChangesInMemory maybeIndexOfItemToDelete =
    Html.div
        [ class "fixed z-10 inset-0 overflow-y-auto print:hidden"
        , Extras.HtmlAttribute.showIf (maybeIndexOfItemToDelete == Nothing) <| class "invisible"
        , Extras.HtmlEvents.onEscape <| PageMsg.Internal CancelDelete
        , Accessibility.Aria.labelledBy ElementIds.confirmDeleteModalTitle
        , Accessibility.Role.dialog
        , Accessibility.Aria.modal True
        ]
        [ div
            [ class "flex items-end justify-center min-h-screen pt-4 px-4 pb-20 text-center sm:block sm:p-0" ]
            [ Html.div
                [ class "fixed inset-0 bg-gray-500 dark:bg-gray-800 bg-opacity-75 dark:bg-opacity-75 transition-opacity"
                , if maybeIndexOfItemToDelete == Nothing then
                    class "ease-in duration-200 opacity-0"

                  else
                    class "ease-out duration-300 opacity-100"
                , Accessibility.Aria.hidden True
                , Html.Events.onClick <| PageMsg.Internal CancelDelete
                ]
                []
            , span
                [ class "hidden sm:inline-block sm:align-middle sm:h-screen"
                , Accessibility.Aria.hidden True
                ]
                [ text "\u{200B}" ]
            , div
                [ class "inline-block align-bottom bg-white dark:bg-gray-700 rounded-lg px-4 pt-5 pb-4 text-left overflow-hidden shadow-xl transform transition-all sm:my-8 sm:align-middle sm:max-w-lg sm:w-full sm:p-6"
                , if maybeIndexOfItemToDelete == Nothing then
                    class "ease-in duration-200 opacity-0 translate-y-4 sm:translate-y-0 sm:scale-95"

                  else
                    class "ease-out duration-300 opacity-100 translate-y-0 sm:scale-100"
                ]
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
                            [ text "Delete item"
                            ]
                        , div
                            [ class "mt-2" ]
                            [ p
                                [ class "text-sm text-gray-500 dark:text-gray-400" ]
                                [ text "Are you sure you want to delete this item?" ]
                            ]
                        ]
                    ]
                , Extras.Html.showIf enableSavingChangesInMemory <|
                    div
                        [ class "mt-5 sm:mt-4 text-sm text-gray-500 dark:text-gray-400 sm:text-right" ]
                        [ text Components.Copy.sandboxModeMessage ]
                , div
                    [ class "mt-5 sm:mt-4 sm:flex sm:flex-row-reverse" ]
                    [ Components.Button.primary True
                        [ class "w-full bg-red-600 dark:bg-red-400 hover:bg-red-700 focus:ring-red-500 sm:ml-3 sm:w-auto sm:text-sm dark:text-gray-800"
                        , Extras.HtmlAttribute.showMaybe
                            (Html.Events.onClick << PageMsg.Internal << Delete)
                            maybeIndexOfItemToDelete
                        ]
                        [ text "Delete" ]
                    , Components.Button.white True
                        [ class "mt-3 w-full sm:mt-0 sm:w-auto sm:text-sm"
                        , Html.Events.onClick <| PageMsg.Internal CancelDelete
                        , Extras.HtmlEvents.onEnter <| PageMsg.Internal CancelDelete
                        ]
                        [ text "Cancel" ]
                    ]
                ]
            ]
        ]


viewMakeChangesButton : Bool -> Html Msg
viewMakeChangesButton tabbable =
    div
        [ class "pb-6 print:hidden" ]
        [ Components.Button.white True
            [ Html.Events.onClick <| PageMsg.Internal MakeChanges
            , Accessibility.Key.tabbable tabbable
            ]
            [ Icons.pencil
                [ Svg.Attributes.class "h-5 w-5" ]
            , span
                [ class "ml-2" ]
                [ text "Make changes" ]
            ]
        ]


viewEditTitleAndAboutButton : Bool -> CommonModel -> Html Msg
viewEditTitleAndAboutButton tabbable common =
    div
        [ class "pb-6 print:hidden" ]
        [ Components.Button.white True
            [ Html.Events.onClick <| PageMsg.NavigateToEditTitleAndAbout { common | maybeIndex = Nothing }
            , Accessibility.Key.tabbable tabbable
            ]
            [ Icons.pencil
                [ Svg.Attributes.class "h-5 w-5" ]
            , span
                [ class "ml-2" ]
                [ text "Edit title and about section" ]
            ]
        ]


viewCreateGlossaryItemButtonForEmptyState : Bool -> CommonModel -> Html Msg
viewCreateGlossaryItemButtonForEmptyState tabbable common =
    div
        [ class "pt-4 print:hidden" ]
        [ Components.Button.emptyState
            [ class "p-9"
            , Html.Events.onClick <| PageMsg.NavigateToCreateOrEdit { common | maybeIndex = Nothing }
            , Accessibility.Key.tabbable tabbable
            ]
            [ Icons.viewGridAdd
                [ Svg.Attributes.class "mx-auto h-12 w-12 text-gray-400" ]
            , span
                [ class "mt-2 block font-medium text-gray-900 dark:text-gray-200" ]
                [ text "Create a new glossary item" ]
            ]
        ]


viewCreateGlossaryItemButton : Bool -> CommonModel -> Html Msg
viewCreateGlossaryItemButton tabbable common =
    div
        [ class "pb-2 print:hidden" ]
        [ Components.Button.secondary
            [ Html.Events.onClick <| PageMsg.NavigateToCreateOrEdit { common | maybeIndex = Nothing }
            , Accessibility.Key.tabbable tabbable
            ]
            [ Icons.viewGridAdd
                [ Svg.Attributes.class "mx-auto -ml-1 mr-2 h-5 w-5"
                , fill "currentColor"
                , stroke "none"
                ]
            , text "Create a new glossary item"
            ]
        ]


viewCards : Model -> Bool -> Bool -> List ( GlossaryItemIndex, GlossaryItem ) -> Html Msg
viewCards model editable tabbable indexedGlossaryItems =
    Html.article
        [ Html.Attributes.id ElementIds.items ]
        [ div
            [ class "pt-2 border-t border-gray-300 dark:border-gray-700" ]
            [ Extras.Html.showIf editable <|
                div
                    [ class "pt-2" ]
                    [ if List.isEmpty indexedGlossaryItems then
                        viewCreateGlossaryItemButtonForEmptyState tabbable model.common

                      else
                        viewCreateGlossaryItemButton tabbable model.common
                    ]
            ]
        , Extras.Html.showIf (not <| List.isEmpty indexedGlossaryItems) <|
            viewOrderItemsBy model
        , Html.dl
            []
            (indexedGlossaryItems
                |> List.map
                    (\( index, glossaryItem ) ->
                        viewGlossaryItem
                            index
                            tabbable
                            model
                            editable
                            model.errorWhileDeleting
                            glossaryItem
                    )
            )
        , Components.SearchDialog.view
            (PageMsg.Internal << SearchDialogMsg)
            model.searchDialog.model
            model.searchDialog.term
            model.searchDialog.results
        , viewConfirmDeleteModal
            model.common.enableSavingChangesInMemory
            model.confirmDeleteIndex
        ]


viewMenuForMobile : Model -> Bool -> TermIndex -> Html Msg
viewMenuForMobile model tabbable termIndex =
    div
        [ class "invisible" |> Extras.HtmlAttribute.showIf (model.menuForMobileVisibility == Invisible)
        , class "fixed inset-0 flex z-40 lg:hidden"
        , Accessibility.Role.dialog
        , Accessibility.Aria.modal True
        ]
        [ Html.div
            [ class "fixed inset-0 bg-gray-600 bg-opacity-75"
            , if model.menuForMobileVisibility == Visible then
                class "transition-opacity ease-linear duration-300 opacity-100"

              else
                class "transition-opacity ease-linear duration-300 opacity-0"
            , Html.Events.onClick <| PageMsg.Internal StartHidingMenuForMobile
            , Accessibility.Aria.hidden True
            ]
            []
        , div
            [ class "relative flex-1 flex flex-col max-w-xs w-full pt-5 bg-white dark:bg-gray-900"
            , if model.menuForMobileVisibility == Visible then
                class "transition ease-in-out duration-300 transform translate-x-0"

              else
                class "transition ease-in-out duration-300 transform -translate-x-full"
            ]
            [ div
                [ class "absolute top-0 right-0 -mr-12 pt-2"
                , if model.menuForMobileVisibility == Visible then
                    class "ease-in-out duration-300 opacity-100"

                  else
                    class "ease-in-out duration-300 opacity-0"
                ]
                [ button
                    [ Html.Attributes.type_ "button"
                    , class "ml-1 flex items-center justify-center h-10 w-10 rounded-full focus:outline-none focus:ring-2 focus:ring-inset focus:ring-white dark:focus:ring-gray-500"
                    , Html.Events.onClick <| PageMsg.Internal StartHidingMenuForMobile
                    ]
                    [ span
                        [ class "sr-only" ]
                        [ text "Close sidebar"
                        ]
                    , Icons.x
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
                    , viewTermsIndex tabbable False termIndex
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
                [ text "Back to top"
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
                , class "hidden w-full lg:flex items-center text-sm leading-6 text-slate-400 rounded-md ring-1 ring-slate-900/10 shadow-sm py-1.5 pl-2 pr-3 hover:ring-slate-400 dark:hover:ring-slate-600 dark:bg-slate-800 dark:highlight-white/5 dark:hover:bg-slate-800"
                , Html.Events.onClick <| PageMsg.Internal <| SearchDialogMsg Components.SearchDialog.show
                , Accessibility.Aria.hidden True
                , Accessibility.Key.tabbable tabbable
                ]
                [ Icons.search
                    [ width "24"
                    , height "24"
                    , Svg.Attributes.class "mr-3 flex-none"
                    ]
                , text "Quick search..."
                , span
                    [ class "ml-auto pl-3 flex-none text-xs font-semibold" ]
                    [ text "Ctrl K"
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


viewTermIndexFirstCharacterGrid : Bool -> Bool -> TermIndex -> Html Msg
viewTermIndexFirstCharacterGrid staticSidebar tabbable termIndex =
    div
        [ class "bg-white dark:bg-slate-900 select-none pointer-events-auto" ]
        (List.map
            (\termIndexGroup ->
                viewTermIndexFirstCharacter
                    staticSidebar
                    tabbable
                    termIndexGroup.label
                    (not <| List.isEmpty termIndexGroup.terms)
            )
            termIndex
        )


viewQuickSearchButtonAndLetterGrid : Bool -> Bool -> TermIndex -> Html Msg
viewQuickSearchButtonAndLetterGrid staticSidebar tabbable termIndex =
    div
        [ id ElementIds.quickSearchButtonAndLetterGrid
        , class "-mb-6 sticky top-0 -ml-0.5 pointer-events-none"
        ]
        [ div
            [ class "h-7 bg-white dark:bg-slate-900" ]
            []
        , div
            [ class "pr-4" ]
            [ viewBackToTopLink True tabbable ]
        , viewQuickSearchButton tabbable
        , div
            [ class "px-3 bg-white dark:bg-slate-900" ]
            [ viewTermIndexFirstCharacterGrid staticSidebar tabbable termIndex ]
        , div
            [ class "h-8 bg-gradient-to-b from-white dark:from-slate-900" ]
            []
        ]


viewStaticSidebarForDesktop : Bool -> TermIndex -> Html Msg
viewStaticSidebarForDesktop tabbable termIndex =
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
                [ viewTermsIndex tabbable True termIndex ]
            ]
        ]


viewTopBar : Bool -> Components.DropdownMenu.Model -> Html Msg
viewTopBar tabbable exportDropdownMenu =
    div
        [ class "sticky top-0 z-10 shrink-0 flex justify-between h-16 bg-white dark:bg-gray-900 border-b border-gray-200 dark:border-gray-800 lg:hidden print:hidden items-center" ]
        [ div
            [ class "flex-1" ]
            [ button
                [ Html.Attributes.type_ "button"
                , class "px-4 border-r border-gray-200 dark:border-gray-700 text-gray-500 focus:outline-none lg:hidden"
                , Html.Events.onClick <| PageMsg.Internal ShowMenuForMobile
                ]
                [ span
                    [ class "sr-only" ]
                    [ text "Open sidebar" ]
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
                    [ text "Search" ]
                , Icons.search
                    [ width "24"
                    , height "24"
                    , Accessibility.Aria.hidden True
                    ]
                ]
            ]
        , div
            [ class "flex pr-4" ]
            [ viewExportButton tabbable exportDropdownMenu ]
        ]


viewExportButton : Bool -> Components.DropdownMenu.Model -> Html Msg
viewExportButton enabled exportDropdownMenu =
    Components.DropdownMenu.view
        (PageMsg.Internal << ExportDropdownMenuMsg)
        exportDropdownMenu
        enabled
        [ Icons.documentDownload
            [ Svg.Attributes.class "h-5 w-5 mr-2" ]
        , text "Export"
        ]
        [ Components.DropdownMenu.choice
            [ text "Anki deck" ]
            (PageMsg.Internal <| DownloadAnki)
        , Components.DropdownMenu.choice
            [ text "Markdown" ]
            (PageMsg.Internal <| DownloadMarkdown)
        ]


viewOrderItemsBy : Model -> Html Msg
viewOrderItemsBy model =
    let
        tabbable =
            noModalDialogShown model
    in
    div
        [ class "print:hidden pt-4 pb-6" ]
        [ label
            [ class "font-medium text-gray-900 dark:text-gray-100" ]
            [ text "Order items" ]
        , fieldset [ class "mt-4" ]
            [ legend
                [ class "sr-only" ]
                [ text "Sort order" ]
            , div
                [ class "space-y-4 sm:flex sm:items-center sm:space-y-0 sm:space-x-6" ]
                [ div
                    [ class "flex items-center" ]
                    [ Components.Button.radio
                        "order-items-by"
                        "order-items-alphabetically"
                        (model.common.orderItemsBy == CommonModel.Alphabetically)
                        tabbable
                        [ id ElementIds.orderItemsAlphabetically
                        , Html.Events.onClick <| PageMsg.Internal <| ChangeOrderItemsBy CommonModel.Alphabetically
                        ]
                    , label
                        [ class "ml-3 block font-medium text-gray-700 dark:text-gray-300 select-none"
                        , for ElementIds.orderItemsAlphabetically
                        ]
                        [ text "alphabetically" ]
                    ]
                , div
                    [ class "flex items-center" ]
                    [ Components.Button.radio
                        "order-items-by"
                        "order-items-most-frequent-first"
                        (model.common.orderItemsBy == CommonModel.MostFrequentFirst)
                        tabbable
                        [ id ElementIds.orderItemsMostFrequentFirst
                        , Html.Events.onClick <| PageMsg.Internal <| ChangeOrderItemsBy CommonModel.MostFrequentFirst
                        ]
                    , label
                        [ class "ml-3 block font-medium text-gray-700 dark:text-gray-300 select-none"
                        , for ElementIds.orderItemsMostFrequentFirst
                        ]
                        [ text "most frequent first" ]
                    ]
                ]
            ]
        ]


noModalDialogShown : Model -> Bool
noModalDialogShown model =
    model.confirmDeleteIndex == Nothing && not (Components.SearchDialog.visible model.searchDialog.model)


view : Model -> Document Msg
view model =
    case model.common.glossary of
        Err error ->
            { title = "Glossary"
            , body = [ pre [] [ text <| Decode.errorToString error ] ]
            }

        Ok (PlaintextGlossary { title, aboutSection, items }) ->
            let
                editable =
                    model.makingChanges

                noModalDialogShown_ =
                    noModalDialogShown model

                termIndex =
                    termIndexFromGlossaryItems items

                ( aboutParagraph, aboutLinks ) =
                    case aboutSection of
                        PlaintextAboutSection { paragraph, links } ->
                            ( paragraph, links )
            in
            { title = GlossaryTitle.toString title
            , body =
                [ Html.div
                    [ class "min-h-full focus:outline-none"
                    , Html.Attributes.id ElementIds.outer
                    , Accessibility.Key.tabbable True
                    , Html.Events.preventDefaultOn "keydown"
                        (Extras.HtmlEvents.preventDefaultOnDecoder
                            (\event ->
                                case ( model.confirmDeleteIndex, Components.SearchDialog.visible model.searchDialog.model ) of
                                    ( Just index, _ ) ->
                                        if event == Extras.HtmlEvents.escape then
                                            Just <| ( PageMsg.Internal CancelDelete, True )

                                        else if event == Extras.HtmlEvents.enter then
                                            Just <| ( PageMsg.Internal <| Delete index, True )

                                        else
                                            Nothing

                                    ( _, True ) ->
                                        if event == Extras.HtmlEvents.escape || event == Extras.HtmlEvents.controlK then
                                            Just <| ( PageMsg.Internal <| SearchDialogMsg Components.SearchDialog.hide, True )

                                        else
                                            Nothing

                                    ( _, _ ) ->
                                        if event == Extras.HtmlEvents.controlK then
                                            Just <| ( PageMsg.Internal <| SearchDialogMsg Components.SearchDialog.show, True )

                                        else if model.makingChanges && event == Extras.HtmlEvents.n then
                                            let
                                                common_ =
                                                    model.common
                                            in
                                            Just <| ( PageMsg.NavigateToCreateOrEdit { common_ | maybeIndex = Nothing }, True )

                                        else
                                            Nothing
                            )
                        )
                    ]
                    [ viewMenuForMobile model noModalDialogShown_ termIndex
                    , viewStaticSidebarForDesktop noModalDialogShown_ termIndex
                    , div
                        [ class "lg:pl-64 flex flex-col" ]
                        [ viewTopBar noModalDialogShown_ model.exportDropdownMenu
                        , div
                            [ Html.Attributes.id ElementIds.container
                            , Html.Attributes.attribute "data-enable-markdown-based-syntax" "false"
                            ]
                            [ header []
                                [ div
                                    [ class "lg:border-b border-gray-300 dark:border-gray-700 lg:mb-4" ]
                                    [ div
                                        [ class "flex flex-row justify-start lg:justify-end" ]
                                        [ Extras.Html.showIf (model.common.enableSavingChangesInMemory && not model.makingChanges) <|
                                            div
                                                [ class "flex-none" ]
                                                [ viewMakeChangesButton noModalDialogShown_ ]
                                        , Extras.Html.showIf editable <|
                                            div
                                                [ class "flex-none" ]
                                                [ viewEditTitleAndAboutButton noModalDialogShown_ model.common ]
                                        , div
                                            [ class "hidden lg:block ml-auto pb-3" ]
                                            [ viewExportButton noModalDialogShown_ model.exportDropdownMenu ]
                                        ]
                                    , Extras.Html.showIf (model.common.enableSavingChangesInMemory && model.makingChanges) <|
                                        div
                                            [ class "mb-5 sm:mb-4 text-sm text-gray-500 dark:text-gray-400" ]
                                            [ text Components.Copy.sandboxModeMessage ]
                                    ]
                                , viewMakingChangesHelp model.common.filename noModalDialogShown_
                                    |> Extras.Html.showIf (not model.common.enableSavingChangesInMemory)
                                , h1
                                    [ id ElementIds.title ]
                                    [ text <| GlossaryTitle.toString title ]
                                ]
                            , Html.main_
                                []
                                [ div
                                    [ id ElementIds.about ]
                                    [ p []
                                        [ text <| AboutParagraph.toString aboutParagraph ]
                                    , ul [] <|
                                        List.map
                                            (\aboutLink ->
                                                li []
                                                    [ a
                                                        [ target "_blank"
                                                        , href <| AboutLink.href aboutLink
                                                        , Accessibility.Key.tabbable noModalDialogShown_
                                                        ]
                                                        [ text <| AboutLink.body aboutLink ]
                                                    ]
                                            )
                                            aboutLinks
                                    ]
                                , items
                                    |> (if model.common.orderItemsBy == CommonModel.Alphabetically then
                                            GlossaryItems.orderedAlphabetically

                                        else
                                            GlossaryItems.orderedByFrequency
                                       )
                                    |> viewCards model editable noModalDialogShown_
                                ]
                            ]
                        ]
                    ]
                ]
            }

        Ok MarkdownGlossary ->
            { title = "Not supported yet." -- TODO
            , body = [ Html.text "Not supported yet." ]
            }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Components.DropdownMenu.subscriptions model.exportDropdownMenu
            |> Sub.map (ExportDropdownMenuMsg >> PageMsg.Internal)
        ]
