port module Pages.ListAll exposing (Model, Msg, init, subscriptions, update, view)

import Accessibility exposing (..)
import Accessibility.Aria
import Accessibility.Key exposing (tabbable)
import Accessibility.Role
import Browser exposing (Document)
import Browser.Dom as Dom
import Browser.Events as Events
import CommonModel exposing (CommonModel)
import Components.Button
import Components.DropdownMenu
import Data.AboutLink as AboutLink
import Data.GlossaryItem as GlossaryItem exposing (GlossaryItem)
import Data.GlossaryItemIndex exposing (GlossaryItemIndex)
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Data.GlossaryTitle as GlossaryTitle
import Dict exposing (Dict)
import ElementIds
import Export.Markdown
import Extras.Html
import Extras.HtmlAttribute
import Extras.HtmlEvents
import Extras.HtmlTree as HtmlTree exposing (HtmlTree(..))
import Extras.Http
import Html
import Html.Attributes exposing (class, for, href, id, target)
import Html.Events
import Http
import Icons
import Json.Decode as Decode
import PageMsg exposing (PageMsg)
import Process
import Svg.Attributes exposing (fill, height, stroke, width)
import Task



-- MODEL


type GradualVisibility
    = Visible
    | Disappearing
    | Invisible


type alias MenuForMobileVisibility =
    GradualVisibility


type MakingChanges
    = NoHelpForMakingChanges
    | ReadyForMakingChanges
    | MakingChangesHelpCollapsed
    | MakingChangesHelpExpanded


type alias Model =
    { common : CommonModel
    , makingChanges : MakingChanges
    , menuForMobileVisibility : MenuForMobileVisibility
    , exportDropdownMenu : Components.DropdownMenu.Model
    , confirmDeleteIndex : Maybe GlossaryItemIndex
    , errorWhileDeleting : Maybe ( GlossaryItemIndex, String )
    }


type InternalMsg
    = NoOp
    | ToggleMakingChangesHelp
    | ShowMenuForMobile
    | StartHidingMenuForMobile
    | CompleteHidingMenuForMobile
    | ExportDropdownMenuMsg Components.DropdownMenu.Msg
    | ConfirmDelete GlossaryItemIndex
    | CancelDelete
    | Delete GlossaryItemIndex
    | Deleted GlossaryItems
    | FailedToDelete GlossaryItemIndex Http.Error
    | JumpToTermIndexGroup Bool String
    | ChangeOrderItemsBy CommonModel.OrderItemsBy
    | DownloadMarkdown GlossaryItems


type alias Msg =
    PageMsg InternalMsg


init : Bool -> CommonModel -> ( Model, Cmd Msg )
init editorIsRunning commonModel =
    ( { makingChanges =
            case ( editorIsRunning, commonModel.enableHelpForMakingChanges ) of
                ( True, _ ) ->
                    ReadyForMakingChanges

                ( False, True ) ->
                    MakingChangesHelpCollapsed

                ( False, False ) ->
                    NoHelpForMakingChanges
      , common = commonModel
      , menuForMobileVisibility = Invisible
      , exportDropdownMenu =
            Components.DropdownMenu.init
                [ Components.DropdownMenu.id ElementIds.exportDropdownButton ]
      , confirmDeleteIndex = Nothing
      , errorWhileDeleting = Nothing
      }
    , case commonModel.maybeIndex of
        Just index ->
            scrollGlossaryItemIntoView index

        Nothing ->
            scrollToTop
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

        ToggleMakingChangesHelp ->
            let
                makingChangesToggled =
                    case model.makingChanges of
                        MakingChangesHelpExpanded ->
                            MakingChangesHelpCollapsed

                        MakingChangesHelpCollapsed ->
                            MakingChangesHelpExpanded

                        _ ->
                            model.makingChanges
            in
            ( { model | makingChanges = makingChangesToggled }, Cmd.none )

        ShowMenuForMobile ->
            ( { model | menuForMobileVisibility = Visible }
            , Cmd.batch
                [ preventBackgroundScrolling ()
                , scrollToTopInElement ElementIds.indexForMobile
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

        ExportDropdownMenuMsg msg_ ->
            Components.DropdownMenu.update
                (\x -> { model | exportDropdownMenu = x })
                (PageMsg.Internal << ExportDropdownMenuMsg)
                msg_
                model.exportDropdownMenu

        ConfirmDelete index ->
            ( { model | confirmDeleteIndex = Just index }, preventBackgroundScrolling () )

        CancelDelete ->
            if model.confirmDeleteIndex /= Nothing then
                ( { model | confirmDeleteIndex = Nothing }, allowBackgroundScrolling () )

            else
                ( model, Cmd.none )

        Delete index ->
            case model.common.loadedGlossaryItems of
                Ok glossaryItems ->
                    let
                        updatedGlossaryItems =
                            GlossaryItems.remove index glossaryItems
                    in
                    ( { model
                        | confirmDeleteIndex = Nothing
                        , errorWhileDeleting = Nothing
                      }
                    , Cmd.batch
                        [ patchHtmlFile model.common index updatedGlossaryItems
                        , allowBackgroundScrolling ()
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        Deleted updatedGlossaryItems ->
            let
                common =
                    model.common
            in
            ( { model | common = { common | loadedGlossaryItems = Ok updatedGlossaryItems } }
            , Cmd.none
            )

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

        DownloadMarkdown glossaryItems ->
            ( { model | exportDropdownMenu = Components.DropdownMenu.hidden model.exportDropdownMenu }
            , Cmd.batch
                [ Export.Markdown.download
                    model.common.title
                    model.common.aboutParagraph
                    model.common.aboutLinks
                    glossaryItems
                ]
            )


patchHtmlFile : CommonModel -> GlossaryItemIndex -> GlossaryItems -> Cmd Msg
patchHtmlFile common indexOfItemBeingDeleted glossaryItems =
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
                            PageMsg.Internal <| Deleted glossaryItems

                        Err error ->
                            PageMsg.Internal <| FailedToDelete indexOfItemBeingDeleted error
                )
        , timeout = Nothing
        , tracker = Nothing
        }


scrollToTop : Cmd Msg
scrollToTop =
    Dom.setViewport 0 0
        |> Task.onError (always <| Task.succeed ())
        |> Task.attempt (always <| PageMsg.Internal NoOp)


scrollGlossaryItemIntoView : GlossaryItemIndex -> Cmd Msg
scrollGlossaryItemIntoView =
    ElementIds.glossaryItemDiv >> scrollElementIntoView


scrollElementIntoView : String -> Cmd Msg
scrollElementIntoView id =
    id
        |> Dom.getElement
        |> Task.andThen (\element -> Dom.setViewport 0 <| element.element.y - 96)
        |> Task.onError (always <| Task.succeed ())
        |> Task.attempt (always <| PageMsg.Internal NoOp)


scrollToTopInElement : String -> Cmd Msg
scrollToTopInElement id =
    id
        |> Dom.getViewportOf
        |> Task.andThen (always <| Dom.setViewportOf id 0 0)
        |> Task.attempt (always <| PageMsg.Internal NoOp)



-- VIEW


viewMakingChangesHelp : String -> Bool -> Bool -> Html Msg
viewMakingChangesHelp filename tabbable expanded =
    div
        [ class "mb-5 rounded-md overflow-x-auto bg-amber-50 dark:bg-gray-700 text-gray-700 dark:text-gray-300 print:hidden"
        , class <|
            if expanded then
                "p-4"

            else
                "pt-4 pr-4 pl-4 pb-2"
        ]
        [ Html.h3
            [ class "inline-flex text-lg leading-6 items-center font-medium text-gray-900 dark:text-gray-100 select-none"
            , Html.Events.onClick <| PageMsg.Internal ToggleMakingChangesHelp
            ]
            [ span
                [ class "text-gray-500 dark:text-gray-300" ]
                [ if expanded then
                    Icons.chevronDown
                        [ Svg.Attributes.class "h-5 w-5 mb-0.5" ]

                  else
                    Icons.chevronRight
                        [ Svg.Attributes.class "h-5 w-5 mb-0.5" ]
                ]
            , span
                [ class "ml-2" ]
                [ text "How to Make Changes" ]
            ]
        , Extras.Html.showIf expanded <|
            div []
                [ p
                    [ class "mt-3 max-w-xl" ]
                    [ text "This page includes a web interface for making changes that are saved back to the HTML file itself. If you're on macOS or Linux and have "
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
                    [ code []
                        [ text "sed -n '/START OF editor.js$/,$p' "
                        , text filename
                        , if filename == "glossary.html" then
                            text " | node"

                          else
                            text <| " | FILE=" ++ filename ++ " node"
                        ]
                    ]
                , p
                    [ class "mt-5 max-w-xl" ]
                    [ text "This works best if the file is under version control." ]
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


viewTermIndexItem : Bool -> GlossaryItem.Term -> Html Msg
viewTermIndexItem tabbable term =
    li []
        [ Html.a
            [ class "block border-l pl-4 -ml-px border-transparent hover:border-slate-400 dark:hover:border-slate-400 text-slate-700 hover:text-slate-900 dark:text-slate-400 dark:hover:text-slate-300"
            , Html.Attributes.href <| "#" ++ term.id
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


viewGlossaryTerm : GlossaryItem.Term -> Html Msg
viewGlossaryTerm term =
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
            [ class "silcrow invisible" ]
            [ Html.a
                [ "#" ++ term.id |> Html.Attributes.href
                , Accessibility.Key.tabbable False
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
                                    [ "#" ++ relatedTerm.idReference |> Html.Attributes.href
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
                (List.map viewGlossaryTerm glossaryItem.terms
                    ++ List.map viewGlossaryItemDetails glossaryItem.details
                    ++ viewGlossaryItemRelatedTerms tabbable itemHasSomeDetails glossaryItem.relatedTerms
                )
            , div
                [ class "print:hidden mt-3 flex flex-col flex-grow justify-end z-10" ]
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
            (List.map viewGlossaryTerm glossaryItem.terms
                ++ List.map viewGlossaryItemDetails glossaryItem.details
                ++ viewGlossaryItemRelatedTerms tabbable itemHasSomeDetails glossaryItem.relatedTerms
            )


viewConfirmDeleteModal : Maybe GlossaryItemIndex -> Html Msg
viewConfirmDeleteModal maybeIndexOfItemToDelete =
    Html.div
        [ class "fixed z-10 inset-0 overflow-y-auto print:hidden"
        , Extras.HtmlAttribute.showIf (maybeIndexOfItemToDelete == Nothing) <| class "invisible"
        , Extras.HtmlEvents.onEscape <| PageMsg.Internal CancelDelete
        , Accessibility.Aria.labelledBy ElementIds.modalTitle
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
                            , id ElementIds.modalTitle
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
                , div
                    [ class "mt-5 sm:mt-4 sm:flex sm:flex-row-reverse" ]
                    [ Components.Button.primary
                        [ class "w-full bg-red-600 dark:bg-red-400 hover:bg-red-700 focus:ring-red-500 sm:ml-3 sm:w-auto sm:text-sm dark:text-gray-800 "
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
        , viewConfirmDeleteModal model.confirmDeleteIndex
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
                    [ viewTermIndexFirstCharacterGrid False termIndex
                    , viewTermsIndex tabbable False termIndex
                    ]
                ]
            ]
        , div
            [ class "shrink-0 w-14", Accessibility.Aria.hidden True ]
            []
        ]


viewQuickSearchButton : Html Msg
viewQuickSearchButton =
    div
        [ class "hidden px-3 pb-4 bg-white dark:bg-slate-900" ]
        [ div
            [ class "bg-gray-50 dark:bg-slate-900 relative pointer-events-auto" ]
            [ button
                [ Html.Attributes.type_ "button"
                , class "hidden w-full lg:flex items-center text-sm leading-6 text-slate-400 rounded-md ring-1 ring-slate-900/10 shadow-sm py-1.5 pl-2 pr-3 hover:ring-slate-400 dark:hover:ring-slate-600 dark:bg-slate-800 dark:highlight-white/5 dark:hover:bg-slate-800"
                , Accessibility.Aria.hidden True
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


viewTermIndexFirstCharacterGrid : Bool -> TermIndex -> Html Msg
viewTermIndexFirstCharacterGrid staticSidebar termIndex =
    div
        [ class "bg-white dark:bg-slate-900 select-none pointer-events-auto" ]
        (List.map
            (\termIndexGroup ->
                viewTermIndexFirstCharacter
                    staticSidebar
                    termIndexGroup.label
                    (not <| List.isEmpty termIndexGroup.terms)
            )
            termIndex
        )


viewQuickSearchButtonAndLetterGrid : Bool -> TermIndex -> Html Msg
viewQuickSearchButtonAndLetterGrid staticSidebar termIndex =
    div
        [ id ElementIds.quickSearchButtonAndLetterGrid
        , class "-mb-6 sticky top-0 -ml-0.5 pointer-events-none"
        ]
        [ div
            [ class "h-7 bg-white dark:bg-slate-900" ]
            []
        , viewQuickSearchButton
        , div
            [ class "px-3 bg-white dark:bg-slate-900" ]
            [ viewTermIndexFirstCharacterGrid staticSidebar termIndex ]
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
            [ viewQuickSearchButtonAndLetterGrid True termIndex
            , nav
                [ class "px-3" ]
                [ viewTermsIndex tabbable True termIndex ]
            ]
        ]


viewTopBar : GlossaryItems -> Components.DropdownMenu.Model -> Html Msg
viewTopBar glossaryItems exportDropdownMenu =
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
                , class "hidden ml-auto text-slate-500 w-8 h-8 -my-1 flex items-center justify-center hover:text-slate-600 lg:hidden dark:text-slate-400 dark:hover:text-slate-300"
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
            [ viewExportButton glossaryItems exportDropdownMenu ]
        ]


viewExportButton : GlossaryItems -> Components.DropdownMenu.Model -> Html Msg
viewExportButton glossaryItems exportDropdownMenu =
    Components.DropdownMenu.view
        (PageMsg.Internal << ExportDropdownMenuMsg)
        exportDropdownMenu
        [ text "Export" ]
        [ Components.DropdownMenu.choice
            [ text "Markdown" ]
            (PageMsg.Internal <| DownloadMarkdown glossaryItems)
        ]


viewOrderItemsBy : Model -> Html Msg
viewOrderItemsBy model =
    let
        tabbable =
            model.confirmDeleteIndex == Nothing
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


view : Model -> Document Msg
view model =
    case model.common.loadedGlossaryItems of
        Err error ->
            { title = "Glossary"
            , body = [ pre [] [ text <| Decode.errorToString error ] ]
            }

        Ok glossaryItems ->
            let
                editable =
                    model.makingChanges == ReadyForMakingChanges

                noModalDialogShown =
                    model.confirmDeleteIndex == Nothing

                termIndex =
                    termIndexFromGlossaryItems glossaryItems
            in
            { title = GlossaryTitle.toString model.common.title
            , body =
                [ Html.div
                    [ class "min-h-full"
                    , Extras.HtmlEvents.onKeydown
                        (\code ->
                            if code == Extras.HtmlEvents.enter then
                                Maybe.map (PageMsg.Internal << Delete) model.confirmDeleteIndex

                            else if code == Extras.HtmlEvents.escape then
                                Just <| PageMsg.Internal CancelDelete

                            else
                                Nothing
                        )
                    ]
                    [ viewMenuForMobile model noModalDialogShown termIndex
                    , viewStaticSidebarForDesktop noModalDialogShown termIndex
                    , div
                        [ class "lg:pl-64 flex flex-col" ]
                        [ viewTopBar glossaryItems model.exportDropdownMenu
                        , div
                            [ Html.Attributes.id ElementIds.container ]
                            [ header []
                                [ div
                                    [ class "flex flex-row justify-start lg:justify-end lg:border-b border-gray-300 dark:border-gray-700 lg:mb-4" ]
                                    [ Extras.Html.showIf editable <|
                                        div
                                            [ class "flex-none" ]
                                            [ viewEditTitleAndAboutButton noModalDialogShown model.common ]
                                    , div
                                        [ class "hidden lg:block ml-auto pb-3" ]
                                        [ viewExportButton glossaryItems model.exportDropdownMenu ]
                                    ]
                                , case model.makingChanges of
                                    MakingChangesHelpCollapsed ->
                                        viewMakingChangesHelp model.common.filename noModalDialogShown False

                                    MakingChangesHelpExpanded ->
                                        viewMakingChangesHelp model.common.filename noModalDialogShown True

                                    _ ->
                                        Extras.Html.nothing
                                , h1
                                    [ id ElementIds.title ]
                                    [ text <| GlossaryTitle.toString model.common.title ]
                                ]
                            , Html.main_
                                []
                                [ div
                                    [ id ElementIds.about ]
                                    [ p []
                                        [ text model.common.aboutParagraph ]
                                    , ul [] <|
                                        List.map
                                            (\aboutLink ->
                                                li []
                                                    [ a
                                                        [ target "_blank"
                                                        , href <| AboutLink.href aboutLink
                                                        , Accessibility.Key.tabbable <| (model.confirmDeleteIndex == Nothing)
                                                        ]
                                                        [ text <| AboutLink.body aboutLink ]
                                                    ]
                                            )
                                            model.common.aboutLinks
                                    ]
                                , glossaryItems
                                    |> (if model.common.orderItemsBy == CommonModel.Alphabetically then
                                            GlossaryItems.orderedAlphabetically

                                        else
                                            GlossaryItems.orderedByFrequency
                                       )
                                    |> viewCards model editable noModalDialogShown
                                ]
                            ]
                        ]
                    ]
                ]
            }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Components.DropdownMenu.subscriptions model.exportDropdownMenu
            |> Sub.map (ExportDropdownMenuMsg >> PageMsg.Internal)
        ]
