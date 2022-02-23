port module Pages.ListAll exposing (Model, Msg, init, update, view)

import Browser.Dom as Dom
import CommonModel exposing (CommonModel)
import Data.AboutHtml as AboutHtml
import Data.GlossaryItem as GlossaryItem exposing (GlossaryItem)
import Data.GlossaryItemIndex as GlossaryItemIndex exposing (GlossaryItemIndex)
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Data.TitleHeaderHtml as TitleHeaderHtml
import Dict exposing (Dict)
import ElementIds
import Extras.Html
import Extras.HtmlAttribute
import Extras.HtmlTree as HtmlTree exposing (HtmlTree(..))
import Extras.Http
import Html exposing (Attribute, Html, a, button, code, div, fieldset, h3, h5, input, label, legend, li, nav, p, pre, span, text, ul)
import Html.Attributes exposing (attribute, checked, class, for, href, id, name)
import Html.Events
import Http
import Icons
import Json.Decode as Decode
import PageMsg exposing (PageMsg)
import Process
import Svg exposing (circle, path, svg)
import Svg.Attributes exposing (cx, cy, d, fill, height, r, stroke, strokeLinecap, strokeLinejoin, strokeWidth, viewBox, width)
import Task



-- MODEL


type MenuForMobileVisibility
    = Visible
    | Disappearing
    | Invisible


type MakingChanges
    = NoHelpForMakingChanges
    | ReadyForMakingChanges
    | MakingChangesHelpCollapsed
    | MakingChangesHelpExpanded


type alias Model =
    { common : CommonModel
    , makingChanges : MakingChanges
    , menuForMobileVisibility : MenuForMobileVisibility
    , confirmDeleteIndex : Maybe GlossaryItemIndex
    , errorWhileDeleting : Maybe ( GlossaryItemIndex, String )
    }


type InternalMsg
    = NoOp
    | ToggleMakingChangesHelp
    | ShowMenuForMobile
    | StartHidingMenuForMobile
    | CompleteHidingMenuForMobile
    | ConfirmDelete GlossaryItemIndex
    | CancelDelete
    | Delete GlossaryItemIndex
    | Deleted GlossaryItems
    | FailedToDelete GlossaryItemIndex Http.Error
    | JumpToTermIndexGroup Bool String
    | ChangeOrderItemsBy CommonModel.OrderItemsBy


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
                , scrollToTopInElement idOfIndexForMobile
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

        ConfirmDelete index ->
            ( { model | confirmDeleteIndex = Just index }, preventBackgroundScrolling () )

        CancelDelete ->
            ( { model | confirmDeleteIndex = Nothing }, allowBackgroundScrolling () )

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
                        [ patchHtmlFile model.common.enableHelpForMakingChanges index updatedGlossaryItems
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
                        idOfStaticSidebarForDesktop

                    else
                        idOfIndexForMobile
            in
            ( model
            , Dom.getViewportOf idOfSidebarOrMenu
                |> Task.andThen
                    (\viewport ->
                        termIndexGroupLabel
                            |> idForTermIndexGroupLabel staticSidebar
                            |> Dom.getElement
                            |> Task.andThen
                                (\termIndexGroupElement ->
                                    if staticSidebar then
                                        idOfQuickSearchButtonAndLetterGrid
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


patchHtmlFile : Bool -> GlossaryItemIndex -> GlossaryItems -> Cmd Msg
patchHtmlFile enableHelpForMakingChanges indexOfItemBeingDeleted glossaryItems =
    Http.request
        { method = "PATCH"
        , headers = []
        , url = "/"
        , body =
            glossaryItems
                |> GlossaryItems.toHtmlTree enableHelpForMakingChanges
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
    idForGlossaryItemDiv >> scrollElementIntoView


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


viewMakingChangesHelp : Bool -> Html Msg
viewMakingChangesHelp expanded =
    div
        [ class "mb-5 rounded-md overflow-x-auto bg-amber-50 dark:bg-gray-700 text-gray-700 dark:text-gray-300 print:hidden"
        , class <|
            if expanded then
                "p-4"

            else
                "pt-4 pr-4 pl-4 pb-2"
        ]
        [ h3
            [ class "inline-flex text-lg leading-6 items-center font-medium text-gray-900 dark:text-gray-100 select-none"
            , Html.Events.onClick <| PageMsg.Internal ToggleMakingChangesHelp
            ]
            [ span
                [ class "text-gray-500 dark:text-gray-300" ]
                [ if expanded then
                    Icons.chevronDownSolid

                  else
                    Icons.chevronRightSolid
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
                        ]
                        [ text "Node.js" ]
                    , text " installed, then just run"
                    ]
                , pre
                    [ class "mt-5" ]
                    [ code []
                        [ text "sed -n '/START OF editor.js$/,$p' glossary.html | node" ]
                    ]
                , p
                    [ class "mt-5 max-w-xl" ]
                    [ text "(You'll need to be in the same directory.) This works best if the file is under version control." ]
                , p
                    [ class "mt-3 max-w-xl" ]
                    [ text "You can hide these instructions altogether by setting the "
                    , code [] [ text "data-enable-help-for-making-changes" ]
                    , text " attribute to "
                    , code [] [ text "false" ]
                    , text " on the "
                    , code [] [ text "<article id=\"glossary\">" ]
                    , text " element."
                    ]
                , p
                    [ class "mt-3 max-w-xl" ]
                    [ text "Custom file names are supported via"
                    ]
                , pre
                    [ class "mt-5" ]
                    [ code []
                        [ text "sed -n '/START OF editor.js$/,$p' custom.html | FILE=custom.html node" ]
                    ]
                ]
        ]


viewTermIndexItem : GlossaryItem.Term -> Html Msg
viewTermIndexItem term =
    li []
        [ a
            [ class "block border-l pl-4 -ml-px border-transparent hover:border-slate-400 dark:hover:border-slate-400 text-slate-700 hover:text-slate-900 dark:text-slate-400 dark:hover:text-slate-300"
            , Html.Attributes.href <| "#" ++ term.id
            , Html.Events.onClick <| PageMsg.Internal StartHidingMenuForMobile
            ]
            [ text term.body ]
        ]


idForTermIndexGroupLabel : Bool -> String -> String
idForTermIndexGroupLabel staticSidebar termIndexGroupLabel =
    if staticSidebar then
        "index-group-static-sidebar-" ++ termIndexGroupLabel

    else
        "index-group-" ++ termIndexGroupLabel


viewTermIndexGroup : Bool -> TermIndexGroup -> Html Msg
viewTermIndexGroup staticSidebar { label, terms } =
    li
        [ id <| idForTermIndexGroupLabel staticSidebar label
        , class "mt-6"
        ]
        [ h5
            [ class "mb-8 lg:mb-3 font-semibold text-slate-900 dark:text-slate-200" ]
            [ text label ]
        , ul
            [ class "space-y-6 lg:space-y-2 border-l border-slate-200 dark:border-slate-600" ]
            (List.map viewTermIndexItem terms)
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


idOfTermsIndex : Bool -> String
idOfTermsIndex staticSidebar =
    if staticSidebar then
        "terms-index-static-sidebar"

    else
        "terms-index"


viewTermsIndex : Bool -> TermIndex -> Html Msg
viewTermsIndex staticSidebar termIndex =
    ul
        [ id <| idOfTermsIndex staticSidebar
        , class "mb-10"
        ]
        (List.filterMap
            (\termIndexGroup ->
                if List.isEmpty termIndexGroup.terms then
                    Nothing

                else
                    Just <| viewTermIndexGroup staticSidebar termIndexGroup
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
            [ class "silcrow invisible group-hover:visible hover:visible" ]
            [ Html.a
                [ "#" ++ term.id |> Html.Attributes.href ]
                [ text "§" ]
            ]
        ]


viewGlossaryItemDetails : String -> Html Msg
viewGlossaryItemDetails details =
    Html.dd
        []
        [ text details ]


viewGlossaryItemRelatedTerms : Bool -> List GlossaryItem.RelatedTerm -> List (Html Msg)
viewGlossaryItemRelatedTerms itemHasSomeDetails relatedTerms =
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
                                    [ "#" ++ relatedTerm.idReference |> Html.Attributes.href ]
                                    [ text relatedTerm.body ]
                            )
                        |> List.intersperse (text ", ")
                   )
            )
        ]


viewGlossaryItemButton : List (Attribute Msg) -> Html Msg -> String -> Html Msg
viewGlossaryItemButton attributes icon label =
    button
        ([ Html.Attributes.type_ "button"
         , class "inline-flex space-x-2 text-gray-400 dark:text-gray-300 hover:text-gray-500 dark:hover:text-gray-400"
         ]
            ++ attributes
        )
        [ icon
        , span
            [ class "font-medium text-gray-600 dark:text-gray-300 hover:text-gray-700 dark:hover:text-gray-400" ]
            [ text label ]
        ]


idForGlossaryItemDiv : GlossaryItemIndex -> String
idForGlossaryItemDiv index =
    "glossary-item-" ++ (index |> GlossaryItemIndex.toInt |> String.fromInt)


viewGlossaryItem : GlossaryItemIndex -> Model -> Bool -> Maybe ( GlossaryItemIndex, String ) -> GlossaryItem -> Html Msg
viewGlossaryItem index model editable errorWhileDeleting glossaryItem =
    let
        errorDiv message =
            div
                [ class "flex justify-end mt-2" ]
                [ p
                    [ class "text-red-600" ]
                    [ text message ]
                ]

        itemSomeDetails =
            GlossaryItem.hasSomeDetails glossaryItem

        common =
            model.common
    in
    if editable then
        div
            [ class "flex flex-col justify-items-end"
            , id <| idForGlossaryItemDiv index
            ]
            [ div
                []
                (List.map viewGlossaryTerm glossaryItem.terms
                    ++ List.map viewGlossaryItemDetails glossaryItem.details
                    ++ viewGlossaryItemRelatedTerms itemSomeDetails glossaryItem.relatedTerms
                )
            , div
                [ class "print:hidden mt-3 flex flex-col flex-grow justify-end" ]
                [ div
                    [ class "flex justify-between" ]
                    [ span
                        [ class "inline-flex items-center" ]
                        [ viewGlossaryItemButton
                            [ Html.Events.onClick <|
                                PageMsg.NavigateToCreateOrEdit { common | maybeIndex = Just index }
                            ]
                            Icons.pencilSolid
                            "Edit"
                        ]
                    , span
                        [ class "ml-3 inline-flex items-center" ]
                        [ viewGlossaryItemButton
                            [ Html.Events.onClick <| PageMsg.Internal <| ConfirmDelete index ]
                            Icons.trashSolid
                            "Delete"
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
                ++ viewGlossaryItemRelatedTerms itemSomeDetails glossaryItem.relatedTerms
            )


viewConfirmDeleteModal : Maybe GlossaryItemIndex -> Html Msg
viewConfirmDeleteModal maybeIndexOfItemToDelete =
    div
        [ class "fixed z-10 inset-0 overflow-y-auto print:hidden"
        , Extras.HtmlAttribute.showIf (maybeIndexOfItemToDelete == Nothing) <| class "invisible"
        , attribute "aria-labelledby" ElementIds.modalTitle
        , attribute "role" "dialog"
        , attribute "aria-modal" "true"
        ]
        [ div
            [ class "flex items-end justify-center min-h-screen pt-4 px-4 pb-20 text-center sm:block sm:p-0" ]
            [ div
                [ class "fixed inset-0 bg-gray-500 dark:bg-gray-800 bg-opacity-75 dark:bg-opacity-75 transition-opacity"
                , if maybeIndexOfItemToDelete == Nothing then
                    class "ease-in duration-200 opacity-0"

                  else
                    class "ease-out duration-300 opacity-100"
                , attribute "aria-hidden" "true"
                , Html.Events.onClick <| PageMsg.Internal CancelDelete
                ]
                []
            , span
                [ class "hidden sm:inline-block sm:align-middle sm:h-screen"
                , attribute "aria-hidden" "true"
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
                        [ svg
                            [ Svg.Attributes.class "h-6 w-6 text-red-600 dark:text-red-800"
                            , fill "none"
                            , viewBox "0 0 24 24"
                            , stroke "currentColor"
                            , attribute "aria-hidden" "true"
                            ]
                            [ path
                                [ strokeLinecap "round"
                                , strokeLinejoin "round"
                                , strokeWidth "2"
                                , d
                                    "M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z"
                                ]
                                []
                            ]
                        ]
                    , div
                        [ class "mt-3 text-center sm:mt-0 sm:ml-4 sm:text-left" ]
                        [ h3
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
                    [ button
                        [ Html.Attributes.type_ "button"
                        , class "w-full inline-flex justify-center rounded-md border border-transparent shadow-sm px-4 py-2 bg-red-600 dark:bg-red-400 text-base font-medium text-white dark:text-gray-800 hover:bg-red-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-red-500 sm:ml-3 sm:w-auto sm:text-sm"
                        , maybeIndexOfItemToDelete
                            |> Maybe.map (Html.Events.onClick << PageMsg.Internal << Delete)
                            |> Maybe.withDefault Extras.HtmlAttribute.empty
                        ]
                        [ text "Delete" ]
                    , button
                        [ Html.Attributes.type_ "button"
                        , class "mt-3 w-full inline-flex justify-center rounded-md border border-gray-300 dark:border-gray-700 shadow-sm px-4 py-2 bg-white dark:bg-gray-500 text-base font-medium text-gray-700 dark:text-gray-200 hover:bg-gray-50 dark:hover:bg-gray-600 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 sm:mt-0 sm:w-auto sm:text-sm"
                        , Html.Events.onClick <| PageMsg.Internal CancelDelete
                        ]
                        [ text "Cancel" ]
                    ]
                ]
            ]
        ]


viewCreateGlossaryItemButtonForEmptyState : Model -> Html Msg
viewCreateGlossaryItemButtonForEmptyState model =
    let
        common =
            model.common
    in
    div
        [ class "pt-4 print:hidden" ]
        [ button
            [ Html.Attributes.type_ "button"
            , class "relative block max-w-lg border-2 border-gray-300 border-dashed rounded-lg p-9 text-center hover:border-gray-400 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
            , Html.Events.onClick <|
                PageMsg.NavigateToCreateOrEdit { common | maybeIndex = Nothing }
            ]
            [ svg
                [ Svg.Attributes.class "mx-auto h-12 w-12 text-gray-400"
                , stroke "currentColor"
                , fill "none"
                , viewBox "0 0 20 20"
                ]
                [ path
                    [ d "M5 3a2 2 0 00-2 2v2a2 2 0 002 2h2a2 2 0 002-2V5a2 2 0 00-2-2H5zM5 11a2 2 0 00-2 2v2a2 2 0 002 2h2a2 2 0 002-2v-2a2 2 0 00-2-2H5zM11 5a2 2 0 012-2h2a2 2 0 012 2v2a2 2 0 01-2 2h-2a2 2 0 01-2-2V5zM14 11a1 1 0 011 1v1h1a1 1 0 110 2h-1v1a1 1 0 11-2 0v-1h-1a1 1 0 110-2h1v-1a1 1 0 011-1z" ]
                    []
                ]
            , span
                [ class "mt-2 block font-medium text-gray-900 dark:text-gray-200" ]
                [ text "Create a new glossary item" ]
            ]
        ]


viewCreateGlossaryItemButton : Model -> Html Msg
viewCreateGlossaryItemButton model =
    let
        common =
            model.common
    in
    div
        [ class "pb-2 print:hidden" ]
        [ button
            [ Html.Attributes.type_ "button"
            , class "inline-flex items-center px-4 py-2 border border-transparent shadow-sm font-medium rounded-md text-indigo-700 dark:text-indigo-300 bg-indigo-100 dark:bg-indigo-900 hover:bg-indigo-200 dark:hover:bg-indigo-800 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
            , Html.Events.onClick <| PageMsg.NavigateToCreateOrEdit { common | maybeIndex = Nothing }
            ]
            [ svg
                [ Svg.Attributes.class "-ml-1 mr-2 h-5 w-5", viewBox "0 0 20 20", fill "currentColor" ]
                [ path
                    [ d "M5 3a2 2 0 00-2 2v2a2 2 0 002 2h2a2 2 0 002-2V5a2 2 0 00-2-2H5zM5 11a2 2 0 00-2 2v2a2 2 0 002 2h2a2 2 0 002-2v-2a2 2 0 00-2-2H5zM11 5a2 2 0 012-2h2a2 2 0 012 2v2a2 2 0 01-2 2h-2a2 2 0 01-2-2V5zM14 11a1 1 0 011 1v1h1a1 1 0 110 2h-1v1a1 1 0 11-2 0v-1h-1a1 1 0 110-2h1v-1a1 1 0 011-1z" ]
                    []
                ]
            , text "Create a new glossary item"
            ]
        ]


viewCards : Model -> Bool -> List ( GlossaryItemIndex, GlossaryItem ) -> Html Msg
viewCards model editable indexedGlossaryItems =
    Html.article
        [ Html.Attributes.id ElementIds.items ]
        [ case model.makingChanges of
            MakingChangesHelpCollapsed ->
                viewMakingChangesHelp False

            MakingChangesHelpExpanded ->
                viewMakingChangesHelp True

            _ ->
                Extras.Html.nothing
        , Extras.Html.showIf editable <|
            div
                [ class "pb-4" ]
                [ if List.isEmpty indexedGlossaryItems then
                    viewCreateGlossaryItemButtonForEmptyState model

                  else
                    viewCreateGlossaryItemButton model
                ]
        , viewOrderItemsBy model
        , Html.dl
            []
            (indexedGlossaryItems
                |> List.map
                    (\( index, glossaryItem ) ->
                        viewGlossaryItem
                            index
                            model
                            editable
                            model.errorWhileDeleting
                            glossaryItem
                    )
            )
        , viewConfirmDeleteModal model.confirmDeleteIndex
        ]


idOfIndexForMobile : String
idOfIndexForMobile =
    "menu-for-mobile"


viewMenuForMobile : Model -> TermIndex -> Html Msg
viewMenuForMobile model termIndex =
    div
        [ class "invisible" |> Extras.HtmlAttribute.showIf (model.menuForMobileVisibility == Invisible)
        , class "fixed inset-0 flex z-40 lg:hidden"
        , attribute "role" "dialog"
        , attribute "aria-modal" "true"
        ]
        [ div
            [ class "fixed inset-0 bg-gray-600 bg-opacity-75"
            , if model.menuForMobileVisibility == Visible then
                class "transition-opacity ease-linear duration-300 opacity-100"

              else
                class "transition-opacity ease-linear duration-300 opacity-0"
            , Html.Events.onClick <| PageMsg.Internal StartHidingMenuForMobile
            , attribute "aria-hidden" "true"
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
                    , class "ml-1 flex items-center justify-center h-10 w-10 rounded-full focus:outline-none focus:ring-2 focus:ring-inset focus:ring-white"
                    , Html.Events.onClick <| PageMsg.Internal StartHidingMenuForMobile
                    ]
                    [ span
                        [ class "sr-only" ]
                        [ text "Close sidebar"
                        ]
                    , svg
                        [ Svg.Attributes.class "h-6 w-6 text-white"
                        , fill "none"
                        , viewBox "0 0 24 24"
                        , stroke "currentColor"
                        ]
                        [ path
                            [ strokeLinecap "round"
                            , strokeLinejoin "round"
                            , strokeWidth "2"
                            , d "M6 18L18 6M6 6l12 12"
                            ]
                            []
                        ]
                    ]
                ]
            , div
                [ id idOfIndexForMobile
                , class "flex-1 h-0 overflow-y-auto"
                ]
                [ nav
                    [ class "px-4 pt-1 pb-6" ]
                    [ viewTermIndexFirstCharacterGrid False termIndex
                    , viewTermsIndex False termIndex
                    ]
                ]
            ]
        , div
            [ class "flex-shrink-0 w-14", attribute "aria-hidden" "true" ]
            []
        ]


viewTermIndexFirstCharacter : Bool -> String -> Bool -> Html Msg
viewTermIndexFirstCharacter staticSidebar firstCharacter enabled =
    if enabled then
        button
            [ Html.Attributes.type_ "button"
            , class "inline-flex items-center m-0.5 px-3 py-2 border border-gray-200 dark:border-gray-800 shadow-sm leading-4 font-medium rounded-md text-gray-700 dark:text-slate-200 bg-white dark:bg-slate-900 hover:bg-gray-50 dark:hover:bg-slate-800 focus:outline-none focus:ring-2 focus:ring-offset-1 focus:ring-indigo-500"
            , Html.Events.onClick <| PageMsg.Internal <| JumpToTermIndexGroup staticSidebar firstCharacter
            ]
            [ text firstCharacter ]

    else
        button
            [ Html.Attributes.type_ "button"
            , Html.Attributes.disabled True
            , class "inline-flex items-center m-0.5 px-3 py-2 border border-gray-200 dark:border-gray-800 shadow-sm leading-4 font-medium rounded-md text-gray-300 dark:text-slate-600 bg-white dark:bg-slate-900"
            ]
            [ text firstCharacter ]


viewTermIndexFirstCharacterGrid : Bool -> TermIndex -> Html Msg
viewTermIndexFirstCharacterGrid staticSidebar termIndex =
    div
        [ class "bg-white dark:bg-slate-900 select-none pointer-events-auto" ]
        (List.map
            (\termIndexGroup ->
                viewTermIndexFirstCharacter staticSidebar termIndexGroup.label <| not <| List.isEmpty termIndexGroup.terms
            )
            termIndex
        )


idOfQuickSearchButtonAndLetterGrid : String
idOfQuickSearchButtonAndLetterGrid =
    "search-button-and-letter-grid"


viewQuickSearchButtonAndLetterGrid : Bool -> TermIndex -> Html Msg
viewQuickSearchButtonAndLetterGrid staticSidebar termIndex =
    div
        [ id idOfQuickSearchButtonAndLetterGrid
        , class "-mb-6 sticky top-0 -ml-0.5 pointer-events-none"
        ]
        [ div
            [ class "h-7 bg-white dark:bg-slate-900" ]
            []

        -- , viewQuickSearchButton
        , div
            [ class "px-3 bg-white dark:bg-slate-900" ]
            [ viewTermIndexFirstCharacterGrid staticSidebar termIndex ]
        , div
            [ class "h-8 bg-gradient-to-b from-white dark:from-slate-900" ]
            []
        ]


idOfStaticSidebarForDesktop : String
idOfStaticSidebarForDesktop =
    "static-sidebar-for-desktop"


viewStaticSidebarForDesktop : TermIndex -> Html Msg
viewStaticSidebarForDesktop termIndex =
    div
        [ class "hidden print:hidden lg:flex lg:flex-col lg:w-64 lg:fixed lg:inset-y-0 lg:border-r lg:border-gray-200 lg:bg-white lg:dark:border-gray-800 lg:dark:bg-gray-900"
        ]
        [ div
            [ id idOfStaticSidebarForDesktop
            , class "h-0 flex-1 flex flex-col overflow-y-auto"
            ]
            [ viewQuickSearchButtonAndLetterGrid True termIndex
            , nav
                [ class "px-3" ]
                [ viewTermsIndex True termIndex ]
            ]
        ]


viewTopBar : Html Msg
viewTopBar =
    div
        [ class "sticky top-0 z-10 flex-shrink-0 flex h-16 bg-white dark:bg-gray-900 border-b border-gray-200 dark:border-gray-800 lg:hidden print:hidden items-center" ]
        [ button
            [ Html.Attributes.type_ "button"
            , class "px-4 border-r border-gray-200 dark:border-gray-700 text-gray-500 focus:outline-none lg:hidden"
            , Html.Events.onClick <| PageMsg.Internal ShowMenuForMobile
            ]
            [ span
                [ class "sr-only" ]
                [ text "Open sidebar"
                ]
            , svg
                [ Svg.Attributes.class "h-6 w-6"
                , fill "none"
                , viewBox "0 0 24 24"
                , stroke "currentColor"
                , attribute "aria-hidden" "true"
                ]
                [ path
                    [ strokeLinecap "round"
                    , strokeLinejoin "round"
                    , strokeWidth "2"
                    , d "M4 6h16M4 12h8m-8 6h16"
                    ]
                    []
                ]
            ]
        , div
            [ class "hidden flex-1 flex justify-between px-4 sm:px-6 lg:px-8 dark:bg-gray-900 dark:text-white" ]
            [ button
                [ Html.Attributes.type_ "button"
                , class "ml-auto text-slate-500 w-8 h-8 -my-1 flex items-center justify-center hover:text-slate-600 lg:hidden dark:text-slate-400 dark:hover:text-slate-300"
                ]
                [ span
                    [ class "sr-only" ]
                    [ text "Search" ]
                , svg
                    [ width "24"
                    , height "24"
                    , fill "none"
                    , stroke "currentColor"
                    , strokeWidth "2"
                    , strokeLinecap "round"
                    , strokeLinejoin "round"
                    , attribute "aria-hidden" "true"
                    ]
                    [ path [ d "m19 19-3.5-3.5" ] []
                    , circle [ cx "11", cy "11", r "6" ] []
                    ]
                ]
            ]
        ]


viewOrderItemsBy : Model -> Html Msg
viewOrderItemsBy model =
    div
        [ class "print:hidden border-t border-gray-300 dark:border-gray-700 pt-6 pb-6" ]
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
                    [ input
                        [ checked <| model.common.orderItemsBy == CommonModel.Alphabetically
                        , class "focus:ring-indigo-500 h-4 w-4 dark:bg-gray-200 text-indigo-600 dark:text-indigo-400 border-gray-300 dark:border-gray-500"
                        , id ElementIds.orderItemsAlphabetically
                        , name "order-items-by"
                        , Html.Attributes.type_ "radio"
                        , Html.Events.onClick <| PageMsg.Internal <| ChangeOrderItemsBy CommonModel.Alphabetically
                        ]
                        []
                    , label
                        [ class "ml-3 block font-medium text-gray-700 dark:text-gray-300 select-none"
                        , for ElementIds.orderItemsAlphabetically
                        ]
                        [ text "alphabetically" ]
                    ]
                , div
                    [ class "flex items-center" ]
                    [ input
                        [ checked <| model.common.orderItemsBy == CommonModel.MostFrequentFirst
                        , class "focus:ring-indigo-500 h-4 w-4 dark:bg-gray-200 text-indigo-600 dark:text-indigo-400 border-gray-300 dark:border-gray-500"
                        , id ElementIds.orderItemsMostFrequentFirst
                        , name "order-items-by"
                        , Html.Attributes.type_ "radio"
                        , Html.Events.onClick <| PageMsg.Internal <| ChangeOrderItemsBy CommonModel.MostFrequentFirst
                        ]
                        []
                    , label
                        [ class "ml-3 block font-medium text-gray-700 dark:text-gray-300 select-none"
                        , for ElementIds.orderItemsMostFrequentFirst
                        ]
                        [ text "most frequent first" ]
                    ]
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    case model.common.loadedGlossaryItems of
        Err error ->
            pre [] [ text <| Decode.errorToString error ]

        Ok glossaryItems ->
            let
                editable =
                    model.makingChanges == ReadyForMakingChanges

                termIndex =
                    termIndexFromGlossaryItems glossaryItems
            in
            div
                [ class "min-h-full" ]
                [ viewMenuForMobile model termIndex
                , viewStaticSidebarForDesktop termIndex
                , div
                    [ class "lg:pl-64 flex flex-col" ]
                    [ viewTopBar
                    , div
                        [ Html.Attributes.id ElementIds.outer ]
                        [ div [] <| TitleHeaderHtml.toVirtualDom model.common.titleHeaderHtml
                        , Html.main_
                            []
                            [ div [] <| AboutHtml.toVirtualDom model.common.aboutHtml
                            , glossaryItems
                                |> (if model.common.orderItemsBy == CommonModel.Alphabetically then
                                        GlossaryItems.orderedAlphabetically

                                    else
                                        GlossaryItems.orderedByFrequency
                                   )
                                |> viewCards model editable
                            ]
                        ]
                    ]
                ]
