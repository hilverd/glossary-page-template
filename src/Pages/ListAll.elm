port module Pages.ListAll exposing (Model, Msg, init, update, view)

import Array
import Data.GlossaryItem as GlossaryItem exposing (GlossaryItem)
import Data.GlossaryItems as GlossaryItems
import Data.LoadedGlossaryItems exposing (LoadedGlossaryItems)
import Extras.Array
import Extras.Html
import Extras.HtmlAttribute
import Extras.HtmlTree as HtmlTree exposing (HtmlTree(..))
import Extras.Http
import Extras.Layout as Layout
import Html exposing (Attribute, Html, a, button, code, div, h3, p, pre, span, text)
import Html.Attributes exposing (attribute, class, href, id)
import Html.Events
import Http
import Icons
import Json.Decode as Decode
import PageMsg exposing (PageMsg)
import Svg exposing (path, svg)
import Svg.Attributes exposing (d, fill, stroke, strokeLinecap, strokeLinejoin, strokeWidth, viewBox)



-- MODEL


type MakingChanges
    = NoHelpForMakingChanges
    | ReadyForMakingChanges
    | MakingChangesHelpCollapsed
    | MakingChangesHelpExpanded


type alias Model =
    { enableHelpForMakingChanges : Bool
    , makingChanges : MakingChanges
    , glossaryItems : LoadedGlossaryItems
    , confirmDeleteIndex : Maybe Int
    , errorWhileDeleting : Maybe ( Int, String )
    }


type InternalMsg
    = ToggleMakingChangesHelp
    | ConfirmDelete Int
    | CancelDelete
    | Delete Int
    | Deleted (List GlossaryItem)
    | FailedToDelete Int Http.Error


type alias Msg =
    PageMsg InternalMsg


init : Bool -> Bool -> LoadedGlossaryItems -> ( Model, Cmd Msg )
init editorIsRunning enableHelpForMakingChanges glossaryItems =
    ( { enableHelpForMakingChanges = enableHelpForMakingChanges
      , makingChanges =
            case ( editorIsRunning, enableHelpForMakingChanges ) of
                ( True, _ ) ->
                    ReadyForMakingChanges

                ( False, True ) ->
                    MakingChangesHelpCollapsed

                ( False, False ) ->
                    NoHelpForMakingChanges
      , glossaryItems = glossaryItems
      , confirmDeleteIndex = Nothing
      , errorWhileDeleting = Nothing
      }
    , Cmd.none
    )



-- PORTS


port allowBackgroundScrolling : () -> Cmd msg


port preventBackgroundScrolling : () -> Cmd msg



-- UPDATE


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

        ConfirmDelete index ->
            ( { model | confirmDeleteIndex = Just index }, preventBackgroundScrolling () )

        CancelDelete ->
            ( { model | confirmDeleteIndex = Nothing }, allowBackgroundScrolling () )

        Delete index ->
            case model.glossaryItems of
                Ok glossaryItems ->
                    let
                        updatedGlossaryItems =
                            glossaryItems
                                |> Array.fromList
                                |> Extras.Array.delete index
                                |> Array.toList
                                |> GlossaryItems.sanitise
                    in
                    ( { model
                        | confirmDeleteIndex = Nothing
                        , errorWhileDeleting = Nothing
                      }
                    , Cmd.batch
                        [ patchHtmlFile model.enableHelpForMakingChanges index updatedGlossaryItems
                        , allowBackgroundScrolling ()
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        Deleted updatedGlossaryItems ->
            ( { model | glossaryItems = Ok updatedGlossaryItems }, Cmd.none )

        FailedToDelete indexOfItemBeingDeleted error ->
            ( { model
                | errorWhileDeleting =
                    Just ( indexOfItemBeingDeleted, Extras.Http.errorToHumanReadable error )
              }
            , Cmd.none
            )


patchHtmlFile : Bool -> Int -> List GlossaryItem -> Cmd Msg
patchHtmlFile enableHelpForMakingChanges indexOfItemBeingDeleted glossaryItems =
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
                            PageMsg.Internal <| Deleted glossaryItems

                        Err error ->
                            PageMsg.Internal <| FailedToDelete indexOfItemBeingDeleted error
                )
        , timeout = Nothing
        , tracker = Nothing
        }



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
                    [ class "mt-5 mb-5" ]
                    [ code []
                        [ text "sed -n '/START OF editor.js$/,$p' glossary.html | node" ]
                    ]
                , p
                    [ class "max-w-xl" ]
                    [ text "where "
                    , code [] [ text "glossary.html" ]
                    , text " is the name of the glossary file (you'll need to be in the same directory). This works best if the file is under version control."
                    ]
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
                ]
        ]


viewTocItem : GlossaryItem.Term -> Html Msg
viewTocItem term =
    Html.li
        [ class "toc inline" ]
        [ Html.a
            [ class "font-medium"
            , "#" ++ term.id |> Html.Attributes.href
            ]
            [ text term.body ]
        ]


viewToc : List GlossaryItem -> Html Msg
viewToc glossaryItems =
    let
        separator =
            Html.li
                [ class "toc inline px-1 text-gray-500" ]
                [ text "•" ]
    in
    Html.nav
        [ Html.Attributes.id "toc"
        , class "pb-4 print:hidden"
        ]
        [ Html.ul
            []
            (glossaryItems
                |> List.concatMap .terms
                |> List.sortBy .body
                |> List.map viewTocItem
                |> List.intersperse separator
            )
        ]


viewGlossaryTerm : GlossaryItem.Term -> Html Msg
viewGlossaryTerm term =
    Html.dt
        []
        [ Html.dfn
            [ Html.Attributes.id term.id ]
            [ if term.isAbbreviation then
                Html.abbr [] [ text term.body ]

              else
                text term.body
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


viewGlossaryItem : Bool -> LoadedGlossaryItems -> Bool -> Maybe ( Int, String ) -> GlossaryItem -> Int -> Html Msg
viewGlossaryItem enableHelpForMakingChanges glossaryItems editable errorWhileDeleting glossaryItem index =
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
    in
    if editable then
        div
            [ class "flex flex-col justify-items-end" ]
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
                            [ Html.Events.onClick <| PageMsg.NavigateToCreateOrEdit enableHelpForMakingChanges (Just index) glossaryItems ]
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


viewConfirmDeleteModal : Maybe Int -> Html Msg
viewConfirmDeleteModal maybeIndex =
    div
        [ class "fixed z-10 inset-0 overflow-y-auto print:hidden"
        , Extras.HtmlAttribute.showIf (maybeIndex == Nothing) <| class "invisible"
        , attribute "aria-labelledby" "modal-title"
        , attribute "role" "dialog"
        , attribute "aria-modal" "true"
        ]
        [ div
            [ class "flex items-end justify-center min-h-screen pt-4 px-4 pb-20 text-center sm:block sm:p-0" ]
            [ div
                [ class "fixed inset-0 bg-gray-500 dark:bg-gray-800 bg-opacity-75 dark:bg-opacity-75 transition-opacity"
                , if maybeIndex == Nothing then
                    class "ease-in duration-200 opacity-0"

                  else
                    class "ease-out duration-300 opacity-100"
                , attribute "aria-hidden" "true"
                , Html.Events.onClick <| PageMsg.Internal CancelDelete
                ]
                []
            , span
                [ class "hidden sm:inline-block sm:align-middle sm:h-screen", attribute "aria-hidden" "true" ]
                [ text "\u{200B}" ]
            , div
                [ class "inline-block align-bottom bg-white dark:bg-gray-700 rounded-lg px-4 pt-5 pb-4 text-left overflow-hidden shadow-xl transform transition-all sm:my-8 sm:align-middle sm:max-w-lg sm:w-full sm:p-6"
                , if maybeIndex == Nothing then
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
                            [ class "text-lg leading-6 font-medium text-gray-900 dark:text-gray-100", id "modal-title" ]
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
                        , maybeIndex
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


viewCreateGlossaryItemButtonForEmptyState : Bool -> LoadedGlossaryItems -> Html Msg
viewCreateGlossaryItemButtonForEmptyState enableHelpForMakingChanges glossaryItems =
    div [ class "print:hidden" ]
        [ button
            [ Html.Attributes.type_ "button"
            , class "relative block max-w-lg border-2 border-gray-300 border-dashed rounded-lg p-9 text-center hover:border-gray-400 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
            , Html.Events.onClick <| PageMsg.NavigateToCreateOrEdit enableHelpForMakingChanges Nothing glossaryItems
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


viewCreateGlossaryItemButton : Bool -> LoadedGlossaryItems -> Html Msg
viewCreateGlossaryItemButton enableHelpForMakingChanges glossaryItems =
    div [ class "pt-4 print:hidden" ]
        [ button
            [ Html.Attributes.type_ "button"
            , class "inline-flex items-center px-4 py-2 border border-transparent shadow-sm font-medium rounded-md text-indigo-700 dark:text-indigo-300 bg-indigo-100 dark:bg-indigo-900 hover:bg-indigo-200 dark:hover:bg-indigo-800 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
            , Html.Events.onClick <| PageMsg.NavigateToCreateOrEdit enableHelpForMakingChanges Nothing glossaryItems
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


view : Model -> Html Msg
view model =
    case model.glossaryItems of
        Err error ->
            pre [] [ text <| Decode.errorToString error ]

        Ok glossaryItems ->
            let
                editable =
                    model.makingChanges == ReadyForMakingChanges
            in
            Html.article
                [ Html.Attributes.id "glossary" ]
                [ case model.makingChanges of
                    MakingChangesHelpCollapsed ->
                        viewMakingChangesHelp False

                    MakingChangesHelpExpanded ->
                        viewMakingChangesHelp True

                    _ ->
                        Extras.Html.nothing
                , viewToc glossaryItems
                , Html.dl
                    []
                    (glossaryItems
                        |> List.indexedMap Tuple.pair
                        |> List.map
                            (\( index, glossaryItem ) ->
                                viewGlossaryItem
                                    model.enableHelpForMakingChanges
                                    model.glossaryItems
                                    editable
                                    model.errorWhileDeleting
                                    glossaryItem
                                    index
                            )
                    )
                , Extras.Html.showIf editable <|
                    if List.isEmpty glossaryItems then
                        viewCreateGlossaryItemButtonForEmptyState model.enableHelpForMakingChanges model.glossaryItems

                    else
                        viewCreateGlossaryItemButton model.enableHelpForMakingChanges model.glossaryItems
                , viewConfirmDeleteModal model.confirmDeleteIndex
                ]
