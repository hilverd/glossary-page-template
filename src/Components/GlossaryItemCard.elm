module Components.GlossaryItemCard exposing (Style(..), view)

import Accessibility exposing (Html, div, p, span, text)
import Accessibility.Aria
import Accessibility.Key exposing (tabbable)
import Components.Button
import Data.FeatureFlag exposing (enableFeaturesInProgress)
import Data.GlossaryItem as GlossaryItem
import Data.GlossaryItem.Details as Details
import Data.GlossaryItem.RelatedTerm as RelatedTerm exposing (RelatedTerm)
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItemIndex exposing (GlossaryItemIndex)
import Data.GlossaryItemWithPreviousAndNext exposing (GlossaryItemWithPreviousAndNext)
import ElementIds
import Extras.Html
import Extras.HtmlAttribute
import Extras.HtmlEvents
import Extras.Url exposing (fragmentOnly)
import Html
import Html.Attributes exposing (class, id, style)
import Html.Events
import Icons
import Svg.Attributes


type Style msg
    = Preview
    | Normal
        { tabbable : Bool
        , onClickViewFull : msg
        , onClickEdit : msg
        , onClickDelete : msg
        , onClickItem : GlossaryItemIndex -> msg
        , onClickRelatedTerm : RelatedTerm -> msg
        , editable : Bool
        , shownAsSingle : Bool
        , errorWhileDeleting : Maybe ( GlossaryItemIndex, String )
        }


view :
    { enableMathSupport : Bool
    , makeLinksTabbable : Bool
    , enableLastUpdatedDates : Bool
    }
    -> Style msg
    -> GlossaryItemWithPreviousAndNext
    -> Html msg
view { enableMathSupport, makeLinksTabbable, enableLastUpdatedDates } style glossaryItemWithPreviousAndNext =
    Extras.Html.showMaybe
        (\( index, glossaryItem ) ->
            case style of
                Preview ->
                    let
                        itemHasSomeDetails : Bool
                        itemHasSomeDetails =
                            GlossaryItem.hasSomeDetails glossaryItem

                        tabbable : Bool
                        tabbable =
                            True
                    in
                    div
                        [ Html.Attributes.style "max-height" "100%" ]
                        (List.map
                            (viewGlossaryTerm
                                { enableMathSupport = enableMathSupport
                                , preview = True
                                , tabbable = tabbable
                                , showSilcrow = False
                                }
                            )
                            glossaryItem.terms
                            ++ (if glossaryItem.needsUpdating then
                                    [ Html.dd
                                        [ class "needs-updating" ]
                                        [ span
                                            []
                                            [ text "Needs updating" ]
                                        ]
                                    ]

                                else
                                    []
                               )
                            ++ List.map
                                (viewGlossaryItemDetails
                                    { enableMathSupport = enableMathSupport
                                    , makeLinksTabbable = makeLinksTabbable
                                    }
                                )
                                glossaryItem.details
                            ++ viewGlossaryItemRelatedTerms enableMathSupport True tabbable itemHasSomeDetails Nothing glossaryItem.relatedTerms
                        )

                Normal { tabbable, onClickViewFull, onClickEdit, onClickDelete, onClickItem, onClickRelatedTerm, editable, shownAsSingle, errorWhileDeleting } ->
                    let
                        itemHasSomeDetails : Bool
                        itemHasSomeDetails =
                            GlossaryItem.hasSomeDetails glossaryItem
                    in
                    if shownAsSingle then
                        div
                            [ Html.Attributes.style "max-height" "100%"
                            , Html.Attributes.style "border-width" "0px"
                            , class "mt-1.5"
                            ]
                            [ viewAsSingle
                                { tabbable = tabbable
                                , enableMathSupport = enableMathSupport
                                , makeLinksTabbable = makeLinksTabbable
                                , enableLastUpdatedDates = enableLastUpdatedDates
                                , onClickItem = onClickItem
                                , onClickRelatedTerm = onClickRelatedTerm
                                }
                                glossaryItemWithPreviousAndNext
                            ]

                    else if editable then
                        div
                            [ class "flex flex-col justify-items-end"
                            , id <| ElementIds.glossaryItemDiv index
                            ]
                            [ div
                                []
                                [ Extras.Html.showIf (enableFeaturesInProgress && not shownAsSingle) <|
                                    div
                                        [ class "print:hidden hidden sm:block float-right sticky top-0 bg-white dark:bg-gray-800 bg-opacity-75 dark:bg-opacity-75 p-0.5 rounded-full" ]
                                        [ span
                                            []
                                            [ Components.Button.text
                                                [ Accessibility.Key.tabbable tabbable
                                                , Accessibility.Aria.label "View as single item"
                                                , Html.Attributes.title "View as single item"
                                                , Html.Events.onClick onClickViewFull
                                                ]
                                                [ Icons.window
                                                    [ Svg.Attributes.class "h-5 w-5 text-gray-400 dark:text-gray-300 hover:text-gray-500 dark:hover:text-gray-400" ]
                                                ]
                                            ]
                                        ]
                                , div
                                    []
                                    (List.map
                                        (viewGlossaryTerm
                                            { enableMathSupport = enableMathSupport
                                            , preview = False
                                            , tabbable = tabbable
                                            , showSilcrow = True
                                            }
                                        )
                                        glossaryItem.terms
                                        ++ (if glossaryItem.needsUpdating then
                                                [ Html.dd
                                                    [ class "needs-updating" ]
                                                    [ span
                                                        []
                                                        [ text "Needs updating" ]
                                                    ]
                                                ]

                                            else
                                                []
                                           )
                                        ++ List.map
                                            (viewGlossaryItemDetails
                                                { enableMathSupport = enableMathSupport
                                                , makeLinksTabbable = makeLinksTabbable
                                                }
                                            )
                                            glossaryItem.details
                                        ++ viewGlossaryItemRelatedTerms enableMathSupport False tabbable itemHasSomeDetails Nothing glossaryItem.relatedTerms
                                    )
                                ]
                            , div
                                [ class "print:hidden mt-3 flex flex-col flex-grow justify-end" ]
                                [ Extras.Html.showIf enableLastUpdatedDates <|
                                    Extras.Html.showMaybe
                                        (\lastUpdatedDate ->
                                            div
                                                [ class "text-right text-sm mt-1.5 mb-2.5 text-gray-500 dark:text-gray-400" ]
                                                [ text "Updated: "
                                                , Html.node "last-updated"
                                                    [ Html.Attributes.attribute "datetime" lastUpdatedDate ]
                                                    []
                                                ]
                                        )
                                        glossaryItem.lastUpdatedDate
                                , div
                                    [ class "flex justify-between" ]
                                    [ span
                                        [ class "inline-flex items-center" ]
                                        [ Components.Button.text
                                            [ Html.Events.onClick onClickEdit
                                            , Accessibility.Key.tabbable tabbable
                                            ]
                                            [ Icons.pencil
                                                [ Svg.Attributes.class "h-5 w-5 text-gray-400 dark:text-gray-300 hover:text-gray-500 dark:hover:text-gray-400" ]
                                            , span
                                                [ class "font-medium text-gray-600 dark:text-gray-300 hover:text-gray-700 dark:hover:text-gray-400" ]
                                                [ text "Edit" ]
                                            ]
                                        ]
                                    , span
                                        [ class "ml-3 inline-flex items-center" ]
                                        [ Components.Button.text
                                            [ Html.Events.onClick onClickDelete
                                            , Accessibility.Key.tabbable tabbable
                                            ]
                                            [ Icons.trash
                                                [ Svg.Attributes.class "h-5 w-5 text-gray-400 dark:text-gray-300 hover:text-gray-500 dark:hover:text-gray-400" ]
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
                        div
                            [ class "flex flex-col justify-between"
                            , id <| ElementIds.glossaryItemDiv index
                            ]
                            [ div
                                [ class "flex-1" ]
                                [ Extras.Html.showIf enableFeaturesInProgress <|
                                    div
                                        [ class "print:hidden hidden sm:block float-right sticky top-0 bg-white dark:bg-gray-800 bg-opacity-75 dark:bg-opacity-75 p-0.5 rounded-full" ]
                                        [ span
                                            []
                                            [ Components.Button.text
                                                [ Accessibility.Key.tabbable tabbable
                                                , Accessibility.Aria.label "View as single item"
                                                , Html.Attributes.title "View as single item"
                                                , Html.Events.onClick onClickViewFull
                                                ]
                                                [ Icons.window
                                                    [ Svg.Attributes.class "h-5 w-5 text-gray-400 dark:text-gray-300 hover:text-gray-500 dark:hover:text-gray-400" ]
                                                ]
                                            ]
                                        ]
                                , div []
                                    (List.map
                                        (viewGlossaryTerm
                                            { enableMathSupport = enableMathSupport
                                            , preview = False
                                            , tabbable = tabbable
                                            , showSilcrow = True
                                            }
                                        )
                                        glossaryItem.terms
                                        ++ (if glossaryItem.needsUpdating then
                                                [ Html.dd
                                                    [ class "needs-updating" ]
                                                    [ span
                                                        []
                                                        [ text "Needs updating" ]
                                                    ]
                                                ]

                                            else
                                                []
                                           )
                                        ++ List.map
                                            (viewGlossaryItemDetails
                                                { enableMathSupport = enableMathSupport
                                                , makeLinksTabbable = makeLinksTabbable
                                                }
                                            )
                                            glossaryItem.details
                                        ++ viewGlossaryItemRelatedTerms
                                            enableMathSupport
                                            False
                                            tabbable
                                            itemHasSomeDetails
                                            Nothing
                                            glossaryItem.relatedTerms
                                    )
                                ]
                            , Extras.Html.showIf enableLastUpdatedDates <|
                                Extras.Html.showMaybe
                                    (\lastUpdatedDate ->
                                        div
                                            [ class "text-right text-sm mt-1.5 text-gray-500 dark:text-gray-400" ]
                                            [ text "Updated: "
                                            , Html.node "last-updated"
                                                [ Html.Attributes.attribute "datetime" lastUpdatedDate ]
                                                []
                                            ]
                                    )
                                    glossaryItem.lastUpdatedDate
                            ]
        )
        glossaryItemWithPreviousAndNext.item


viewAsSingle :
    { tabbable : Bool
    , enableMathSupport : Bool
    , makeLinksTabbable : Bool
    , enableLastUpdatedDates : Bool
    , onClickItem : GlossaryItemIndex -> msg
    , onClickRelatedTerm : RelatedTerm -> msg
    }
    -> GlossaryItemWithPreviousAndNext
    -> Html msg
viewAsSingle { tabbable, enableMathSupport, makeLinksTabbable, enableLastUpdatedDates, onClickItem, onClickRelatedTerm } glossaryItemWithPreviousAndNext =
    let
        primaryTermForPreviousOrNext glossaryItem =
            glossaryItem.terms
                |> List.take 1
                |> List.map
                    (\term ->
                        if Term.isAbbreviation term then
                            Html.abbr []
                                [ Term.view
                                    enableMathSupport
                                    [ class "text-sm" ]
                                    term
                                ]

                        else
                            Term.view enableMathSupport
                                [ class "text-sm" ]
                                term
                    )
    in
    Extras.Html.showMaybe
        (\( _, glossaryItem ) ->
            Html.div []
                [ Html.nav
                    [ class "flex items-start justify-between px-4 sm:px-0"
                    ]
                    [ Html.div
                        [ class "-mt-px flex w-0 flex-1"
                        ]
                        [ Extras.Html.showMaybe
                            (\( previousItemIndex, previousItem ) ->
                                Components.Button.text
                                    [ Accessibility.Key.tabbable tabbable
                                    , Html.Events.onClick <| onClickItem previousItemIndex
                                    ]
                                    [ Icons.arrowLongLeft
                                        [ Svg.Attributes.class "h-5 w-5" ]
                                    , span
                                        [ class "font-medium" ]
                                        (primaryTermForPreviousOrNext previousItem)
                                    ]
                            )
                            glossaryItemWithPreviousAndNext.previous
                        ]
                    , Html.div
                        [ class "hidden md:-mt-px md:flex md:flex-col px-3" ]
                        (List.map
                            (viewGlossaryTerm
                                { enableMathSupport = enableMathSupport
                                , preview = False
                                , tabbable = tabbable
                                , showSilcrow = False
                                }
                            )
                            glossaryItem.terms
                        )
                    , Html.div
                        [ class "-mt-px flex w-0 flex-1 justify-end" ]
                        [ Extras.Html.showMaybe
                            (\( nextItemIndex, nextItem ) ->
                                Components.Button.text
                                    [ Accessibility.Key.tabbable tabbable
                                    , Html.Events.onClick <| onClickItem nextItemIndex
                                    ]
                                    [ span
                                        [ class "font-medium" ]
                                        (primaryTermForPreviousOrNext nextItem)
                                    , Icons.arrowLongRight
                                        [ Svg.Attributes.class "h-5 w-5" ]
                                    ]
                            )
                            glossaryItemWithPreviousAndNext.next
                        ]
                    ]
                , Html.div
                    [ class "md:hidden mt-4" ]
                    (List.map
                        (viewGlossaryTerm
                            { enableMathSupport = enableMathSupport
                            , preview = False
                            , tabbable = tabbable
                            , showSilcrow = False
                            }
                        )
                        glossaryItem.terms
                    )
                , Html.div
                    [ class "mt-4" ]
                    ((if glossaryItem.needsUpdating then
                        [ Html.dd
                            [ class "needs-updating" ]
                            [ span
                                []
                                [ text "Needs updating" ]
                            ]
                        ]

                      else
                        []
                     )
                        ++ List.map
                            (viewGlossaryItemDetails
                                { enableMathSupport = enableMathSupport
                                , makeLinksTabbable = makeLinksTabbable
                                }
                            )
                            glossaryItem.details
                        ++ viewGlossaryItemRelatedTerms
                            enableMathSupport
                            False
                            tabbable
                            (GlossaryItem.hasSomeDetails glossaryItem)
                            (Just onClickRelatedTerm)
                            glossaryItem.relatedTerms
                    )
                , div
                    [ class "print:hidden mt-3 flex flex-col flex-grow justify-end" ]
                    [ Extras.Html.showIf enableLastUpdatedDates <|
                        Extras.Html.showMaybe
                            (\lastUpdatedDate ->
                                div
                                    [ class "text-right text-sm mt-1.5 mb-2.5 text-gray-500 dark:text-gray-400" ]
                                    [ text "Updated: "
                                    , Html.node "last-updated"
                                        [ Html.Attributes.attribute "datetime" lastUpdatedDate ]
                                        []
                                    ]
                            )
                            glossaryItem.lastUpdatedDate
                    ]
                ]
        )
        glossaryItemWithPreviousAndNext.item


errorDiv : String -> Html msg
errorDiv message =
    div
        [ class "flex justify-end mt-2" ]
        [ p
            [ class "text-red-600" ]
            [ text message ]
        ]


viewGlossaryTerm : { enableMathSupport : Bool, preview : Bool, tabbable : Bool, showSilcrow : Bool } -> Term -> Html msg
viewGlossaryTerm { enableMathSupport, preview, tabbable, showSilcrow } term =
    div
        [ class "flex justify-between" ]
        [ Html.dt
            [ class "group" ]
            [ span [ class "mr-1.5 hidden print:inline" ] [ text "➢" ]
            , Html.dfn
                [ Html.Attributes.id <| Term.id term ]
                [ if Term.isAbbreviation term then
                    Html.abbr [] [ Term.view enableMathSupport [] term ]

                  else
                    Term.view enableMathSupport [] term
                ]
            , Extras.Html.showIf showSilcrow <|
                span
                    [ class "silcrow invisible group-hover:visible hover:visible print:group-hover:invisible print:hover:invisible" ]
                    [ Html.a
                        [ term |> Term.id |> fragmentOnly |> Html.Attributes.href
                        , Accessibility.Key.tabbable tabbable
                        ]
                        [ text "§" ]
                    ]
            ]
        ]


viewGlossaryItemDetails : { enableMathSupport : Bool, makeLinksTabbable : Bool } -> Details.Details -> Html msg
viewGlossaryItemDetails { enableMathSupport, makeLinksTabbable } details =
    Html.dd
        []
        [ Details.view { enableMathSupport = enableMathSupport, makeLinksTabbable = makeLinksTabbable } details ]


viewGlossaryItemRelatedTerms : Bool -> Bool -> Bool -> Bool -> Maybe (RelatedTerm -> msg) -> List RelatedTerm -> List (Html msg)
viewGlossaryItemRelatedTerms enableMathSupport preview tabbable itemHasSomeDetails onClick relatedTerms =
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
                                    [ (if preview then
                                        "#"

                                       else
                                        fragmentOnly (RelatedTerm.idReference relatedTerm)
                                      )
                                        |> Html.Attributes.href
                                    , Accessibility.Key.tabbable tabbable
                                    , Extras.HtmlAttribute.showMaybe
                                        (\onClick1 ->
                                            Extras.HtmlEvents.onClickPreventDefaultAndStopPropagation <| onClick1 relatedTerm
                                        )
                                        onClick
                                    ]
                                    [ RelatedTerm.view enableMathSupport relatedTerm ]
                            )
                        |> List.intersperse (text ", ")
                   )
            )
        ]
