module Components.GlossaryItemCard exposing (Style(..), view)

import Accessibility exposing (Html, div, span, text)
import Accessibility.Aria
import Accessibility.Key
import Components.Button
import Data.FeatureFlag exposing (enableTagsFeature)
import Data.GlossaryItem as GlossaryItem
import Data.GlossaryItem.Definition as Definition exposing (Definition)
import Data.GlossaryItem.RelatedTerm as RelatedTerm exposing (RelatedTerm)
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItem.TermId as TermId
import Data.GlossaryItemIndex exposing (GlossaryItemIndex)
import Data.GlossaryItemWithPreviousAndNext exposing (GlossaryItemWithPreviousAndNext)
import ElementIds
import Extras.Html
import Extras.HtmlAttribute
import Extras.HtmlEvents
import Extras.Url exposing (fragmentOnly)
import Html
import Html.Attributes exposing (class, id)
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
            let
                preferredTerm : Term
                preferredTerm =
                    GlossaryItem.preferredTerm glossaryItem

                alternativeTerms : List Term
                alternativeTerms =
                    GlossaryItem.alternativeTerms glossaryItem

                itemHasSomeDefinitions : Bool
                itemHasSomeDefinitions =
                    GlossaryItem.hasADefinition glossaryItem

                definitions =
                    GlossaryItem.definition glossaryItem
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []

                relatedTerms =
                    GlossaryItem.relatedPreferredTerms glossaryItem

                needsUpdating =
                    GlossaryItem.needsUpdating glossaryItem

                lastUpdatedDate =
                    GlossaryItem.lastUpdatedDate glossaryItem
            in
            case style of
                Preview ->
                    let
                        tabbable : Bool
                        tabbable =
                            True
                    in
                    div
                        [ Html.Attributes.style "max-height" "100%" ]
                        (viewGlossaryTerm
                            { enableMathSupport = enableMathSupport
                            , tabbable = tabbable
                            , showSilcrow = False
                            , isPreferred = True
                            }
                            preferredTerm
                            :: List.map
                                (viewGlossaryTerm
                                    { enableMathSupport = enableMathSupport
                                    , tabbable = tabbable
                                    , showSilcrow = False
                                    , isPreferred = False
                                    }
                                )
                                alternativeTerms
                            ++ (if GlossaryItem.needsUpdating glossaryItem then
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
                                (viewGlossaryItemDefinition
                                    { enableMathSupport = enableMathSupport
                                    , tabbable = makeLinksTabbable
                                    , tagsClickable = False
                                    }
                                )
                                definitions
                            ++ viewGlossaryItemRelatedTerms
                                enableMathSupport
                                True
                                tabbable
                                itemHasSomeDefinitions
                                Nothing
                                relatedTerms
                        )

                Normal { tabbable, onClickViewFull, onClickEdit, onClickDelete, onClickItem, onClickRelatedTerm, editable, shownAsSingle } ->
                    if shownAsSingle then
                        div
                            [ Html.Attributes.style "max-height" "100%"
                            , Html.Attributes.style "border-width" "0px"
                            , class "mt-1.5"
                            ]
                            [ viewAsSingle
                                { enableMathSupport = enableMathSupport
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
                                [ Extras.Html.showIf (not shownAsSingle) <|
                                    div
                                        [ class "print:hidden hidden lg:block float-right sticky top-0 bg-white dark:bg-gray-800 bg-opacity-75 dark:bg-opacity-75 p-0.5 rounded-full" ]
                                        [ span
                                            []
                                            [ Components.Button.text
                                                [ Accessibility.Key.tabbable tabbable
                                                , Accessibility.Aria.label "View as single item"
                                                , Html.Attributes.title "View as single item"
                                                , Html.Events.onClick onClickViewFull
                                                ]
                                                [ Icons.maximize2
                                                    [ Svg.Attributes.class "h-5 w-5 text-gray-400 dark:text-gray-300 hover:text-gray-500 dark:hover:text-gray-400" ]
                                                ]
                                            ]
                                        ]
                                , div
                                    []
                                    (viewGlossaryTerm
                                        { enableMathSupport = enableMathSupport
                                        , tabbable = tabbable
                                        , showSilcrow = True
                                        , isPreferred = True
                                        }
                                        preferredTerm
                                        :: List.map
                                            (viewGlossaryTerm
                                                { enableMathSupport = enableMathSupport
                                                , tabbable = tabbable
                                                , showSilcrow = False
                                                , isPreferred = False
                                                }
                                            )
                                            alternativeTerms
                                        ++ (if needsUpdating then
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
                                            (viewGlossaryItemDefinition
                                                { enableMathSupport = enableMathSupport
                                                , tabbable = makeLinksTabbable
                                                , tagsClickable = makeLinksTabbable
                                                }
                                            )
                                            definitions
                                        ++ viewGlossaryItemRelatedTerms enableMathSupport False tabbable itemHasSomeDefinitions Nothing relatedTerms
                                    )
                                ]
                            , div
                                [ class "print:hidden mt-3 flex flex-col flex-grow justify-end" ]
                                [ Extras.Html.showIf enableLastUpdatedDates <|
                                    Extras.Html.showMaybe
                                        (\lastUpdatedDate_ ->
                                            div
                                                [ class "text-right text-sm mt-1.5 mb-2.5 text-gray-500 dark:text-gray-400" ]
                                                [ text "Updated: "
                                                , Html.node "last-updated"
                                                    [ Html.Attributes.attribute "datetime" lastUpdatedDate_ ]
                                                    []
                                                ]
                                        )
                                        lastUpdatedDate
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
                                ]
                            ]

                    else
                        div
                            [ class "flex flex-col justify-between"
                            , id <| ElementIds.glossaryItemDiv index
                            ]
                            [ div
                                [ class "flex-1" ]
                                [ div
                                    [ class "print:hidden hidden lg:block float-right sticky top-0 bg-white dark:bg-gray-800 bg-opacity-75 dark:bg-opacity-75 p-0.5 rounded-full" ]
                                    [ span
                                        []
                                        [ Components.Button.text
                                            [ Accessibility.Key.tabbable tabbable
                                            , Accessibility.Aria.label "View as single item"
                                            , Html.Attributes.title "View as single item"
                                            , Html.Events.onClick onClickViewFull
                                            ]
                                            [ Icons.maximize2
                                                [ Svg.Attributes.class "h-5 w-5 text-gray-400 dark:text-gray-300 hover:text-gray-500 dark:hover:text-gray-400" ]
                                            ]
                                        ]
                                    ]
                                , div []
                                    (viewGlossaryTerm
                                        { enableMathSupport = enableMathSupport
                                        , tabbable = tabbable
                                        , showSilcrow = True
                                        , isPreferred = True
                                        }
                                        preferredTerm
                                        :: List.map
                                            (viewGlossaryTerm
                                                { enableMathSupport = enableMathSupport
                                                , tabbable = tabbable
                                                , showSilcrow = False
                                                , isPreferred = False
                                                }
                                            )
                                            alternativeTerms
                                        ++ (if needsUpdating then
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
                                            (viewGlossaryItemDefinition
                                                { enableMathSupport = enableMathSupport
                                                , tabbable = makeLinksTabbable
                                                , tagsClickable = makeLinksTabbable
                                                }
                                            )
                                            definitions
                                        ++ viewGlossaryItemRelatedTerms
                                            enableMathSupport
                                            False
                                            tabbable
                                            itemHasSomeDefinitions
                                            Nothing
                                            relatedTerms
                                    )
                                ]
                            , Extras.Html.showIf enableLastUpdatedDates <|
                                Extras.Html.showMaybe
                                    (\lastUpdatedDate_ ->
                                        div
                                            [ class "text-right text-sm mt-1.5 text-gray-500 dark:text-gray-400" ]
                                            [ text "Updated: "
                                            , Html.node "last-updated"
                                                [ Html.Attributes.attribute "datetime" lastUpdatedDate_ ]
                                                []
                                            ]
                                    )
                                    lastUpdatedDate
                            ]
        )
        glossaryItemWithPreviousAndNext.item


viewAsSingle :
    { enableMathSupport : Bool
    , makeLinksTabbable : Bool
    , enableLastUpdatedDates : Bool
    , onClickItem : GlossaryItemIndex -> msg
    , onClickRelatedTerm : RelatedTerm -> msg
    }
    -> GlossaryItemWithPreviousAndNext
    -> Html msg
viewAsSingle { enableMathSupport, enableLastUpdatedDates, onClickItem, onClickRelatedTerm } glossaryItemWithPreviousAndNext =
    let
        preferredTermForPreviousOrNext : GlossaryItem.GlossaryItem -> Html msg
        preferredTermForPreviousOrNext glossaryItem =
            let
                preferredTerm =
                    GlossaryItem.preferredTerm glossaryItem
            in
            if Term.isAbbreviation preferredTerm then
                Html.abbr []
                    [ Term.view
                        enableMathSupport
                        [ class "text-sm" ]
                        preferredTerm
                    ]

            else
                Term.view enableMathSupport
                    [ class "text-sm" ]
                    preferredTerm
    in
    Extras.Html.showMaybe
        (\( _, glossaryItem ) ->
            let
                preferredTerm =
                    GlossaryItem.preferredTerm glossaryItem

                alternativeTerms =
                    GlossaryItem.alternativeTerms glossaryItem

                definitions =
                    GlossaryItem.definition glossaryItem
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []

                relatedTerms =
                    GlossaryItem.relatedPreferredTerms glossaryItem

                needsUpdating =
                    GlossaryItem.needsUpdating glossaryItem

                lastUpdatedDate =
                    GlossaryItem.lastUpdatedDate glossaryItem
            in
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
                                    [ Html.Events.onClick <| onClickItem previousItemIndex ]
                                    [ Icons.arrowLongLeft
                                        [ Svg.Attributes.class "h-5 w-5" ]
                                    , span
                                        [ class "font-medium" ]
                                        [ preferredTermForPreviousOrNext previousItem ]
                                    ]
                            )
                            glossaryItemWithPreviousAndNext.previous
                        ]
                    , Html.div
                        [ class "hidden md:-mt-px md:flex md:flex-col px-3" ]
                        (viewGlossaryTerm
                            { enableMathSupport = enableMathSupport
                            , tabbable = True
                            , showSilcrow = False
                            , isPreferred = True
                            }
                            preferredTerm
                            :: List.map
                                (viewGlossaryTerm
                                    { enableMathSupport = enableMathSupport
                                    , tabbable = True
                                    , showSilcrow = False
                                    , isPreferred = False
                                    }
                                )
                                alternativeTerms
                        )
                    , Html.div
                        [ class "-mt-px flex w-0 flex-1 justify-end" ]
                        [ Extras.Html.showMaybe
                            (\( nextItemIndex, nextItem ) ->
                                Components.Button.text
                                    [ Html.Events.onClick <| onClickItem nextItemIndex ]
                                    [ span
                                        [ class "font-medium" ]
                                        [ preferredTermForPreviousOrNext nextItem ]
                                    , Icons.arrowLongRight
                                        [ Svg.Attributes.class "h-5 w-5" ]
                                    ]
                            )
                            glossaryItemWithPreviousAndNext.next
                        ]
                    ]
                , Html.div
                    [ class "md:hidden mt-4" ]
                    (viewGlossaryTerm
                        { enableMathSupport = enableMathSupport
                        , tabbable = True
                        , showSilcrow = False
                        , isPreferred = True
                        }
                        preferredTerm
                        :: List.map
                            (viewGlossaryTerm
                                { enableMathSupport = enableMathSupport
                                , tabbable = True
                                , showSilcrow = False
                                , isPreferred = False
                                }
                            )
                            alternativeTerms
                    )
                , Html.div
                    [ class "mt-4" ]
                    ((if needsUpdating then
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
                            (viewGlossaryItemDefinition
                                { enableMathSupport = enableMathSupport
                                , tabbable = True
                                , tagsClickable = False
                                }
                            )
                            definitions
                        ++ viewGlossaryItemRelatedTerms
                            enableMathSupport
                            False
                            True
                            (GlossaryItem.hasADefinition glossaryItem)
                            (Just onClickRelatedTerm)
                            relatedTerms
                    )
                , div
                    [ class "print:hidden mt-3 flex flex-col flex-grow justify-end" ]
                    [ Extras.Html.showIf enableLastUpdatedDates <|
                        Extras.Html.showMaybe
                            (\lastUpdatedDate_ ->
                                div
                                    [ class "text-right text-sm mt-1.5 mb-2.5 text-gray-500 dark:text-gray-400" ]
                                    [ text "Updated: "
                                    , Html.node "last-updated"
                                        [ Html.Attributes.attribute "datetime" lastUpdatedDate_ ]
                                        []
                                    ]
                            )
                            lastUpdatedDate
                    ]
                ]
        )
        glossaryItemWithPreviousAndNext.item


viewGlossaryTerm :
    { enableMathSupport : Bool, tabbable : Bool, showSilcrow : Bool, isPreferred : Bool }
    -> Term
    -> Html msg
viewGlossaryTerm { enableMathSupport, tabbable, showSilcrow, isPreferred } term =
    let
        viewTerm =
            if isPreferred then
                Term.view enableMathSupport [] term

            else
                span
                    [ class "inline-flex items-center" ]
                    [ Icons.cornerLeftUp
                        [ Svg.Attributes.class "h-5 w-5 shrink-0 pb-1 mr-1.5 text-gray-400 dark:text-gray-400"
                        ]
                    , Term.view enableMathSupport [] term
                    ]
    in
    div
        [ class "flex justify-between" ]
        [ Html.dt
            [ class "group" ]
            [ span [ class "mr-1.5 hidden print:inline" ] [ text "➢" ]
            , Html.dfn
                [ Extras.HtmlAttribute.showIf showSilcrow <| Html.Attributes.id <| TermId.toString <| Term.id term ]
                [ if Term.isAbbreviation term then
                    Html.abbr []
                        [ viewTerm ]

                  else
                    viewTerm
                ]
            , Extras.Html.showIf showSilcrow <|
                span
                    [ class "silcrow invisible group-hover:visible hover:visible print:group-hover:invisible print:hover:invisible" ]
                    [ Html.a
                        [ term |> Term.id |> TermId.toString |> fragmentOnly |> Html.Attributes.href
                        , Accessibility.Key.tabbable tabbable
                        ]
                        [ text "§" ]
                    ]
            ]
        ]


viewGlossaryItemDefinition : { enableMathSupport : Bool, tabbable : Bool, tagsClickable : Bool } -> Definition -> Html msg
viewGlossaryItemDefinition { enableMathSupport, tabbable, tagsClickable } definition =
    Html.dd
        []
        [ Extras.Html.showIf enableTagsFeature <|
            Html.div
                [ class "mb-4" ]
                [ Components.Button.softSmall
                    tagsClickable
                    [ class "mr-2 mb-2"
                    , Html.Attributes.title "Tag: First Tag"
                    ]
                    [ text "First Tag" ]
                ]
        , Definition.view { enableMathSupport = enableMathSupport, makeLinksTabbable = tabbable } definition
        ]


viewGlossaryItemRelatedTerms : Bool -> Bool -> Bool -> Bool -> Maybe (RelatedTerm -> msg) -> List RelatedTerm -> List (Html msg)
viewGlossaryItemRelatedTerms enableMathSupport preview tabbable itemHasSomeDefinitions onClick relatedTerms =
    if List.isEmpty relatedTerms then
        []

    else
        [ Html.dd
            [ class "related-terms" ]
            (text
                (if itemHasSomeDefinitions then
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
                                        relatedTerm |> RelatedTerm.idReference |> TermId.toString |> fragmentOnly
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
