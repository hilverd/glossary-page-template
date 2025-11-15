module Components.GlossaryItemCard exposing (Style(..), view)

import Accessibility exposing (Html, div, span, text)
import Accessibility.Aria
import Components.Button
import Data.GlossaryItem.Definition as Definition exposing (Definition)
import Data.GlossaryItem.DisambiguatedTerm as DisambiguatedTerm exposing (DisambiguatedTerm)
import Data.GlossaryItem.Tag as Tag exposing (Tag)
import Data.GlossaryItem.Term as Term exposing (TagIconAppearance(..), Term)
import Data.GlossaryItemForUi as GlossaryItemForUi exposing (GlossaryItemForUi, isItemForTag)
import Data.GlossaryItemId exposing (GlossaryItemId)
import Data.GlossaryItemWithPreviousAndNext exposing (GlossaryItemWithPreviousAndNext)
import ElementIds
import Extras.Html
import Extras.HtmlAttribute
import Extras.HtmlEvents
import Extras.Url exposing (fragmentOnly)
import Html
import Html.Attributes exposing (class, id, target)
import Html.Events
import Icons
import Internationalisation as I18n
import Svg.Attributes


type Style msg
    = Preview
    | Normal
        { onClickViewFull : msg
        , onClickCopyToClipboard : msg
        , onClickEdit : msg
        , onClickDelete : msg
        , onClickItem : GlossaryItemId -> msg
        , onClickTag : Tag -> msg
        , onClickRelatedTerm : Term -> msg
        , resultOfAttemptingToCopyItemTextToClipboard : Maybe Bool
        , editable : Bool
        , shownAsSingle : Bool
        }


view :
    { enableMathSupport : Bool
    , enableLastUpdatedDates : Bool
    }
    -> Style msg
    -> Maybe Tag
    -> Maybe GlossaryItemId
    -> GlossaryItemWithPreviousAndNext
    -> Html msg
view { enableMathSupport, enableLastUpdatedDates } style tagBeingFilteredBy itemWithFocus glossaryItemWithPreviousAndNext =
    Extras.Html.showMaybe
        (\glossaryItem ->
            let
                disambiguatedPreferredTerm : Term
                disambiguatedPreferredTerm =
                    glossaryItem
                        |> GlossaryItemForUi.disambiguatedPreferredTerm
                        |> DisambiguatedTerm.toTerm

                alternativeTerms : List Term
                alternativeTerms =
                    GlossaryItemForUi.alternativeTerms glossaryItem

                tags : List Tag
                tags =
                    GlossaryItemForUi.allTags glossaryItem

                itemHasADefinition : Bool
                itemHasADefinition =
                    GlossaryItemForUi.definition glossaryItem /= Nothing

                definition : Maybe Definition
                definition =
                    GlossaryItemForUi.definition glossaryItem

                relatedTerms : List DisambiguatedTerm
                relatedTerms =
                    GlossaryItemForUi.relatedPreferredTerms glossaryItem

                needsUpdating : Bool
                needsUpdating =
                    GlossaryItemForUi.needsUpdating glossaryItem

                isItemForTag : Bool
                isItemForTag =
                    GlossaryItemForUi.isItemForTag glossaryItem

                tagIconAppearance : TagIconAppearance
                tagIconAppearance =
                    if isItemForTag then
                        NormalTagIcon

                    else
                        NoTagIcon
            in
            case style of
                Preview ->
                    div
                        [ Html.Attributes.style "max-height" "100%"
                        , class "max-w-[69ch] overflow-x-clip"
                        ]
                        (viewGlossaryTerm
                            { enableMathSupport = enableMathSupport
                            , showSilcrow = False
                            , isPreferred = True
                            , tagIconAppearance = tagIconAppearance
                            }
                            disambiguatedPreferredTerm
                            :: List.map
                                (viewGlossaryTerm
                                    { enableMathSupport = enableMathSupport
                                    , showSilcrow = False
                                    , isPreferred = False
                                    , tagIconAppearance = tagIconAppearance
                                    }
                                )
                                alternativeTerms
                            ++ (if needsUpdating then
                                    [ Html.dd
                                        [ class "needs-updating" ]
                                        [ span
                                            []
                                            [ text I18n.needsUpdating ]
                                        ]
                                    ]

                                else
                                    []
                               )
                            ++ [ viewTags
                                    { enableMathSupport = enableMathSupport
                                    , onClickTag = Nothing
                                    }
                                    tags
                               , definition
                                    |> Extras.Html.showMaybe
                                        (viewGlossaryItemDefinition
                                            { enableMathSupport = enableMathSupport
                                            }
                                        )
                               ]
                            ++ viewGlossaryItemRelatedTerms
                                enableMathSupport
                                True
                                itemHasADefinition
                                Nothing
                                relatedTerms
                        )

                Normal { onClickViewFull, onClickCopyToClipboard, onClickEdit, onClickDelete, onClickTag, onClickItem, onClickRelatedTerm, resultOfAttemptingToCopyItemTextToClipboard, editable, shownAsSingle } ->
                    if shownAsSingle then
                        div
                            [ Html.Attributes.style "max-height" "100%"
                            , Html.Attributes.style "border-width" "0px"
                            , class "mt-1.5"
                            ]
                            [ viewAsSingle
                                { enableMathSupport = enableMathSupport
                                , enableLastUpdatedDates = enableLastUpdatedDates
                                , onClickItem = onClickItem
                                , onClickCopyToClipboard = onClickCopyToClipboard
                                , onClickRelatedTerm = onClickRelatedTerm
                                , resultOfAttemptingToCopyItemTextToClipboard = resultOfAttemptingToCopyItemTextToClipboard
                                }
                                tagBeingFilteredBy
                                glossaryItemWithPreviousAndNext
                            ]

                    else
                        let
                            index : GlossaryItemId
                            index =
                                GlossaryItemForUi.id glossaryItem

                            hasFocus : Bool
                            hasFocus =
                                itemWithFocus == Just index

                            tagsNotBeingFilteredBy : List Tag
                            tagsNotBeingFilteredBy =
                                tags |> List.filter (Just >> (/=) tagBeingFilteredBy)

                            lastUpdatedDate : Maybe String
                            lastUpdatedDate =
                                GlossaryItemForUi.lastUpdatedDateAsIso8601 glossaryItem

                            lastUpdatedByName : Maybe String
                            lastUpdatedByName =
                                GlossaryItemForUi.lastUpdatedByName glossaryItem

                            lastUpdatedByEmailAddress : Maybe String
                            lastUpdatedByEmailAddress =
                                GlossaryItemForUi.lastUpdatedByEmailAddress glossaryItem
                        in
                        if editable then
                            div
                                [ class "flex flex-col justify-items-end"
                                , Extras.HtmlAttribute.showIf hasFocus <| class "print:outline-hidden outline-offset-2 outline-4 outline-dashed outline-yellow-500 dark:outline-pink-900"
                                , id <| ElementIds.glossaryItemDiv index
                                ]
                                [ div
                                    []
                                    [ div
                                        [ class "print:hidden hidden lg:block float-right sticky top-0 bg-white/75 dark:bg-gray-800/75 p-0.5 rounded-full" ]
                                        [ span
                                            []
                                            [ Components.Button.text
                                                [ Accessibility.Aria.label I18n.viewAsSingleItem
                                                , Html.Attributes.title I18n.viewAsSingleItem
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
                                            , showSilcrow = True
                                            , isPreferred = True
                                            , tagIconAppearance = tagIconAppearance
                                            }
                                            disambiguatedPreferredTerm
                                            :: List.map
                                                (viewGlossaryTerm
                                                    { enableMathSupport = enableMathSupport
                                                    , showSilcrow = False
                                                    , isPreferred = False
                                                    , tagIconAppearance = tagIconAppearance
                                                    }
                                                )
                                                alternativeTerms
                                            ++ (if needsUpdating then
                                                    [ Html.dd
                                                        [ class "needs-updating" ]
                                                        [ span
                                                            []
                                                            [ text I18n.needsUpdating ]
                                                        ]
                                                    ]

                                                else
                                                    []
                                               )
                                            ++ [ viewTags
                                                    { enableMathSupport = enableMathSupport
                                                    , onClickTag = Just onClickTag
                                                    }
                                                    tagsNotBeingFilteredBy
                                               , definition
                                                    |> Extras.Html.showMaybe
                                                        (viewGlossaryItemDefinition
                                                            { enableMathSupport = enableMathSupport }
                                                        )
                                               ]
                                            ++ viewGlossaryItemRelatedTerms enableMathSupport False itemHasADefinition Nothing relatedTerms
                                        )
                                    ]
                                , div
                                    [ class "print:hidden mt-3 flex flex-col grow justify-end" ]
                                    [ Extras.Html.showIf enableLastUpdatedDates <|
                                        Extras.Html.showMaybe
                                            (I18n.updatedOn lastUpdatedByName lastUpdatedByEmailAddress)
                                            lastUpdatedDate
                                    , div
                                        [ class "flex justify-between" ]
                                        [ span
                                            [ class "inline-flex items-center" ]
                                            [ Components.Button.text
                                                [ Html.Events.onClick onClickEdit
                                                , class "group"
                                                ]
                                                [ Icons.pencil
                                                    [ Svg.Attributes.class "h-5 w-5 text-gray-400 dark:text-gray-300 group-hover:text-gray-500 dark:group-hover:text-gray-100" ]
                                                , span
                                                    [ class "font-medium text-gray-600 dark:text-gray-300 group-hover:text-gray-800 dark:group-hover:text-gray-100" ]
                                                    [ text I18n.edit ]
                                                ]
                                            ]
                                        , span
                                            [ class "ml-3 inline-flex items-center" ]
                                            [ Components.Button.text
                                                [ Html.Events.onClick onClickDelete
                                                , class "group"
                                                ]
                                                [ Icons.trash
                                                    [ Svg.Attributes.class "h-5 w-5 text-gray-400 dark:text-gray-300 group-hover:text-gray-500 dark:group-hover:text-gray-100" ]
                                                , span
                                                    [ class "font-medium text-gray-600 dark:text-gray-300 group-hover:text-gray-800 dark:group-hover:text-gray-100" ]
                                                    [ text I18n.delete ]
                                                ]
                                            ]
                                        ]
                                    ]
                                ]

                        else
                            div
                                [ class "flex flex-col justify-between overflow-x-clip"
                                , Extras.HtmlAttribute.showIf hasFocus <| class "print:outline-hidden outline-offset-2 outline-4 outline-dashed outline-yellow-500 dark:outline-pink-900"
                                , id <| ElementIds.glossaryItemDiv index
                                ]
                                [ div
                                    [ class "flex-1" ]
                                    [ div
                                        [ class "print:hidden hidden lg:block float-right sticky top-0 bg-white/75 dark:bg-gray-800/75 p-0.5 rounded-full" ]
                                        [ span
                                            []
                                            [ Components.Button.text
                                                [ Accessibility.Aria.label I18n.viewAsSingleItem
                                                , Html.Attributes.title I18n.viewAsSingleItem
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
                                            , showSilcrow = True
                                            , isPreferred = True
                                            , tagIconAppearance = tagIconAppearance
                                            }
                                            disambiguatedPreferredTerm
                                            :: List.map
                                                (viewGlossaryTerm
                                                    { enableMathSupport = enableMathSupport
                                                    , showSilcrow = False
                                                    , isPreferred = False
                                                    , tagIconAppearance = tagIconAppearance
                                                    }
                                                )
                                                alternativeTerms
                                            ++ (if needsUpdating then
                                                    [ Html.dd
                                                        [ class "needs-updating" ]
                                                        [ span
                                                            []
                                                            [ text I18n.needsUpdating ]
                                                        ]
                                                    ]

                                                else
                                                    []
                                               )
                                            ++ [ viewTags
                                                    { enableMathSupport = enableMathSupport
                                                    , onClickTag = Just onClickTag
                                                    }
                                                    tagsNotBeingFilteredBy
                                               , definition
                                                    |> Extras.Html.showMaybe
                                                        (viewGlossaryItemDefinition
                                                            { enableMathSupport = enableMathSupport }
                                                        )
                                               ]
                                            ++ viewGlossaryItemRelatedTerms
                                                enableMathSupport
                                                False
                                                itemHasADefinition
                                                Nothing
                                                relatedTerms
                                        )
                                    ]
                                , div
                                    [ class "flex justify-between items-center w-full" ]
                                    [ Components.Button.text
                                        [ class "print:hidden"
                                        , Extras.HtmlAttribute.showIf (resultOfAttemptingToCopyItemTextToClipboard == Nothing) <|
                                            Html.Events.onClick onClickCopyToClipboard
                                        , Html.Attributes.title I18n.copyToClipboard
                                        , Accessibility.Aria.label I18n.copyToClipboard
                                        ]
                                        [ if resultOfAttemptingToCopyItemTextToClipboard == Just True then
                                            Icons.tick
                                                [ Svg.Attributes.class "h-5 w-5 text-green-700 dark:text-green-300" ]

                                          else
                                            Icons.copy
                                                [ Svg.Attributes.class "h-5 w-5 text-gray-400 dark:text-gray-300 hover:text-gray-500 dark:hover:text-gray-400" ]
                                        ]
                                    , Extras.Html.showIf enableLastUpdatedDates <|
                                        Extras.Html.showMaybe
                                            (I18n.updatedOn lastUpdatedByName lastUpdatedByEmailAddress)
                                            lastUpdatedDate
                                    ]
                                ]
        )
        glossaryItemWithPreviousAndNext.item


viewAsSingle :
    { enableMathSupport : Bool
    , enableLastUpdatedDates : Bool
    , onClickItem : GlossaryItemId -> msg
    , onClickCopyToClipboard : msg
    , onClickRelatedTerm : Term -> msg
    , resultOfAttemptingToCopyItemTextToClipboard : Maybe Bool
    }
    -> Maybe Tag
    -> GlossaryItemWithPreviousAndNext
    -> Html msg
viewAsSingle { enableMathSupport, enableLastUpdatedDates, onClickItem, onClickCopyToClipboard, onClickRelatedTerm, resultOfAttemptingToCopyItemTextToClipboard } tagBeingFilteredBy glossaryItemWithPreviousAndNext =
    let
        disambiguatedPreferredTermForPreviousOrNext : GlossaryItemForUi -> Html msg
        disambiguatedPreferredTermForPreviousOrNext glossaryItem =
            let
                preferredTerm : Term
                preferredTerm =
                    glossaryItem
                        |> GlossaryItemForUi.disambiguatedPreferredTerm
                        |> DisambiguatedTerm.toTerm

                isItemForTag : Bool
                isItemForTag =
                    GlossaryItemForUi.isItemForTag glossaryItem

                tagIconAppearance =
                    if isItemForTag then
                        NormalTagIcon

                    else
                        NoTagIcon
            in
            if Term.isAbbreviation preferredTerm then
                Html.abbr []
                    [ Term.view enableMathSupport
                        tagIconAppearance
                        [ class "text-sm" ]
                        preferredTerm
                    ]

            else
                Term.view enableMathSupport
                    tagIconAppearance
                    [ class "text-sm" ]
                    preferredTerm
    in
    Extras.Html.showMaybe
        (\glossaryItem ->
            let
                disambiguatedPreferredTerm : Term
                disambiguatedPreferredTerm =
                    glossaryItem
                        |> GlossaryItemForUi.disambiguatedPreferredTerm
                        |> DisambiguatedTerm.toTerm

                alternativeTerms : List Term
                alternativeTerms =
                    GlossaryItemForUi.alternativeTerms glossaryItem

                tagsNotBeingFilteredBy : List Tag
                tagsNotBeingFilteredBy =
                    glossaryItem
                        |> GlossaryItemForUi.allTags
                        |> List.filter (Just >> (/=) tagBeingFilteredBy)

                definition : Maybe Definition
                definition =
                    GlossaryItemForUi.definition glossaryItem

                relatedTerms : List DisambiguatedTerm
                relatedTerms =
                    GlossaryItemForUi.relatedPreferredTerms glossaryItem

                needsUpdating : Bool
                needsUpdating =
                    GlossaryItemForUi.needsUpdating glossaryItem

                lastUpdatedDate : Maybe String
                lastUpdatedDate =
                    GlossaryItemForUi.lastUpdatedDateAsIso8601 glossaryItem

                lastUpdatedByName : Maybe String
                lastUpdatedByName =
                    GlossaryItemForUi.lastUpdatedByName glossaryItem

                lastUpdatedByEmailAddress : Maybe String
                lastUpdatedByEmailAddress =
                    GlossaryItemForUi.lastUpdatedByEmailAddress glossaryItem

                isItemForTag : Bool
                isItemForTag =
                    GlossaryItemForUi.isItemForTag glossaryItem

                tagIconAppearance : TagIconAppearance
                tagIconAppearance =
                    if isItemForTag then
                        NormalTagIcon

                    else
                        NoTagIcon
            in
            Html.div []
                [ Html.nav
                    [ class "flex items-start justify-between px-4 sm:px-0"
                    ]
                    [ Html.div
                        [ class "-mt-px flex w-0 flex-1"
                        ]
                        [ Extras.Html.showMaybe
                            (\previousItem ->
                                Components.Button.textWrapNormal
                                    [ Html.Events.onClick <| onClickItem <| GlossaryItemForUi.id previousItem
                                    , class "cursor-pointer"
                                    ]
                                    [ Icons.arrowLongLeft
                                        [ Svg.Attributes.class "h-5 w-5 shrink-0" ]
                                    , span
                                        [ class "font-medium" ]
                                        [ disambiguatedPreferredTermForPreviousOrNext previousItem ]
                                    ]
                            )
                            glossaryItemWithPreviousAndNext.previous
                        ]
                    , Html.div
                        [ class "hidden md:-mt-px md:flex max-w-xs md:flex-col px-3" ]
                        (viewGlossaryTerm
                            { enableMathSupport = enableMathSupport
                            , showSilcrow = False
                            , isPreferred = True
                            , tagIconAppearance = tagIconAppearance
                            }
                            disambiguatedPreferredTerm
                            :: List.map
                                (viewGlossaryTerm
                                    { enableMathSupport = enableMathSupport
                                    , showSilcrow = False
                                    , isPreferred = False
                                    , tagIconAppearance = tagIconAppearance
                                    }
                                )
                                alternativeTerms
                        )
                    , Html.div
                        [ class "-mt-px flex w-0 flex-1 justify-end" ]
                        [ Extras.Html.showMaybe
                            (\nextItem ->
                                Components.Button.textWrapNormal
                                    [ Html.Events.onClick <| onClickItem <| GlossaryItemForUi.id nextItem
                                    , class "cursor-pointer"
                                    ]
                                    [ span
                                        [ class "font-medium" ]
                                        [ disambiguatedPreferredTermForPreviousOrNext nextItem ]
                                    , Icons.arrowLongRight
                                        [ Svg.Attributes.class "h-5 w-5 shrink-0" ]
                                    ]
                            )
                            glossaryItemWithPreviousAndNext.next
                        ]
                    ]
                , Html.div
                    [ class "md:hidden mt-4" ]
                    (viewGlossaryTerm
                        { enableMathSupport = enableMathSupport
                        , showSilcrow = False
                        , isPreferred = True
                        , tagIconAppearance = tagIconAppearance
                        }
                        disambiguatedPreferredTerm
                        :: List.map
                            (viewGlossaryTerm
                                { enableMathSupport = enableMathSupport
                                , showSilcrow = False
                                , isPreferred = False
                                , tagIconAppearance = tagIconAppearance
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
                                [ text I18n.needsUpdating ]
                            ]
                        ]

                      else
                        []
                     )
                        ++ [ viewTags
                                { enableMathSupport = enableMathSupport
                                , onClickTag = Nothing
                                }
                                tagsNotBeingFilteredBy
                           , Extras.Html.showMaybe
                                (viewGlossaryItemDefinition
                                    { enableMathSupport = enableMathSupport }
                                )
                                definition
                           ]
                        ++ viewGlossaryItemRelatedTerms
                            enableMathSupport
                            False
                            (GlossaryItemForUi.definition glossaryItem /= Nothing)
                            (Just onClickRelatedTerm)
                            relatedTerms
                    )
                , div
                    [ class "flex justify-between items-center w-full" ]
                    [ Components.Button.text
                        [ class "print:hidden"
                        , Extras.HtmlAttribute.showIf (resultOfAttemptingToCopyItemTextToClipboard == Nothing) <|
                            Html.Events.onClick onClickCopyToClipboard
                        , Html.Attributes.title I18n.copyToClipboard
                        , Accessibility.Aria.label I18n.copyToClipboard
                        ]
                        [ if resultOfAttemptingToCopyItemTextToClipboard == Just True then
                            Icons.tick
                                [ Svg.Attributes.class "h-5 w-5 text-green-700 dark:text-green-300" ]

                          else
                            Icons.copy
                                [ Svg.Attributes.class "h-5 w-5 text-gray-400 dark:text-gray-300 hover:text-gray-500 dark:hover:text-gray-400" ]
                        ]
                    , div
                        [ class "print:hidden mt-3 flex flex-col grow justify-end" ]
                        [ Extras.Html.showIf enableLastUpdatedDates <|
                            Extras.Html.showMaybe
                                (I18n.updatedOn lastUpdatedByName lastUpdatedByEmailAddress)
                                lastUpdatedDate
                        ]
                    ]
                ]
        )
        glossaryItemWithPreviousAndNext.item


viewGlossaryTerm :
    { enableMathSupport : Bool
    , showSilcrow : Bool
    , isPreferred : Bool
    , tagIconAppearance : TagIconAppearance
    }
    -> Term
    -> Html msg
viewGlossaryTerm { enableMathSupport, showSilcrow, isPreferred, tagIconAppearance } term =
    let
        viewTerm : Html msg
        viewTerm =
            if isPreferred then
                Term.view enableMathSupport tagIconAppearance [] term

            else
                span
                    [ class "inline-flex items-center" ]
                    [ Icons.cornerLeftUp
                        [ Svg.Attributes.class "h-5 w-5 shrink-0 pb-1 mr-1.5 text-gray-400 dark:text-gray-400 print:hidden"
                        ]
                    , Term.view enableMathSupport NoTagIcon [ class "font-normal" ] term
                    ]
    in
    div
        [ class "flex justify-between"
        , Extras.HtmlAttribute.showIf isPreferred <| class "mb-1"
        ]
        [ Html.dt
            [ class "group" ]
            [ span [ class "mr-1.5 hidden print:inline" ] [ text "➢" ]
            , Html.dfn
                [ Extras.HtmlAttribute.showIf showSilcrow <| Html.Attributes.id <| Term.id term ]
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
                        [ term |> Term.id |> fragmentOnly |> Html.Attributes.href
                        , target "_self"
                        ]
                        [ text "§" ]
                    ]
            ]
        ]


viewTags :
    { enableMathSupport : Bool, onClickTag : Maybe (Tag -> msg) }
    -> List Tag
    -> Html msg
viewTags { enableMathSupport, onClickTag } tags =
    Html.div
        [ class "mt-4" ]
        (List.map
            (\tag ->
                Components.Button.softSmall
                    (onClickTag /= Nothing)
                    [ class "mr-2 mb-2"
                    , Html.Attributes.title <| I18n.tag ++ ": " ++ Tag.inlineText tag
                    , Extras.HtmlAttribute.showMaybe (\onClickTag_ -> Html.Events.onClick <| onClickTag_ tag) onClickTag
                    ]
                    [ Icons.tag
                        [ Svg.Attributes.class "h-4 w-4 mr-1.5 text-gray-400 dark:text-gray-300 group-hover:text-gray-500 dark:group-hover:text-gray-100 flex-shrink-0" ]
                    , Tag.view enableMathSupport
                        [ class "text-sm" ]
                        tag
                    ]
            )
            tags
        )


viewGlossaryItemDefinition : { enableMathSupport : Bool } -> Definition -> Html msg
viewGlossaryItemDefinition { enableMathSupport } definition =
    Html.dd
        []
        [ Definition.view { enableMathSupport = enableMathSupport } definition
        ]


viewGlossaryItemRelatedTerms : Bool -> Bool -> Bool -> Maybe (Term -> msg) -> List DisambiguatedTerm -> List (Html msg)
viewGlossaryItemRelatedTerms enableMathSupport preview itemHasADefinition onClick relatedTerms =
    if List.isEmpty relatedTerms then
        []

    else
        [ Html.dd
            [ class "related-terms" ]
            (text
                (if itemHasADefinition then
                    I18n.seeAlso ++ ": "

                 else
                    I18n.see ++ ": "
                )
                :: (relatedTerms
                        |> List.map DisambiguatedTerm.toTerm
                        |> List.map
                            (\relatedTerm ->
                                Html.a
                                    [ (if preview then
                                        "#"

                                       else
                                        relatedTerm |> Term.id |> fragmentOnly
                                      )
                                        |> Html.Attributes.href
                                    , target "_self"
                                    , Extras.HtmlAttribute.showMaybe
                                        (\onClick1 ->
                                            Extras.HtmlEvents.onClickPreventDefaultAndStopPropagation <| onClick1 relatedTerm
                                        )
                                        onClick
                                    ]
                                    [ Term.view enableMathSupport NoTagIcon [] relatedTerm ]
                            )
                        |> List.intersperse (text ", ")
                   )
            )
        ]
