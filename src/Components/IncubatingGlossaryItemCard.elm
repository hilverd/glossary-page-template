module Components.IncubatingGlossaryItemCard exposing (view, viewRelatedItem)

import Accessibility exposing (Html, div, span, text)
import Accessibility.Aria
import Components.Button
import Data.GlossaryItem.Definition as Definition exposing (Definition)
import Data.GlossaryItem.DisambiguatedTerm as DisambiguatedTerm exposing (DisambiguatedTerm)
import Data.GlossaryItem.Tag as Tag exposing (Tag)
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItemForUi as GlossaryItemForUi exposing (GlossaryItemForUi)
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
import Url.Builder


view :
    { enableMathSupport : Bool
    , enableLastUpdatedDates : Bool
    , onClickCopyToClipboard : msg
    , onClickEdit : msg
    , onClickDelete : msg
    , resultOfAttemptingToCopyItemTextToClipboard : Maybe Bool
    , editable : Bool
    }
    -> Maybe Tag
    -> Maybe String
    -> GlossaryItemForUi
    -> Html msg
view { enableMathSupport, enableLastUpdatedDates, onClickCopyToClipboard, onClickEdit, onClickDelete, resultOfAttemptingToCopyItemTextToClipboard, editable } tagBeingFilteredBy currentFragment glossaryItem =
    let
        tags : List Tag
        tags =
            GlossaryItemForUi.allTags glossaryItem

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

        disambiguatedPreferredTerm : Term
        disambiguatedPreferredTerm =
            glossaryItem
                |> GlossaryItemForUi.disambiguatedPreferredTerm
                |> DisambiguatedTerm.toTerm

        alternativeTerms : List Term
        alternativeTerms =
            GlossaryItemForUi.alternativeTerms glossaryItem

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
    in
    if editable then
        div
            [ class "w-[69ch] max-w-full flex flex-col justify-items-end bg-white dark:bg-black print:bg-white border dark:border-gray-700 print:border-none rounded-lg print:px-0 px-4 py-4 print:py-0"
            ]
            [ div
                [ class "" ]
                (viewGlossaryTerm
                    { enableMathSupport = enableMathSupport
                    , withLink = False
                    , isPreferred = True
                    }
                    disambiguatedPreferredTerm
                    :: List.map
                        (viewGlossaryTerm
                            { enableMathSupport = enableMathSupport
                            , withLink = False
                            , isPreferred = False
                            }
                        )
                        alternativeTerms
                    ++ (if needsUpdating then
                            [ Html.dd
                                [ class "mt-1 py-2 print:py-2 text-gray-600 dark:text-gray-300 print:text-black needs-updating" ]
                                [ span
                                    [ class "inline-flex items-center rounded-md bg-yellow-50 dark:bg-yellow-400/10 px-2 py-1 text-sm font-medium text-yellow-800 dark:text-yellow-500 ring-1 ring-inset ring-yellow-600/20 dark:ring-yellow-400/20" ]
                                    [ text I18n.needsUpdating ]
                                ]
                            ]

                        else
                            []
                       )
                    ++ [ viewTags
                            { enableMathSupport = enableMathSupport
                            , currentFragment = currentFragment
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
                            ]
                            [ Icons.pencil
                                [ Svg.Attributes.class "h-5 w-5 text-gray-400 dark:text-gray-300 hover:text-gray-500 dark:hover:text-gray-400" ]
                            , span
                                [ class "font-medium text-gray-600 dark:text-gray-300 hover:text-gray-700 dark:hover:text-gray-400" ]
                                [ text I18n.edit ]
                            ]
                        ]
                    , span
                        [ class "ml-3 inline-flex items-center" ]
                        [ Components.Button.text
                            [ Html.Events.onClick onClickDelete
                            ]
                            [ Icons.trash
                                [ Svg.Attributes.class "h-5 w-5 text-gray-400 dark:text-gray-300 hover:text-gray-500 dark:hover:text-gray-400" ]
                            , span
                                [ class "font-medium text-gray-600 dark:text-gray-300 hover:text-gray-700 dark:hover:text-gray-400" ]
                                [ text I18n.delete ]
                            ]
                        ]
                    ]
                ]
            ]

    else
        div
            [ class "w-[69ch] max-w-full flex flex-col justify-between overflow-x-clip bg-white dark:bg-black print:bg-white border dark:border-gray-700 print:border-none rounded-lg print:px-0 px-4 py-4 print:py-0"
            ]
            [ div
                [ class "flex-1" ]
                [ div []
                    (viewGlossaryTerm
                        { enableMathSupport = enableMathSupport
                        , withLink = False
                        , isPreferred = True
                        }
                        disambiguatedPreferredTerm
                        :: List.map
                            (viewGlossaryTerm
                                { enableMathSupport = enableMathSupport
                                , withLink = False
                                , isPreferred = False
                                }
                            )
                            alternativeTerms
                        ++ (if needsUpdating then
                                [ Html.dd
                                    [ class "mt-1 py-2 print:py-2 text-gray-600 dark:text-gray-300 print:text-black needs-updating" ]
                                    [ span
                                        [ class "inline-flex items-center rounded-md bg-yellow-50 dark:bg-yellow-400/10 px-2 py-1 text-sm font-medium text-yellow-800 dark:text-yellow-500 ring-1 ring-inset ring-yellow-600/20 dark:ring-yellow-400/20" ]
                                        [ text I18n.needsUpdating ]
                                    ]
                                ]

                            else
                                []
                           )
                        ++ [ viewTags
                                { enableMathSupport = enableMathSupport
                                , currentFragment = currentFragment
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


viewGlossaryTerm :
    { enableMathSupport : Bool, withLink : Bool, isPreferred : Bool }
    -> Term
    -> Html msg
viewGlossaryTerm { enableMathSupport, withLink, isPreferred } term =
    let
        viewTerm : Html msg
        viewTerm =
            if isPreferred then
                if withLink then
                    Html.a
                        [ term |> Term.id |> fragmentOnly |> Html.Attributes.href
                        , target "_self"
                        ]
                        [ Term.view enableMathSupport [] term ]

                else
                    Term.view enableMathSupport [] term

            else
                span
                    [ class "inline-flex items-center" ]
                    [ Icons.cornerLeftUp
                        [ Svg.Attributes.class "h-5 w-5 shrink-0 pb-1 mr-1.5 text-gray-400 dark:text-gray-400 print:hidden"
                        ]
                    , Term.view enableMathSupport [ class "font-normal" ] term
                    ]
    in
    div
        [ class "flex justify-between"
        , Extras.HtmlAttribute.showIf isPreferred <| class "mb-1"
        ]
        [ Html.dt
            [ class "font-semibold print:font-normal text-gray-600 dark:text-gray-200 print:text-black" ]
            [ span [ class "mr-1.5 hidden print:inline" ] [ text "➢" ]
            , Html.dfn
                [ class "not-italic" ]
                [ if Term.isAbbreviation term then
                    Html.abbr []
                        [ viewTerm ]

                  else
                    viewTerm
                ]
            ]
        ]


{-| Build a URL with a filter-by-tag query parameter and optional fragment.
-}
buildTagFilterUrl : Tag -> Maybe String -> String
buildTagFilterUrl tag maybeFragment =
    let
        queryUrl : String
        queryUrl =
            [ Tag.toQueryParameter tag ]
                |> Url.Builder.relative []
                |> (\urlString ->
                        if urlString == "" then
                            "?"

                        else
                            urlString
                   )
    in
    case maybeFragment of
        Just fragment ->
            queryUrl ++ "#" ++ fragment

        Nothing ->
            queryUrl


viewTags :
    { enableMathSupport : Bool, currentFragment : Maybe String }
    -> List Tag
    -> Html msg
viewTags { enableMathSupport, currentFragment } tags =
    Html.div
        [ class "mt-4" ]
        (List.map
            (\tag ->
                Html.a
                    [ class "inline-flex items-center rounded-full max-w-3xs border border-gray-300 dark:border-gray-500 bg-white dark:bg-gray-800 px-2 py-1 text-sm text-gray-700 dark:text-gray-100 shadow-xs hover:bg-gray-100 dark:hover:bg-gray-700 no-underline hover:no-underline mr-2 mb-2"
                    , Html.Attributes.title <| I18n.tag ++ ": " ++ Tag.inlineText tag
                    , Html.Attributes.href <| buildTagFilterUrl tag currentFragment
                    ]
                    [ Tag.view enableMathSupport
                        [ class "text-sm" ]
                        tag
                    ]
            )
            tags
        )


viewGlossaryItemDefinition : { enableMathSupport : Bool } -> Definition -> Html msg
viewGlossaryItemDefinition { enableMathSupport } definition =
    Html.dd
        [ class "mt-1 print:mt-0 py-2 print:py-0 text-gray-600 dark:text-gray-300 print:text-black item-definition" ]
        [ Definition.view
            { enableMathSupport = enableMathSupport }
            definition
        ]


viewGlossaryItemRelatedTerms : Bool -> Bool -> Bool -> Maybe (Term -> msg) -> List DisambiguatedTerm -> List (Html msg)
viewGlossaryItemRelatedTerms enableMathSupport preview itemHasADefinition onClick relatedTerms =
    if List.isEmpty relatedTerms then
        []

    else
        [ Html.dd
            [ class "related-terms mt-1 print:mt-0 py-2 print:py-0 text-gray-600 dark:text-gray-300" ]
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
                                    [ Term.view enableMathSupport [] relatedTerm ]
                            )
                        |> List.intersperse (text ", ")
                   )
            )
        ]


viewRelatedItem : Bool -> GlossaryItemForUi -> Html msg
viewRelatedItem enableMathSupport glossaryItem =
    let
        disambiguatedPreferredTerm : Term
        disambiguatedPreferredTerm =
            glossaryItem
                |> GlossaryItemForUi.disambiguatedPreferredTerm
                |> DisambiguatedTerm.toTerm

        alternativeTerms : List Term
        alternativeTerms =
            GlossaryItemForUi.alternativeTerms glossaryItem

        definition : Maybe Definition
        definition =
            GlossaryItemForUi.definition glossaryItem
    in
    div
        [ class "w-[69ch] relative max-w-full flex flex-col justify-between overflow-x-clip bg-white dark:bg-black print:bg-white border dark:border-gray-700 print:border-none rounded-lg"
        ]
        [ div
            [ class "flex-1 max-h-40 overflow-hidden print:px-0 px-4 pt-4 pb-6 print:py-0" ]
            [ div []
                (viewGlossaryTerm
                    { enableMathSupport = enableMathSupport
                    , withLink = True
                    , isPreferred = True
                    }
                    disambiguatedPreferredTerm
                    :: List.map
                        (viewGlossaryTerm
                            { enableMathSupport = enableMathSupport
                            , withLink = False
                            , isPreferred = False
                            }
                        )
                        alternativeTerms
                    ++ [ definition
                            |> Extras.Html.showMaybe
                                (viewGlossaryItemDefinition
                                    { enableMathSupport = enableMathSupport }
                                )
                       ]
                )
            ]
        , div
            [ class "absolute bottom-0 left-4 right-4 h-8 bg-gradient-to-t from-white dark:from-black to-transparent pointer-events-none" ]
            []
        ]
