module Components.GlossaryItemCard exposing (Style(..), view)

import Accessibility exposing (Html, div, p, span, text)
import Accessibility.Key
import Components.Button
import Data.FeatureFlag exposing (enableFeaturesInProgress)
import Data.GlossaryItem as GlossaryItem exposing (GlossaryItem)
import Data.GlossaryItem.Details as Details
import Data.GlossaryItem.RelatedTerm as RelatedTerm exposing (RelatedTerm)
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItemIndex exposing (GlossaryItemIndex)
import ElementIds
import Extras.Html
import Extras.Url exposing (fragmentOnly)
import Html
import Html.Attributes exposing (class, id)
import Html.Events
import Icons
import Svg.Attributes


type Style msg
    = Preview
    | Normal
        { index : GlossaryItemIndex
        , tabbable : Bool
        , onClickEdit : msg
        , onClickDelete : msg
        , editable : Bool
        , errorWhileDeleting : Maybe ( GlossaryItemIndex, String )
        }


view : { enableMathSupport : Bool, makeLinksTabbable : Bool } -> Style msg -> GlossaryItem -> Html msg
view { enableMathSupport, makeLinksTabbable } style glossaryItem =
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
                (List.map (viewGlossaryTerm enableMathSupport True tabbable) glossaryItem.terms
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
                    ++ viewGlossaryItemRelatedTerms enableMathSupport True tabbable itemHasSomeDetails glossaryItem.relatedTerms
                )

        Normal { index, tabbable, onClickEdit, onClickDelete, editable, errorWhileDeleting } ->
            let
                errorDiv : String -> Html msg
                errorDiv message =
                    div
                        [ class "flex justify-end mt-2" ]
                        [ p
                            [ class "text-red-600" ]
                            [ text message ]
                        ]

                itemHasSomeDetails : Bool
                itemHasSomeDetails =
                    GlossaryItem.hasSomeDetails glossaryItem
            in
            if editable then
                div
                    [ class "flex flex-col justify-items-end"
                    , id <| ElementIds.glossaryItemDiv index
                    ]
                    [ div
                        []
                        [ Extras.Html.showIf enableFeaturesInProgress <|
                            div
                                [ class "float-right sticky top-0 bg-white dark:bg-gray-800 bg-opacity-75 dark:bg-opacity-75 p-0.5 rounded-full" ]
                                [ span
                                    [ class "print:hidden" ]
                                    [ Components.Button.text
                                        [ Accessibility.Key.tabbable tabbable
                                        ]
                                        [ Icons.arrowsPointingOut
                                            [ Svg.Attributes.class "h-5 w-5 text-gray-400 dark:text-gray-300 hover:text-gray-500 dark:hover:text-gray-400" ]
                                        ]
                                    ]
                                ]
                        , div
                            []
                            (List.map (viewGlossaryTerm enableMathSupport False tabbable) glossaryItem.terms
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
                                ++ viewGlossaryItemRelatedTerms enableMathSupport False tabbable itemHasSomeDetails glossaryItem.relatedTerms
                            )
                        ]
                    , div
                        [ class "print:hidden mt-3 flex flex-col flex-grow justify-end" ]
                        [ div
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
                div []
                    [ Extras.Html.showIf enableFeaturesInProgress <|
                        div
                            [ class "float-right sticky top-0 bg-white dark:bg-gray-800 bg-opacity-75 dark:bg-opacity-75 p-0.5 rounded-full" ]
                            [ span
                                [ class "print:hidden" ]
                                [ Components.Button.text
                                    [ Accessibility.Key.tabbable tabbable
                                    ]
                                    [ Icons.arrowsPointingOut
                                        [ Svg.Attributes.class "h-5 w-5 text-gray-400 dark:text-gray-300 hover:text-gray-500 dark:hover:text-gray-400" ]
                                    ]
                                ]
                            ]
                    , div []
                        (List.map (viewGlossaryTerm enableMathSupport False tabbable) glossaryItem.terms
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
                            ++ viewGlossaryItemRelatedTerms enableMathSupport False tabbable itemHasSomeDetails glossaryItem.relatedTerms
                        )
                    ]


viewGlossaryTerm : Bool -> Bool -> Bool -> Term -> Html msg
viewGlossaryTerm enableMathSupport preview tabbable term =
    div
        [ class "flex justify-between" ]
        [ Html.dt
            [ class "group" ]
            [ span [ class "mr-1.5 hidden print:inline" ] [ text "➢" ]
            , Html.dfn
                [ Html.Attributes.id <| Term.id term ]
                [ if Term.isAbbreviation term then
                    Html.abbr [] [ Term.view enableMathSupport term ]

                  else
                    Term.view enableMathSupport term
                ]
            , span
                [ class "silcrow invisible group-hover:visible hover:visible print:group-hover:invisible print:hover:invisible" ]
                [ Html.a
                    [ (if preview then
                        "#"

                       else
                        term |> Term.id |> fragmentOnly
                      )
                        |> Html.Attributes.href
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


viewGlossaryItemRelatedTerms : Bool -> Bool -> Bool -> Bool -> List RelatedTerm -> List (Html msg)
viewGlossaryItemRelatedTerms enableMathSupport preview tabbable itemHasSomeDetails relatedTerms =
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
                                    ]
                                    [ RelatedTerm.view enableMathSupport relatedTerm ]
                            )
                        |> List.intersperse (text ", ")
                   )
            )
        ]
