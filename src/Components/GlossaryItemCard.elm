module Components.GlossaryItemCard exposing (Style(..), view)

import Accessibility exposing (..)
import Accessibility.Key exposing (tabbable)
import Components.Button
import Data.AboutSection exposing (AboutSection(..))
import Data.GlossaryItem as GlossaryItem exposing (GlossaryItem)
import Data.GlossaryItem.Details as Details
import Data.GlossaryItem.RelatedTerm as RelatedTerm exposing (RelatedTerm)
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItemIndex exposing (GlossaryItemIndex)
import ElementIds
import Extras.Html
import Extras.HtmlTree exposing (HtmlTree(..))
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


view : Style msg -> GlossaryItem -> Html msg
view style glossaryItem =
    case style of
        Preview ->
            let
                itemHasSomeDetails =
                    GlossaryItem.hasSomeDetails glossaryItem

                tabbable =
                    True
            in
            div
                [ Html.Attributes.style "max-height" "100%" ]
                (List.map (viewGlossaryTerm True tabbable) glossaryItem.terms
                    ++ List.map viewGlossaryItemDetails glossaryItem.details
                    ++ viewGlossaryItemRelatedTerms True tabbable itemHasSomeDetails glossaryItem.relatedTerms
                )

        Normal { index, tabbable, onClickEdit, onClickDelete, editable, errorWhileDeleting } ->
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
            in
            if editable then
                div
                    [ class "flex flex-col justify-items-end"
                    , id <| ElementIds.glossaryItemDiv index
                    ]
                    [ div
                        []
                        (List.map (viewGlossaryTerm False tabbable) glossaryItem.terms
                            ++ List.map viewGlossaryItemDetails glossaryItem.details
                            ++ viewGlossaryItemRelatedTerms False tabbable itemHasSomeDetails glossaryItem.relatedTerms
                        )
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
                                        [ Svg.Attributes.class "h-5 w-5" ]
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
                    (List.map (viewGlossaryTerm False tabbable) glossaryItem.terms
                        ++ List.map viewGlossaryItemDetails glossaryItem.details
                        ++ viewGlossaryItemRelatedTerms False tabbable itemHasSomeDetails glossaryItem.relatedTerms
                    )


viewGlossaryTerm : Bool -> Bool -> Term -> Html msg
viewGlossaryTerm preview tabbable term =
    Html.dt
        [ class "group" ]
        [ Html.dfn
            [ Html.Attributes.id <| Term.id term ]
            [ if Term.isAbbreviation term then
                Html.abbr [] [ text <| Term.raw term ]

              else
                text <| Term.raw term
            ]
        , span
            [ class "silcrow invisible group-hover:visible hover:visible" ]
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


viewGlossaryItemDetails : Details.Details -> Html msg
viewGlossaryItemDetails details =
    Html.dd
        []
        [ Details.view details ]


viewGlossaryItemRelatedTerms : Bool -> Bool -> Bool -> List RelatedTerm -> List (Html msg)
viewGlossaryItemRelatedTerms preview tabbable itemHasSomeDetails relatedTerms =
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
                                    [ text <| RelatedTerm.raw relatedTerm ]
                            )
                        |> List.intersperse (text ", ")
                   )
            )
        ]
