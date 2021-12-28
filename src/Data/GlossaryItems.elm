module Data.GlossaryItems exposing (GlossaryItems, sanitise, toHtmlTree)

import Array
import Data.GlossaryItem as GlossaryItem exposing (GlossaryItem)
import Extras.HtmlTree as HtmlTree exposing (HtmlTree(..))
import Extras.Layout as Layout exposing (Layout)
import Set


type alias GlossaryItems =
    List GlossaryItem


toHtmlTree : Bool -> Layout -> GlossaryItems -> HtmlTree
toHtmlTree enableHelpForMakingChanges defaultLayout glossaryItems =
    HtmlTree.Node "article"
        [ HtmlTree.Attribute "id" "glossary"
        , HtmlTree.Attribute "data-enable-help-for-making-changes"
            (if enableHelpForMakingChanges then
                "true"

             else
                "false"
            )
        , HtmlTree.Attribute "data-default-layout"
            (case defaultLayout of
                Layout.Cards ->
                    "cards"

                Layout.Table ->
                    "table"
            )
        ]
        [ HtmlTree.Node "dl"
            []
            (List.map GlossaryItem.toHtmlTree glossaryItems)
        ]


sanitise : GlossaryItems -> GlossaryItems
sanitise glossaryItems =
    let
        termIdsSet =
            glossaryItems
                |> List.map .terms
                |> List.concat
                |> List.map .id
                |> Set.fromList
    in
    glossaryItems
        |> List.map
            (\glossaryItem ->
                { glossaryItem
                    | relatedTerms =
                        glossaryItem.relatedTerms
                            |> List.filter (\relatedTerm -> Set.member relatedTerm.idReference termIdsSet)
                }
            )
