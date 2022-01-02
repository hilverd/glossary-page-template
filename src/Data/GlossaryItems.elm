module Data.GlossaryItems exposing (GlossaryItems, sanitise, toHtmlTree)

import Data.GlossaryItem as GlossaryItem exposing (GlossaryItem)
import Extras.HtmlTree as HtmlTree exposing (HtmlTree(..))
import Set


type alias GlossaryItems =
    List GlossaryItem


toHtmlTree : Bool -> GlossaryItems -> HtmlTree
toHtmlTree enableHelpForMakingChanges glossaryItems =
    HtmlTree.Node "article"
        True
        [ HtmlTree.Attribute "id" "glossary"
        , HtmlTree.Attribute "data-enable-help-for-making-changes"
            (if enableHelpForMakingChanges then
                "true"

             else
                "false"
            )
        ]
        [ HtmlTree.Node "dl"
            True
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
