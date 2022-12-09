module Data.Glossary exposing (Glossary, toHtmlTree)

{-| TODO
-}

import Data.AboutLink as AboutLink exposing (AboutLink)
import Data.AboutParagraph as AboutParagraph exposing (AboutParagraph)
import Data.GlossaryItem as GlossaryItem
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Data.GlossaryTitle as GlossaryTitle exposing (GlossaryTitle)
import ElementIds
import Extras.HtmlTree as HtmlTree exposing (HtmlTree(..))


type alias Glossary =
    { enableHelpForMakingChanges : Bool
    , title : GlossaryTitle
    , aboutParagraph : AboutParagraph
    , aboutLinks : List AboutLink
    , items : GlossaryItems
    }


{-| Represent these glossary items as an HTML tree, ready for writing back to the glossary's HTML file.
-}
toHtmlTree : Glossary -> HtmlTree
toHtmlTree { enableHelpForMakingChanges, title, aboutParagraph, aboutLinks, items } =
    HtmlTree.Node "div"
        True
        [ HtmlTree.Attribute "id" ElementIds.container
        , HtmlTree.Attribute "data-enable-help-for-making-changes"
            (if enableHelpForMakingChanges then
                "true"

             else
                "false"
            )
        ]
        [ HtmlTree.Node "header"
            True
            []
            [ HtmlTree.Node "h1"
                True
                [ HtmlTree.Attribute "id" ElementIds.title ]
                [ HtmlTree.Leaf <| GlossaryTitle.toString title ]
            ]
        , HtmlTree.Node "main"
            True
            []
            [ HtmlTree.Node "div"
                True
                [ HtmlTree.Attribute "id" ElementIds.about ]
                [ HtmlTree.Node "p"
                    True
                    []
                    [ HtmlTree.Leaf <| AboutParagraph.toString aboutParagraph ]
                , HtmlTree.Node "ul"
                    True
                    []
                    (List.map
                        (\aboutLink ->
                            HtmlTree.Node "li"
                                True
                                []
                                [ HtmlTree.Node "a"
                                    True
                                    [ HtmlTree.Attribute "target" "_blank"
                                    , HtmlTree.Attribute "href" <| AboutLink.href aboutLink
                                    ]
                                    [ HtmlTree.Leaf <| AboutLink.body aboutLink ]
                                ]
                        )
                        aboutLinks
                    )
                ]
            , HtmlTree.Node "article"
                True
                [ HtmlTree.Attribute "id" ElementIds.items ]
                [ HtmlTree.Node "dl"
                    True
                    []
                    (items
                        |> GlossaryItems.orderedAlphabetically
                        |> List.map (Tuple.second >> GlossaryItem.toHtmlTree)
                    )
                ]
            ]
        ]
