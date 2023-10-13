module Data.Glossary exposing (Glossary, toHtmlTree)

import Array
import Data.AboutLink as AboutLink
import Data.AboutParagraph as AboutParagraph
import Data.AboutSection exposing (AboutSection)
import Data.CardWidth as CardWidth exposing (CardWidth)
import Data.GlossaryItem as GlossaryItem
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Data.GlossaryTitle as GlossaryTitle exposing (GlossaryTitle)
import ElementIds
import Extras.HtmlTree as HtmlTree exposing (HtmlTree)


type alias Glossary =
    { enableMarkdownBasedSyntax : Bool
    , enableMathSupport : Bool
    , enableLastUpdatedDates : Bool
    , cardWidth : CardWidth
    , title : GlossaryTitle
    , aboutSection : AboutSection
    , items : GlossaryItems
    }


{-| Represent these glossary items as an HTML tree, ready for writing back to the glossary's HTML file.
-}
toHtmlTree : Bool -> Bool -> Bool -> Glossary -> HtmlTree
toHtmlTree enableExportMenu enableOrderItemsButtons enableHelpForMakingChanges { enableMarkdownBasedSyntax, cardWidth, title, aboutSection, enableLastUpdatedDates, items } =
    HtmlTree.Node "div"
        True
        [ HtmlTree.Attribute "id" ElementIds.container
        , HtmlTree.boolAttribute "data-enable-help-for-making-changes" enableHelpForMakingChanges
        , HtmlTree.boolAttribute "data-enable-export-menu" enableExportMenu
        , HtmlTree.boolAttribute "data-enable-order-items-buttons" enableOrderItemsButtons
        , HtmlTree.boolAttribute "data-enable-markdown-based-syntax" enableMarkdownBasedSyntax
        , HtmlTree.boolAttribute "data-enable-last-updated-dates" enableLastUpdatedDates
        , cardWidth |> CardWidth.toHtmlTreeAttribute
        ]
        [ HtmlTree.Node "header"
            True
            []
            [ HtmlTree.Node "h1"
                True
                [ HtmlTree.Attribute "id" ElementIds.title ]
                [ HtmlTree.Leaf <| GlossaryTitle.raw title ]
            ]
        , HtmlTree.Node "main"
            True
            []
            [ HtmlTree.Node "div"
                True
                [ HtmlTree.Attribute "id" ElementIds.about ]
                [ HtmlTree.Node "p"
                    False
                    []
                    [ HtmlTree.Leaf <| AboutParagraph.raw aboutSection.paragraph ]
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
                        aboutSection.links
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
                        |> Array.toList
                        |> List.map (Tuple.second >> GlossaryItem.toHtmlTree)
                    )
                ]
            ]
        ]
