module Data.IncubatingGlossary exposing (IncubatingGlossary, toHtmlTree)

import Data.AboutLink as AboutLink
import Data.AboutParagraph as AboutParagraph
import Data.AboutSection exposing (AboutSection)
import Data.CardWidth as CardWidth exposing (CardWidth)
import Data.GlossaryItem.Tag as Tag exposing (Tag)
import Data.GlossaryItemForHtml as GlossaryItemForHtml exposing (GlossaryItemForHtml)
import Data.GlossaryTitle as GlossaryTitle exposing (GlossaryTitle)
import Data.IncubatingGlossaryItems as IncubatingGlossaryItems exposing (IncubatingGlossaryItems)
import ElementIds
import Extras.HtmlTree as HtmlTree exposing (HtmlTree)


type alias IncubatingGlossary =
    { enableMarkdownBasedSyntax : Bool
    , enableMathSupport : Bool
    , enableLastUpdatedDates : Bool
    , cardWidth : CardWidth
    , title : GlossaryTitle
    , aboutSection : AboutSection
    , items : IncubatingGlossaryItems
    }


{-| Represent these glossary items as an HTML tree, ready for writing back to the glossary's HTML file.
-}
toHtmlTree : Bool -> Bool -> IncubatingGlossary -> HtmlTree
toHtmlTree enableExportMenu enableHelpForMakingChanges { enableMarkdownBasedSyntax, cardWidth, title, aboutSection, enableLastUpdatedDates, items } =
    let
        tags =
            IncubatingGlossaryItems.tags items
    in
    HtmlTree.Node "div"
        True
        [ HtmlTree.Attribute "id" ElementIds.container
        , HtmlTree.boolAttribute "data-enable-help-for-making-changes" enableHelpForMakingChanges
        , HtmlTree.boolAttribute "data-enable-export-menu" enableExportMenu
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
                [ HtmlTree.showIf (not <| List.isEmpty tags) <|
                    HtmlTree.Node
                        "div"
                        True
                        [ HtmlTree.Attribute "id" ElementIds.tags ]
                        [ HtmlTree.Node "p"
                            True
                            []
                            (HtmlTree.Leaf "Tags:"
                                :: List.map
                                    (\tag ->
                                        HtmlTree.Node "button"
                                            False
                                            [ HtmlTree.Attribute "type" "button" ]
                                            [ HtmlTree.Leaf <| Tag.raw tag ]
                                    )
                                    tags
                            )
                        ]
                , HtmlTree.Node "dl"
                    True
                    []
                    (items
                        |> IncubatingGlossaryItems.orderedAlphabetically
                        |> List.map (Tuple.second >> GlossaryItemForHtml.toHtmlTree)
                    )
                ]
            ]
        , HtmlTree.Node "footer"
            True
            []
            [ HtmlTree.Leaf "Built using"
            , HtmlTree.Node "a"
                False
                [ HtmlTree.Attribute "target" "_blank"
                , HtmlTree.Attribute "href" "https://glossary.page/template"
                ]
                [ HtmlTree.Leaf "Glossary Page Template" ]
            , HtmlTree.Leaf "."
            ]
        ]
