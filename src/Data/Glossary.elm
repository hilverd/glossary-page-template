module Data.Glossary exposing (Glossary, toHtmlTree)

import Data.AboutLink as AboutLink
import Data.AboutParagraph as AboutParagraph
import Data.AboutSection exposing (AboutSection)
import Data.CardWidth as CardWidth exposing (CardWidth)
import Data.GlossaryItem.Tag as Tag
import Data.GlossaryItemForHtml as GlossaryItemForHtml
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Data.GlossaryTitle as GlossaryTitle exposing (GlossaryTitle)
import Data.TagDescription as TagDescription
import ElementIds
import Extras.HtmlTree as HtmlTree exposing (HtmlTree)
import Internationalisation as I18n


type alias Glossary =
    { enableMathSupport : Bool
    , enableLastUpdatedDates : Bool
    , enableExportMenu : Bool
    , enableOrderItemsButtons : Bool
    , cardWidth : CardWidth
    , title : GlossaryTitle
    , aboutSection : AboutSection
    , items : GlossaryItems
    }


{-| Represent these glossary items as an HTML tree, ready for writing back to the glossary's HTML file.
-}
toHtmlTree : Bool -> Glossary -> HtmlTree
toHtmlTree enableHelpForMakingChanges { enableExportMenu, enableOrderItemsButtons, cardWidth, title, aboutSection, enableLastUpdatedDates, items } =
    let
        tagsWithDescriptions =
            GlossaryItems.tagsWithDescriptions items
    in
    HtmlTree.Node "div"
        True
        [ HtmlTree.Attribute "id" ElementIds.container
        , HtmlTree.boolAttribute "data-enable-help-for-making-changes" enableHelpForMakingChanges
        , HtmlTree.boolAttribute "data-enable-export-menu" enableExportMenu
        , HtmlTree.boolAttribute "data-enable-order-items-buttons" enableOrderItemsButtons
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
            , HtmlTree.showIf (not <| List.isEmpty tagsWithDescriptions) <|
                HtmlTree.Node
                    "div"
                    True
                    [ HtmlTree.Attribute "id" ElementIds.tags ]
                    [ HtmlTree.Leaf <| I18n.tags ++ ":"
                    , HtmlTree.Node "dl"
                        True
                        []
                        (List.map
                            (\( tag, description ) ->
                                HtmlTree.Node "div"
                                    True
                                    []
                                    [ HtmlTree.Node "dt"
                                        False
                                        []
                                        [ HtmlTree.Leaf <| Tag.raw tag ]
                                    , HtmlTree.Node "dd"
                                        False
                                        []
                                        [ HtmlTree.Leaf <| TagDescription.raw description ]
                                    ]
                            )
                            tagsWithDescriptions
                        )
                    ]
            , HtmlTree.Node "article"
                True
                [ HtmlTree.Attribute "id" ElementIds.items ]
                [ HtmlTree.Node "dl"
                    True
                    []
                    (items
                        |> GlossaryItems.orderedAlphabetically Nothing
                        |> List.map (Tuple.second >> GlossaryItemForHtml.toHtmlTree)
                    )
                ]
            ]
        , HtmlTree.Node "footer"
            True
            []
            I18n.builtUsingGlossaryPageTemplateHtmlTree
        ]
