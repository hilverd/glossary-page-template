module Data.Glossary exposing (Glossary, decode, toHtmlTree)

import Data.AboutLink as AboutLink exposing (AboutLink)
import Data.AboutParagraph as AboutParagraph exposing (AboutParagraph)
import Data.AboutSection exposing (AboutSection)
import Data.CardWidth as CardWidth exposing (CardWidth)
import Data.GlossaryItem.Tag as Tag exposing (Tag)
import Data.GlossaryItemForHtml as GlossaryItemForHtml exposing (GlossaryItemForHtml)
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Data.GlossaryTitle as GlossaryTitle exposing (GlossaryTitle)
import Data.TagDescription as TagDescription exposing (TagDescription)
import ElementIds
import Extras.HtmlTree as HtmlTree exposing (HtmlTree)
import Internationalisation as I18n
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)


type alias Glossary =
    { enableLastUpdatedDates : Bool
    , enableExportMenu : Bool
    , enableOrderItemsButtons : Bool
    , enableHelpForMakingChanges : Bool
    , cardWidth : CardWidth
    , title : GlossaryTitle
    , aboutSection : AboutSection
    , items : GlossaryItems
    }


create :
    Bool
    -> Bool
    -> Bool
    -> Bool
    -> CardWidth
    -> GlossaryTitle
    -> AboutParagraph
    -> List AboutLink
    -> List ( Tag, TagDescription )
    -> List GlossaryItemForHtml
    -> Result String Glossary
create enableLastUpdatedDates enableExportMenu enableOrderItemsButtons enableHelpForMakingChanges cardWidth title aboutParagraph aboutLinks tagsWithDescriptions itemsForHtml =
    let
        aboutSection =
            { paragraph = aboutParagraph, links = aboutLinks }

        items : Result String GlossaryItems
        items =
            GlossaryItems.fromList tagsWithDescriptions itemsForHtml
    in
    items
        |> Result.map
            (\items_ ->
                { enableLastUpdatedDates = enableLastUpdatedDates
                , enableExportMenu = enableExportMenu
                , enableOrderItemsButtons = enableOrderItemsButtons
                , enableHelpForMakingChanges = enableHelpForMakingChanges
                , cardWidth = cardWidth
                , title = title
                , aboutSection = aboutSection
                , items = items_
                }
            )


decode : Decoder (Result String Glossary)
decode =
    Decode.succeed create
        |> optional "enableLastUpdatedDates" Decode.bool False
        |> optional "enableExportMenu" Decode.bool True
        |> optional "enableOrderItemsButtons" Decode.bool True
        |> optional "enableHelpForMakingChanges" Decode.bool False
        |> optional "cardWidth" CardWidth.decode CardWidth.Compact
        |> optional "titleString" (Decode.map GlossaryTitle.fromMarkdown Decode.string) (GlossaryTitle.fromMarkdown I18n.elementNotFound)
        |> optional "aboutParagraph" (Decode.map AboutParagraph.fromMarkdown Decode.string) (AboutParagraph.fromMarkdown I18n.elementNotFound)
        |> optional "aboutLinks" (Decode.list AboutLink.decode) []
        |> optional "tagsWithDescriptions"
            (Decode.list <|
                Decode.map2
                    (\tagString descriptionString ->
                        ( Tag.fromMarkdown tagString
                        , TagDescription.fromMarkdown descriptionString
                        )
                    )
                    (Decode.field "tag" <| Decode.string)
                    (Decode.field "description" <| Decode.string)
            )
            []
        |> required "glossaryItems" (Decode.list GlossaryItemForHtml.decode)


{-| Represent these glossary items as an HTML tree, ready for writing back to the glossary's HTML file.
-}
toHtmlTree : Glossary -> HtmlTree
toHtmlTree { enableExportMenu, enableOrderItemsButtons, enableHelpForMakingChanges, cardWidth, title, aboutSection, enableLastUpdatedDates, items } =
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
