module Data.Glossary exposing (Glossary, codec, toHtmlTree)

import Codec exposing (Codec)
import Data.AboutLink as AboutLink exposing (AboutLink)
import Data.AboutParagraph as AboutParagraph exposing (AboutParagraph)
import Data.AboutSection exposing (AboutSection)
import Data.CardWidth as CardWidth exposing (CardWidth)
import Data.GlossaryItem.Tag as Tag exposing (Tag)
import Data.GlossaryItemForHtml as GlossaryItemForHtml exposing (GlossaryItemForHtml)
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems, tagsWithDescriptions)
import Data.GlossaryTitle as GlossaryTitle exposing (GlossaryTitle)
import Data.TagDescription as TagDescription exposing (TagDescription)
import ElementIds
import Extras.HtmlTree as HtmlTree exposing (HtmlTree)
import Internationalisation as I18n


type alias Glossary =
    { enableLastUpdatedDates : Bool
    , enableExportMenu : Bool
    , enableOrderItemsButtons : Bool
    , enableHelpForMakingChanges : Bool
    , cardWidth : CardWidth
    , separateBackendBaseUrl : Maybe String
    , title : GlossaryTitle
    , aboutSection : AboutSection
    , items : GlossaryItems
    }


create :
    Maybe Bool
    -> Maybe Bool
    -> Maybe Bool
    -> Maybe Bool
    -> Maybe CardWidth
    -> Maybe String
    -> Maybe GlossaryTitle
    -> Maybe AboutParagraph
    -> Maybe (List AboutLink)
    -> Maybe (List ( Tag, TagDescription ))
    -> List GlossaryItemForHtml
    -> Glossary
create enableLastUpdatedDates enableExportMenu enableOrderItemsButtons enableHelpForMakingChanges cardWidth separateBackendBaseUrl title aboutParagraph aboutLinks tagsWithDescriptions itemsForHtml =
    let
        aboutSection =
            { paragraph = Maybe.withDefault (AboutParagraph.fromMarkdown I18n.elementNotFound) aboutParagraph
            , links = Maybe.withDefault [] aboutLinks
            }

        items : Result String GlossaryItems
        items =
            GlossaryItems.fromList (Maybe.withDefault [] tagsWithDescriptions) itemsForHtml
    in
    { enableLastUpdatedDates = Maybe.withDefault False enableLastUpdatedDates
    , enableExportMenu = Maybe.withDefault True enableExportMenu
    , enableOrderItemsButtons = Maybe.withDefault True enableOrderItemsButtons
    , enableHelpForMakingChanges = Maybe.withDefault False enableHelpForMakingChanges
    , cardWidth = Maybe.withDefault CardWidth.Compact cardWidth
    , separateBackendBaseUrl = separateBackendBaseUrl
    , title = Maybe.withDefault (GlossaryTitle.fromMarkdown I18n.elementNotFound) title
    , aboutSection = aboutSection
    , items = Result.withDefault GlossaryItems.empty items
    }


codec : Codec Glossary
codec =
    Codec.object create
        |> Codec.optionalField "enableLastUpdatedDates" (.enableLastUpdatedDates >> Just) Codec.bool
        |> Codec.optionalField "enableExportMenu" (.enableExportMenu >> Just) Codec.bool
        |> Codec.optionalField "enableOrderItemsButtons" (.enableOrderItemsButtons >> Just) Codec.bool
        |> Codec.optionalField "enableHelpForMakingChanges" (.enableHelpForMakingChanges >> Just) Codec.bool
        |> Codec.optionalField "cardWidth" (.cardWidth >> Just) CardWidth.codec
        |> Codec.optionalNullableField "separateBackendBaseUrl" .separateBackendBaseUrl Codec.string
        |> Codec.optionalField "titleString" (.title >> Just) (Codec.map GlossaryTitle.fromMarkdown GlossaryTitle.raw Codec.string)
        |> Codec.optionalField "aboutParagraph" (.aboutSection >> .paragraph >> Just) (Codec.map AboutParagraph.fromMarkdown AboutParagraph.raw Codec.string)
        |> Codec.optionalField "aboutLinks" (.aboutSection >> .links >> Just) (Codec.list AboutLink.codec)
        |> Codec.optionalField "tagsWithDescriptions"
            (.items >> GlossaryItems.tagsWithDescriptions >> Just)
            (Codec.list
                (Codec.object
                    (\tagString descriptionString ->
                        ( Tag.fromMarkdown tagString
                        , TagDescription.fromMarkdown descriptionString
                        )
                    )
                    |> Codec.field "tag" (Tuple.first >> Tag.raw) Codec.string
                    |> Codec.field "description" (Tuple.second >> TagDescription.raw) Codec.string
                    |> Codec.buildObject
                )
            )
        |> Codec.field "glossaryItems"
            (.items >> GlossaryItems.orderedAlphabetically Nothing >> List.map Tuple.second)
            (Codec.list GlossaryItemForHtml.codec)
        |> Codec.buildObject


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
