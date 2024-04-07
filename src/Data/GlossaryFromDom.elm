module Data.GlossaryFromDom exposing
    ( GlossaryFromDom
    , create, codec
    , toHtmlTree
    )

{-| A glossary as sent to Elm by JavaScript when read from the DOM.

@docs GlossaryFromDom


# Build

@docs create, codec


# Export

@docs toHtmlTree

-}

import Codec exposing (Codec)
import Data.DescribedTagFromDom as DescribedTagFromDom exposing (DescribedTagFromDom)
import Data.GlossaryItemFromDom as GlossaryItemFromDom exposing (GlossaryItemFromDom)
import ElementIds
import Extras.HtmlTree as HtmlTree exposing (HtmlTree)
import Internationalisation as I18n


{-| A glossary read from the DOM.
-}
type alias GlossaryFromDom =
    { enableLastUpdatedDates : Bool
    , enableExportMenu : Bool
    , enableOrderItemsButtons : Bool
    , enableHelpForMakingChanges : Bool
    , cardWidth : String
    , title : String
    , aboutParagraph : String
    , aboutLinks : List { href : String, body : String }
    , tags : List DescribedTagFromDom
    , items : List GlossaryItemFromDom
    , versionNumber : Maybe Int
    }


{-| Create a GlossaryFromDom from its parts.
-}
create :
    Bool
    -> Bool
    -> Bool
    -> Bool
    -> String
    -> String
    -> String
    -> List { href : String, body : String }
    -> List DescribedTagFromDom
    -> List GlossaryItemFromDom
    -> Maybe Int
    -> GlossaryFromDom
create enableLastUpdatedDates_ enableExportMenu_ enableOrderItemsButtons_ enableHelpForMakingChanges_ cardWidth_ title_ aboutParagraph_ aboutLinks_ tags_ items_ versionNumber_ =
    { enableLastUpdatedDates = enableLastUpdatedDates_
    , enableExportMenu = enableExportMenu_
    , enableOrderItemsButtons = enableOrderItemsButtons_
    , enableHelpForMakingChanges = enableHelpForMakingChanges_
    , cardWidth = cardWidth_
    , title = title_
    , aboutParagraph = aboutParagraph_
    , aboutLinks = aboutLinks_
    , tags = tags_
    , items = items_
    , versionNumber = versionNumber_
    }


aboutLinkCodec : Codec { href : String, body : String }
aboutLinkCodec =
    Codec.object
        (\href_ body_ -> { href = href_, body = body_ })
        |> Codec.field "href" .href Codec.string
        |> Codec.field "body" .body Codec.string
        |> Codec.buildObject


{-| Convert a GlossaryFromDom to/from its JSON representation.
-}
codec : Codec GlossaryFromDom
codec =
    Codec.object
        create
        |> Codec.field "enableLastUpdatedDates" .enableLastUpdatedDates Codec.bool
        |> Codec.field "enableExportMenu" .enableExportMenu Codec.bool
        |> Codec.field "enableOrderItemsButtons" .enableOrderItemsButtons Codec.bool
        |> Codec.field "enableHelpForMakingChanges" .enableHelpForMakingChanges Codec.bool
        |> Codec.field "cardWidth" .cardWidth Codec.string
        |> Codec.field "titleString" .title Codec.string
        |> Codec.field "aboutParagraph" .aboutParagraph Codec.string
        |> Codec.field "aboutLinks" .aboutLinks (Codec.list aboutLinkCodec)
        |> Codec.field "tagsWithDescriptions" .tags (Codec.list DescribedTagFromDom.codec)
        |> Codec.field "glossaryItems" .items (Codec.list GlossaryItemFromDom.codec)
        |> Codec.field "versionNumber" .versionNumber (Codec.maybe Codec.int)
        |> Codec.buildObject


{-| Represent this GlossaryFromDom as an HTML tree, ready for writing back to the glossary's HTML file.
-}
toHtmlTree : List DescribedTagFromDom -> GlossaryFromDom -> HtmlTree
toHtmlTree describedTags glossaryFromDom =
    HtmlTree.Node "div"
        True
        [ HtmlTree.Attribute "id" ElementIds.container
        , HtmlTree.boolAttribute "data-enable-help-for-making-changes" glossaryFromDom.enableHelpForMakingChanges
        , HtmlTree.boolAttribute "data-enable-export-menu" glossaryFromDom.enableExportMenu
        , HtmlTree.boolAttribute "data-enable-order-items-buttons" glossaryFromDom.enableOrderItemsButtons
        , HtmlTree.boolAttribute "data-enable-last-updated-dates" glossaryFromDom.enableLastUpdatedDates
        , HtmlTree.Attribute "data-card-width" glossaryFromDom.cardWidth
        , HtmlTree.showAttributeMaybe "data-version-number" String.fromInt glossaryFromDom.versionNumber
        ]
        [ HtmlTree.Node "header"
            True
            []
            [ HtmlTree.Node "h1"
                True
                [ HtmlTree.Attribute "id" ElementIds.title ]
                [ HtmlTree.Leaf <| glossaryFromDom.title ]
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
                    [ HtmlTree.Leaf <| glossaryFromDom.aboutParagraph ]
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
                                    , HtmlTree.Attribute "href" <| aboutLink.href
                                    ]
                                    [ HtmlTree.Leaf <| aboutLink.body ]
                                ]
                        )
                        glossaryFromDom.aboutLinks
                    )
                ]
            , HtmlTree.showIf (not <| List.isEmpty describedTags) <|
                HtmlTree.Node
                    "div"
                    True
                    [ HtmlTree.Attribute "id" ElementIds.tags ]
                    [ HtmlTree.Leaf <| I18n.tags ++ ":"
                    , HtmlTree.Node "dl"
                        True
                        []
                        (List.map
                            (\describedTag ->
                                HtmlTree.Node "div"
                                    True
                                    [ HtmlTree.Attribute "data-id" <| describedTag.id ]
                                    [ HtmlTree.Node "dt"
                                        False
                                        []
                                        [ HtmlTree.Leaf describedTag.tag ]
                                    , HtmlTree.Node "dd"
                                        False
                                        []
                                        [ HtmlTree.Leaf describedTag.description ]
                                    ]
                            )
                            describedTags
                        )
                    ]
            , HtmlTree.Node "article"
                True
                [ HtmlTree.Attribute "id" ElementIds.items ]
                [ HtmlTree.Node "dl"
                    True
                    []
                    (List.map GlossaryItemFromDom.toHtmlTree glossaryFromDom.items)
                ]
            ]
        , HtmlTree.Node "footer"
            True
            []
            I18n.builtUsingGlossaryPageTemplateHtmlTree
        ]
