module Data.GlossaryFromDom exposing
    ( GlossaryFromDom
    , create, codec
    )

{-| A glossary as sent to Elm by JavaScript when read from the DOM.

@docs GlossaryFromDom


# Build

@docs create, codec

-}

import Codec exposing (Codec)
import Data.DescribedTagFromDom as DescribedTagFromDom exposing (DescribedTagFromDom)
import Data.GlossaryItemFromDom as GlossaryItemFromDom exposing (GlossaryItemFromDom)


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
        |> Codec.field "title" .title Codec.string
        |> Codec.field "aboutParagraph" .aboutParagraph Codec.string
        |> Codec.field "aboutLinks" .aboutLinks (Codec.list aboutLinkCodec)
        |> Codec.field "tags" .tags (Codec.list DescribedTagFromDom.codec)
        |> Codec.field "items" .items (Codec.list GlossaryItemFromDom.codec)
        |> Codec.field "versionNumber" .versionNumber (Codec.maybe Codec.int)
        |> Codec.buildObject
