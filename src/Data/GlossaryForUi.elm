module Data.GlossaryForUi exposing
    ( GlossaryForUi
    , create, fromGlossaryFromDom
    , enableLastUpdatedDates, enableExportMenu, enableOrderItemsButtons, enableHelpForMakingChanges, cardWidth, defaultTheme, title, aboutSection, items, versionNumber
    , toGlossaryFromDom
    )

{-| A glossary ready to be used in a view function.


# Glossary

@docs GlossaryForUi


# Build

@docs create, fromGlossaryFromDom


# Query

@docs enableLastUpdatedDates, enableExportMenu, enableOrderItemsButtons, enableHelpForMakingChanges, cardWidth, defaultTheme, title, aboutSection, items, versionNumber


# Export

@docs toGlossaryFromDom

-}

import Data.AboutLink as AboutLink exposing (AboutLink)
import Data.AboutParagraph as AboutParagraph exposing (AboutParagraph)
import Data.AboutSection exposing (AboutSection)
import Data.CardWidth as CardWidth exposing (CardWidth)
import Data.DescribedTag as DescribedTag exposing (DescribedTag)
import Data.GlossaryFromDom exposing (GlossaryFromDom)
import Data.GlossaryItem.Tag as Tag
import Data.GlossaryItemForUi as GlossaryItemForUi exposing (GlossaryItemForUi)
import Data.GlossaryItemsForUi as GlossaryItemsForUi exposing (GlossaryItemsForUi)
import Data.GlossaryTitle as GlossaryTitle exposing (GlossaryTitle)
import Data.GlossaryVersionNumber as GlossaryVersionNumber exposing (GlossaryVersionNumber)
import Data.TagDescription as TagDescription
import Data.TagId as TagId
import Data.Theme as Theme exposing (Theme)
import Internationalisation as I18n


{-| A glossary for the UI.
-}
type GlossaryForUi
    = GlossaryForUi
        { enableLastUpdatedDates : Bool
        , enableExportMenu : Bool
        , enableOrderItemsButtons : Bool
        , enableHelpForMakingChanges : Bool
        , cardWidth : CardWidth
        , defaultTheme : Theme
        , title : GlossaryTitle
        , aboutSection : AboutSection
        , items : GlossaryItemsForUi
        , versionNumber : GlossaryVersionNumber
        }


{-| Whether or not showing last updated dates for items is enabled.
-}
enableLastUpdatedDates : GlossaryForUi -> Bool
enableLastUpdatedDates (GlossaryForUi glossaryForUi) =
    glossaryForUi.enableLastUpdatedDates


{-| Whether or not showing the export menu is enabled.
-}
enableExportMenu : GlossaryForUi -> Bool
enableExportMenu (GlossaryForUi glossaryForUi) =
    glossaryForUi.enableExportMenu


{-| Whether or not showing buttons for ordering items is enabled.
-}
enableOrderItemsButtons : GlossaryForUi -> Bool
enableOrderItemsButtons (GlossaryForUi glossaryForUi) =
    glossaryForUi.enableOrderItemsButtons


{-| Whether or not help for making changes is enabled.
-}
enableHelpForMakingChanges : GlossaryForUi -> Bool
enableHelpForMakingChanges (GlossaryForUi glossaryForUi) =
    glossaryForUi.enableHelpForMakingChanges


{-| Get the card width configuration for a GlossaryForUi.
-}
cardWidth : GlossaryForUi -> CardWidth
cardWidth (GlossaryForUi glossaryForUi) =
    glossaryForUi.cardWidth


{-| Get the default theme configuration for a GlossaryForUi.
-}
defaultTheme : GlossaryForUi -> Theme
defaultTheme (GlossaryForUi glossaryForUi) =
    glossaryForUi.defaultTheme


{-| Get the title for a GlossaryForUi.
-}
title : GlossaryForUi -> GlossaryTitle
title (GlossaryForUi glossaryForUi) =
    glossaryForUi.title


{-| Get the about section for a GlossaryForUi.
-}
aboutSection : GlossaryForUi -> AboutSection
aboutSection (GlossaryForUi glossaryForUi) =
    glossaryForUi.aboutSection


{-| Get the items for a GlossaryForUi.
-}
items : GlossaryForUi -> GlossaryItemsForUi
items (GlossaryForUi glossaryForUi) =
    glossaryForUi.items


{-| Get the version number for a GlossaryForUi.
-}
versionNumber : GlossaryForUi -> GlossaryVersionNumber
versionNumber (GlossaryForUi glossaryForUi) =
    glossaryForUi.versionNumber


{-| Build a GlossaryForUi from a GlossaryFromDom.
-}
fromGlossaryFromDom : GlossaryFromDom -> GlossaryForUi
fromGlossaryFromDom glossaryFromDom =
    create
        glossaryFromDom.enableLastUpdatedDates
        glossaryFromDom.enableExportMenu
        glossaryFromDom.enableOrderItemsButtons
        glossaryFromDom.enableHelpForMakingChanges
        glossaryFromDom.cardWidth
        glossaryFromDom.defaultTheme
        (GlossaryTitle.fromMarkdown glossaryFromDom.title)
        (AboutParagraph.fromMarkdown glossaryFromDom.aboutParagraph)
        (List.map (\aboutLinkFromDom -> AboutLink.create aboutLinkFromDom.href aboutLinkFromDom.body) glossaryFromDom.aboutLinks)
        (List.map
            (\describedTagFromDom ->
                DescribedTag.create
                    (TagId.create describedTagFromDom.id)
                    (Tag.fromMarkdown describedTagFromDom.tag)
                    (TagDescription.fromMarkdown describedTagFromDom.description)
            )
            glossaryFromDom.tags
        )
        (List.map
            GlossaryItemForUi.fromGlossaryItemFromDom
            glossaryFromDom.items
        )
        (GlossaryVersionNumber.create glossaryFromDom.versionNumber)


{-| Creates a new GlossaryForUi with the given configuration.
-}
create :
    Bool
    -> Bool
    -> Bool
    -> Bool
    -> CardWidth
    -> Theme
    -> GlossaryTitle
    -> AboutParagraph
    -> List AboutLink
    -> List DescribedTag
    -> List GlossaryItemForUi
    -> GlossaryVersionNumber
    -> GlossaryForUi
create enableLastUpdatedDates_ enableExportMenu_ enableOrderItemsButtons_ enableHelpForMakingChanges_ cardWidth_ defaultTheme_ title_ aboutParagraph aboutLinks describedTags itemsForHtml versionNumber_ =
    createWithDefaults
        (Just enableLastUpdatedDates_)
        (Just enableExportMenu_)
        (Just enableOrderItemsButtons_)
        (Just enableHelpForMakingChanges_)
        (Just cardWidth_)
        (Just defaultTheme_)
        (Just title_)
        (Just aboutParagraph)
        (Just aboutLinks)
        (Just describedTags)
        itemsForHtml
        (Just versionNumber_)


createWithDefaults :
    Maybe Bool
    -> Maybe Bool
    -> Maybe Bool
    -> Maybe Bool
    -> Maybe CardWidth
    -> Maybe Theme
    -> Maybe GlossaryTitle
    -> Maybe AboutParagraph
    -> Maybe (List AboutLink)
    -> Maybe (List DescribedTag)
    -> List GlossaryItemForUi
    -> Maybe GlossaryVersionNumber
    -> GlossaryForUi
createWithDefaults enableLastUpdatedDates_ enableExportMenu_ enableOrderItemsButtons_ enableHelpForMakingChanges_ cardWidth_ defaultTheme_ title_ aboutParagraph aboutLinks describedTags itemsForHtml versionNumber_ =
    let
        aboutSection_ : AboutSection
        aboutSection_ =
            { paragraph = Maybe.withDefault (AboutParagraph.fromMarkdown I18n.elementNotFound) aboutParagraph
            , links = Maybe.withDefault [] aboutLinks
            }

        items_ : Result String GlossaryItemsForUi
        items_ =
            GlossaryItemsForUi.fromList (Maybe.withDefault [] describedTags) itemsForHtml
    in
    GlossaryForUi
        { enableLastUpdatedDates = Maybe.withDefault False enableLastUpdatedDates_
        , enableExportMenu = Maybe.withDefault True enableExportMenu_
        , enableOrderItemsButtons = Maybe.withDefault True enableOrderItemsButtons_
        , enableHelpForMakingChanges = Maybe.withDefault False enableHelpForMakingChanges_
        , cardWidth = Maybe.withDefault CardWidth.Compact cardWidth_
        , defaultTheme = Maybe.withDefault Theme.System defaultTheme_
        , title = Maybe.withDefault (GlossaryTitle.fromMarkdown I18n.elementNotFound) title_
        , aboutSection = aboutSection_
        , items = Result.withDefault GlossaryItemsForUi.empty items_
        , versionNumber = Maybe.withDefault GlossaryVersionNumber.initial versionNumber_
        }


{-| Convert this glossary to a GlossaryFromDom.
-}
toGlossaryFromDom : GlossaryForUi -> GlossaryFromDom
toGlossaryFromDom (GlossaryForUi glossaryForUi) =
    { enableLastUpdatedDates = glossaryForUi.enableLastUpdatedDates
    , enableExportMenu = glossaryForUi.enableExportMenu
    , enableOrderItemsButtons = glossaryForUi.enableOrderItemsButtons
    , enableHelpForMakingChanges = glossaryForUi.enableHelpForMakingChanges
    , cardWidth = glossaryForUi.cardWidth
    , defaultTheme = glossaryForUi.defaultTheme
    , title = glossaryForUi.title |> GlossaryTitle.raw
    , aboutParagraph = glossaryForUi.aboutSection.paragraph |> AboutParagraph.raw
    , aboutLinks =
        glossaryForUi.aboutSection.links
            |> List.map
                (\aboutLink ->
                    { href = AboutLink.href aboutLink
                    , body = AboutLink.body aboutLink
                    }
                )
    , tags =
        glossaryForUi.items
            |> GlossaryItemsForUi.describedTags
            |> List.map
                (\describedTag ->
                    { id = describedTag |> DescribedTag.id |> TagId.toString
                    , tag = describedTag |> DescribedTag.tag |> Tag.raw
                    , description = describedTag |> DescribedTag.description |> TagDescription.raw
                    }
                )
    , items =
        glossaryForUi.items
            |> GlossaryItemsForUi.orderedAlphabetically Nothing
            |> List.map (Tuple.second >> GlossaryItemForUi.toGlossaryItemFromDom)
    , versionNumber = GlossaryVersionNumber.toInt glossaryForUi.versionNumber
    }
