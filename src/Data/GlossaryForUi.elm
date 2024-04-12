module Data.GlossaryForUi exposing
    ( GlossaryForUi
    , create, codec, setEnableLastUpdatedDates, toggleEnableLastUpdatedDates, setEnableExportMenu, toggleEnableExportMenu, setEnableOrderItemsButtons, toggleEnableOrderItemsButtons, setEnableHelpForMakingChanges, setCardWidth, setTitle, setAboutSection, setItems, fromGlossaryFromDom
    , ApplyChangesResult(..), applyChanges
    , enableLastUpdatedDates, enableExportMenu, enableOrderItemsButtons, enableHelpForMakingChanges, cardWidth, title, aboutSection, items, versionNumber
    , toGlossaryFromDom, toHtmlTree
    )

{-| A glossary ready to be used in a view function.


# Glossary

@docs GlossaryForUi


# Build

@docs create, codec, setEnableLastUpdatedDates, toggleEnableLastUpdatedDates, setEnableExportMenu, toggleEnableExportMenu, setEnableOrderItemsButtons, toggleEnableOrderItemsButtons, setEnableHelpForMakingChanges, setCardWidth, setTitle, setAboutSection, setItems, fromGlossaryFromDom


# Apply Changes

@docs ApplyChangesResult, applyChanges


# Query

@docs enableLastUpdatedDates, enableExportMenu, enableOrderItemsButtons, enableHelpForMakingChanges, cardWidth, title, aboutSection, items, versionNumber


# Export

@docs toGlossaryFromDom, toHtmlTree

-}

import Codec exposing (Codec)
import Data.AboutLink as AboutLink exposing (AboutLink)
import Data.AboutParagraph as AboutParagraph exposing (AboutParagraph)
import Data.AboutSection exposing (AboutSection)
import Data.CardWidth as CardWidth exposing (CardWidth)
import Data.DescribedTag as DescribedTag exposing (DescribedTag)
import Data.GlossaryChange exposing (GlossaryChange(..))
import Data.GlossaryChangelist as GlossaryChangelist exposing (GlossaryChangelist)
import Data.GlossaryFromDom exposing (GlossaryFromDom)
import Data.GlossaryItem.Tag as Tag
import Data.GlossaryItemForUi as GlossaryItemForUi exposing (GlossaryItemForUi)
import Data.GlossaryItemId exposing (GlossaryItemId)
import Data.GlossaryItemsForUi as GlossaryItemsForUi exposing (GlossaryItemsForUi)
import Data.GlossaryTitle as GlossaryTitle exposing (GlossaryTitle)
import Data.GlossaryVersionNumber as GlossaryVersionNumber exposing (GlossaryVersionNumber)
import Data.TagDescription as TagDescription
import Data.TagId as TagId
import Data.TagsChanges exposing (TagsChanges)
import ElementIds
import Extras.HtmlTree as HtmlTree exposing (HtmlTree)
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


{-| Increment the version number for a GlossaryForUi.
-}
incrementVersionNumber : GlossaryForUi -> GlossaryForUi
incrementVersionNumber (GlossaryForUi glossaryForUi) =
    GlossaryForUi { glossaryForUi | versionNumber = GlossaryVersionNumber.increment glossaryForUi.versionNumber }


{-| Enable or disable showing of last updated dates for items.
-}
setEnableLastUpdatedDates : Bool -> GlossaryForUi -> GlossaryForUi
setEnableLastUpdatedDates enable (GlossaryForUi glossaryForUi) =
    GlossaryForUi { glossaryForUi | enableLastUpdatedDates = enable }


{-| Toggle showing of last updated dates for items.
-}
toggleEnableLastUpdatedDates : GlossaryForUi -> GlossaryForUi
toggleEnableLastUpdatedDates (GlossaryForUi glossaryForUi) =
    GlossaryForUi { glossaryForUi | enableLastUpdatedDates = not glossaryForUi.enableLastUpdatedDates }


{-| Enable or disable showing of the export menu.
-}
setEnableExportMenu : Bool -> GlossaryForUi -> GlossaryForUi
setEnableExportMenu enable (GlossaryForUi glossaryForUi) =
    GlossaryForUi { glossaryForUi | enableExportMenu = enable }


{-| Toggle showing of the export menu.
-}
toggleEnableExportMenu : GlossaryForUi -> GlossaryForUi
toggleEnableExportMenu (GlossaryForUi glossaryForUi) =
    GlossaryForUi { glossaryForUi | enableExportMenu = not glossaryForUi.enableExportMenu }


{-| Enable or disable showing of buttons for ordering items.
-}
setEnableOrderItemsButtons : Bool -> GlossaryForUi -> GlossaryForUi
setEnableOrderItemsButtons enable (GlossaryForUi glossaryForUi) =
    GlossaryForUi { glossaryForUi | enableOrderItemsButtons = enable }


{-| Toggle showing of buttons for ordering items.
-}
toggleEnableOrderItemsButtons : GlossaryForUi -> GlossaryForUi
toggleEnableOrderItemsButtons glossaryForUi =
    case glossaryForUi of
        GlossaryForUi glossary_ ->
            GlossaryForUi { glossary_ | enableOrderItemsButtons = not glossary_.enableOrderItemsButtons }


{-| Enable or disable showing help for making changes.
-}
setEnableHelpForMakingChanges : Bool -> GlossaryForUi -> GlossaryForUi
setEnableHelpForMakingChanges enable (GlossaryForUi glossary) =
    GlossaryForUi { glossary | enableHelpForMakingChanges = enable }


{-| Set the card width configuration for a GlossaryForUi.
-}
setCardWidth : CardWidth -> GlossaryForUi -> GlossaryForUi
setCardWidth cardWidth_ (GlossaryForUi glossaryForUi) =
    GlossaryForUi { glossaryForUi | cardWidth = cardWidth_ }


{-| Set the title for a GlossaryForUi.
-}
setTitle : GlossaryTitle -> GlossaryForUi -> GlossaryForUi
setTitle title_ (GlossaryForUi glossaryForUi) =
    GlossaryForUi { glossaryForUi | title = title_ }


{-| Set the about section for a GlossaryForUi.
-}
setAboutSection : AboutSection -> GlossaryForUi -> GlossaryForUi
setAboutSection aboutSection_ (GlossaryForUi glossaryForUi) =
    GlossaryForUi { glossaryForUi | aboutSection = aboutSection_ }


{-| Set the items for a GlossaryForUi.
-}
setItems : GlossaryItemsForUi -> GlossaryForUi -> GlossaryForUi
setItems items_ (GlossaryForUi glossaryForUi) =
    GlossaryForUi { glossaryForUi | items = items_ }


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
    -> GlossaryTitle
    -> AboutParagraph
    -> List AboutLink
    -> List DescribedTag
    -> List GlossaryItemForUi
    -> GlossaryVersionNumber
    -> GlossaryForUi
create enableLastUpdatedDates_ enableExportMenu_ enableOrderItemsButtons_ enableHelpForMakingChanges_ cardWidth_ title_ aboutParagraph aboutLinks describedTags itemsForHtml versionNumber_ =
    createWithDefaults
        (Just enableLastUpdatedDates_)
        (Just enableExportMenu_)
        (Just enableOrderItemsButtons_)
        (Just enableHelpForMakingChanges_)
        (Just cardWidth_)
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
    -> Maybe GlossaryTitle
    -> Maybe AboutParagraph
    -> Maybe (List AboutLink)
    -> Maybe (List DescribedTag)
    -> List GlossaryItemForUi
    -> Maybe GlossaryVersionNumber
    -> GlossaryForUi
createWithDefaults enableLastUpdatedDates_ enableExportMenu_ enableOrderItemsButtons_ enableHelpForMakingChanges_ cardWidth_ title_ aboutParagraph aboutLinks describedTags itemsForHtml versionNumber_ =
    let
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
        , title = Maybe.withDefault (GlossaryTitle.fromMarkdown I18n.elementNotFound) title_
        , aboutSection = aboutSection_
        , items = Result.withDefault GlossaryItemsForUi.empty items_
        , versionNumber = Maybe.withDefault GlossaryVersionNumber.initial versionNumber_
        }


{-| An encoder/decoder for a GlossaryForUi.
-}
codec : Codec GlossaryForUi
codec =
    Codec.object createWithDefaults
        |> Codec.optionalField "enableLastUpdatedDates" (enableLastUpdatedDates >> Just) Codec.bool
        |> Codec.optionalField "enableExportMenu" (enableExportMenu >> Just) Codec.bool
        |> Codec.optionalField "enableOrderItemsButtons" (enableOrderItemsButtons >> Just) Codec.bool
        |> Codec.optionalField "enableHelpForMakingChanges" (enableHelpForMakingChanges >> Just) Codec.bool
        |> Codec.optionalField "cardWidth" (cardWidth >> Just) CardWidth.codec
        |> Codec.optionalField "titleString" (title >> Just) GlossaryTitle.codec
        |> Codec.optionalField "aboutParagraph" (aboutSection >> .paragraph >> Just) (Codec.map AboutParagraph.fromMarkdown AboutParagraph.raw Codec.string)
        |> Codec.optionalField "aboutLinks" (aboutSection >> .links >> Just) (Codec.list AboutLink.codec)
        |> Codec.optionalField "tagsWithDescriptions"
            (items >> GlossaryItemsForUi.describedTags >> Just)
            (Codec.list
                (Codec.object
                    (\tagIdString tagString descriptionString ->
                        DescribedTag.create
                            (TagId.create tagIdString)
                            (Tag.fromMarkdown tagString)
                            (TagDescription.fromMarkdown descriptionString)
                    )
                    |> Codec.field "id" (DescribedTag.id >> TagId.toString) Codec.string
                    |> Codec.field "tag" (DescribedTag.tag >> Tag.raw) Codec.string
                    |> Codec.field "description" (DescribedTag.description >> TagDescription.raw) Codec.string
                    |> Codec.buildObject
                )
            )
        |> Codec.field "glossaryItems"
            (items >> GlossaryItemsForUi.orderedAlphabetically Nothing >> List.map Tuple.second)
            (Codec.list GlossaryItemForUi.codec)
        |> Codec.optionalNullableField "versionNumber" (versionNumber >> Just) GlossaryVersionNumber.codec
        |> Codec.buildObject


{-| The result of applying a sequence of changes to a glossary.
-}
type ApplyChangesResult
    = VersionsDoNotMatch
    | LogicalErrorWhenApplyingChanges String
    | ChangesApplied ( Maybe GlossaryItemId, GlossaryForUi )


{-| Apply a sequence of changes to a glossary, returning a new glossary or an error message.

A change can be inserting, updating, or removing an item, or modifying tags.

If the change is successful, the new glossary is returned along with the ID of the
item that was inserted, if any.

-}
applyChanges : GlossaryChangelist -> GlossaryForUi -> ApplyChangesResult
applyChanges changes glossaryForUi =
    if GlossaryChangelist.applyToVersionNumber changes /= versionNumber glossaryForUi then
        VersionsDoNotMatch

    else
        changes
            |> GlossaryChangelist.body
            |> List.foldl
                (\change -> Result.andThen (Tuple.second >> applyChange change))
                (Ok ( Nothing, incrementVersionNumber glossaryForUi ))
            |> (\result ->
                    case result of
                        Ok result_ ->
                            ChangesApplied result_

                        Err err ->
                            LogicalErrorWhenApplyingChanges err
               )


applyChange : GlossaryChange -> GlossaryForUi -> Result String ( Maybe GlossaryItemId, GlossaryForUi )
applyChange change glossaryForUi =
    case change of
        ToggleEnableLastUpdatedDates ->
            Ok <| ( Nothing, toggleEnableLastUpdatedDates glossaryForUi )

        ToggleEnableExportMenu ->
            Ok <| ( Nothing, toggleEnableExportMenu glossaryForUi )

        ToggleEnableOrderItemsButtons ->
            Ok ( Nothing, toggleEnableOrderItemsButtons glossaryForUi )

        SetTitle title_ ->
            Ok ( Nothing, setTitle title_ glossaryForUi )

        SetAboutSection aboutSection_ ->
            Ok ( Nothing, setAboutSection aboutSection_ glossaryForUi )

        SetCardWidth cardWidth_ ->
            Ok ( Nothing, setCardWidth cardWidth_ glossaryForUi )

        ChangeTags tagsChanges ->
            applyTagsChanges tagsChanges glossaryForUi
                |> Result.map (\newGlossary -> ( Nothing, newGlossary ))

        Insert item ->
            insert item glossaryForUi
                |> Result.map (\( newItemId, newGlossary ) -> ( Just newItemId, newGlossary ))

        Update item ->
            let
                itemId : GlossaryItemId
                itemId =
                    GlossaryItemForUi.id item
            in
            update itemId item glossaryForUi
                |> Result.map (\newGlossary -> ( Just itemId, newGlossary ))

        Remove itemId ->
            remove itemId glossaryForUi
                |> Result.map (\newGlossary -> ( Nothing, newGlossary ))


applyTagsChanges : TagsChanges -> GlossaryForUi -> Result String GlossaryForUi
applyTagsChanges tagsChanges glossaryForUi =
    glossaryForUi
        |> items
        |> GlossaryItemsForUi.applyTagsChanges tagsChanges
        |> Result.map (\items_ -> setItems items_ glossaryForUi)


{-| Insert an item.
-}
insert : GlossaryItemForUi -> GlossaryForUi -> Result String ( GlossaryItemId, GlossaryForUi )
insert item glossaryForUi =
    glossaryForUi
        |> items
        |> GlossaryItemsForUi.insert item
        |> Result.map (\( newItemId, items_ ) -> ( newItemId, setItems items_ glossaryForUi ))


{-| Update an item. Do nothing if there is no item with the given ID.
-}
update : GlossaryItemId -> GlossaryItemForUi -> GlossaryForUi -> Result String GlossaryForUi
update itemId item glossaryForUi =
    glossaryForUi
        |> items
        |> GlossaryItemsForUi.update itemId item
        |> Result.map (\items_ -> setItems items_ glossaryForUi)


{-| Remove the item associated with an ID. Do nothing if the ID is not found.
-}
remove : GlossaryItemId -> GlossaryForUi -> Result String GlossaryForUi
remove itemId glossaryForUi =
    glossaryForUi
        |> items
        |> GlossaryItemsForUi.remove itemId
        |> Result.map (\items_ -> setItems items_ glossaryForUi)


{-| Convert this glossary to a GlossaryFromDom.
-}
toGlossaryFromDom : GlossaryForUi -> GlossaryFromDom
toGlossaryFromDom (GlossaryForUi glossaryForUi) =
    { enableLastUpdatedDates = glossaryForUi.enableLastUpdatedDates
    , enableExportMenu = glossaryForUi.enableExportMenu
    , enableOrderItemsButtons = glossaryForUi.enableOrderItemsButtons
    , enableHelpForMakingChanges = glossaryForUi.enableHelpForMakingChanges
    , cardWidth = glossaryForUi.cardWidth
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


{-| Represent this glossary as an HTML tree, ready for writing back to the glossary's HTML file.
-}
toHtmlTree : GlossaryForUi -> HtmlTree
toHtmlTree (GlossaryForUi glossaryForUi) =
    let
        describedTags =
            GlossaryItemsForUi.describedTags glossaryForUi.items
    in
    HtmlTree.Node "div"
        True
        [ HtmlTree.Attribute "id" ElementIds.container
        , HtmlTree.boolAttribute "data-enable-help-for-making-changes" glossaryForUi.enableHelpForMakingChanges
        , HtmlTree.boolAttribute "data-enable-export-menu" glossaryForUi.enableExportMenu
        , HtmlTree.boolAttribute "data-enable-order-items-buttons" glossaryForUi.enableOrderItemsButtons
        , HtmlTree.boolAttribute "data-enable-last-updated-dates" glossaryForUi.enableLastUpdatedDates
        , CardWidth.toHtmlTreeAttribute glossaryForUi.cardWidth
        , GlossaryVersionNumber.toHtmlTreeAttribute glossaryForUi.versionNumber
        ]
        [ HtmlTree.Node "header"
            True
            []
            [ HtmlTree.Node "h1"
                True
                [ HtmlTree.Attribute "id" ElementIds.title ]
                [ HtmlTree.Leaf <| GlossaryTitle.raw glossaryForUi.title ]
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
                    [ HtmlTree.Leaf <| AboutParagraph.raw glossaryForUi.aboutSection.paragraph ]
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
                        glossaryForUi.aboutSection.links
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
                                    [ HtmlTree.Attribute "data-id" <| TagId.toString <| DescribedTag.id describedTag ]
                                    [ HtmlTree.Node "dt"
                                        False
                                        []
                                        [ HtmlTree.Leaf <| Tag.raw <| DescribedTag.tag describedTag ]
                                    , HtmlTree.Node "dd"
                                        False
                                        []
                                        [ HtmlTree.Leaf <| TagDescription.raw <| DescribedTag.description describedTag ]
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
                    (glossaryForUi.items
                        |> GlossaryItemsForUi.orderedAlphabetically Nothing
                        |> List.map (Tuple.second >> GlossaryItemForUi.toHtmlTree)
                    )
                ]
            ]
        , HtmlTree.Node "footer"
            True
            []
            I18n.builtUsingGlossaryPageTemplateHtmlTree
        ]
