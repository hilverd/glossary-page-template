module Data.Glossary exposing
    ( Glossary
    , create, codec, setEnableLastUpdatedDates, toggleEnableLastUpdatedDates, setEnableExportMenu, toggleEnableExportMenu, setEnableOrderItemsButtons, toggleEnableOrderItemsButtons, setEnableHelpForMakingChanges, setCardWidth, setTitle, setAboutSection, setItems
    , ApplyChangesResult(..), applyChanges
    , enableLastUpdatedDates, enableExportMenu, enableOrderItemsButtons, enableHelpForMakingChanges, cardWidth, title, aboutSection, items, versionNumber
    , toHtmlTree
    )

{-| A glossary.


# Glossary

@docs Glossary


# Build

@docs create, codec, setEnableLastUpdatedDates, toggleEnableLastUpdatedDates, setEnableExportMenu, toggleEnableExportMenu, setEnableOrderItemsButtons, toggleEnableOrderItemsButtons, setEnableHelpForMakingChanges, setCardWidth, setTitle, setAboutSection, setItems


# Apply Changes

@docs ApplyChangesResult, applyChanges


# Query

@docs enableLastUpdatedDates, enableExportMenu, enableOrderItemsButtons, enableHelpForMakingChanges, cardWidth, title, aboutSection, items, versionNumber


# Export

@docs toHtmlTree

-}

import Codec exposing (Codec)
import Data.AboutLink as AboutLink exposing (AboutLink)
import Data.AboutParagraph as AboutParagraph exposing (AboutParagraph)
import Data.AboutSection exposing (AboutSection)
import Data.CardWidth as CardWidth exposing (CardWidth)
import Data.DescribedTag as DescribedTag exposing (DescribedTag)
import Data.GlossaryChange exposing (GlossaryChange(..))
import Data.GlossaryChangelist as GlossaryChangelist exposing (GlossaryChangelist)
import Data.GlossaryItem.Tag as Tag exposing (Tag)
import Data.GlossaryItemForHtml as GlossaryItemForHtml exposing (GlossaryItemForHtml)
import Data.GlossaryItemId exposing (GlossaryItemId)
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems, tagsWithDescriptions)
import Data.GlossaryTitle as GlossaryTitle exposing (GlossaryTitle)
import Data.GlossaryVersionNumber as GlossaryVersionNumber exposing (GlossaryVersionNumber)
import Data.TagDescription as TagDescription exposing (TagDescription)
import Data.TagsChanges exposing (TagsChanges)
import ElementIds
import Extras.HtmlTree as HtmlTree exposing (HtmlTree)
import Internationalisation as I18n


{-| A glossary.
-}
type Glossary
    = Glossary
        { enableLastUpdatedDates : Bool
        , enableExportMenu : Bool
        , enableOrderItemsButtons : Bool
        , enableHelpForMakingChanges : Bool
        , cardWidth : CardWidth
        , title : GlossaryTitle
        , aboutSection : AboutSection
        , items : GlossaryItems
        , versionNumber : GlossaryVersionNumber
        }


{-| Whether or not showing last updated dates for items is enabled.
-}
enableLastUpdatedDates : Glossary -> Bool
enableLastUpdatedDates (Glossary glossary_) =
    glossary_.enableLastUpdatedDates


{-| Whether or not showing the export menu is enabled.
-}
enableExportMenu : Glossary -> Bool
enableExportMenu (Glossary glossary) =
    glossary.enableExportMenu


{-| Whether or not showing buttons for ordering items is enabled.
-}
enableOrderItemsButtons : Glossary -> Bool
enableOrderItemsButtons (Glossary glossary) =
    glossary.enableOrderItemsButtons


{-| Whether or not help for making changes is enabled.
-}
enableHelpForMakingChanges : Glossary -> Bool
enableHelpForMakingChanges (Glossary glossary) =
    glossary.enableHelpForMakingChanges


{-| Get the card width configuration for a glossary.
-}
cardWidth : Glossary -> CardWidth
cardWidth (Glossary glossary) =
    glossary.cardWidth


{-| Get the title for a glossary.
-}
title : Glossary -> GlossaryTitle
title (Glossary glossary) =
    glossary.title


{-| Get the about section for a glossary.
-}
aboutSection : Glossary -> AboutSection
aboutSection (Glossary glossary) =
    glossary.aboutSection


{-| Get the items for a glossary.
-}
items : Glossary -> GlossaryItems
items (Glossary glossary) =
    glossary.items


{-| Get the version number for a glossary.
-}
versionNumber : Glossary -> GlossaryVersionNumber
versionNumber (Glossary glossary) =
    glossary.versionNumber


{-| Increment the version number for a glossary.
-}
incrementVersionNumber : Glossary -> Glossary
incrementVersionNumber (Glossary glossary) =
    Glossary { glossary | versionNumber = GlossaryVersionNumber.increment glossary.versionNumber }


{-| Enable or disable showing of last updated dates for items.
-}
setEnableLastUpdatedDates : Bool -> Glossary -> Glossary
setEnableLastUpdatedDates enable (Glossary glossary) =
    Glossary { glossary | enableLastUpdatedDates = enable }


{-| Toggle showing of last updated dates for items.
-}
toggleEnableLastUpdatedDates : Glossary -> Glossary
toggleEnableLastUpdatedDates (Glossary glossary) =
    Glossary { glossary | enableLastUpdatedDates = not glossary.enableLastUpdatedDates }


{-| Enable or disable showing of the export menu.
-}
setEnableExportMenu : Bool -> Glossary -> Glossary
setEnableExportMenu enable (Glossary glossary) =
    Glossary { glossary | enableExportMenu = enable }


{-| Toggle showing of the export menu.
-}
toggleEnableExportMenu : Glossary -> Glossary
toggleEnableExportMenu (Glossary glossary) =
    Glossary { glossary | enableExportMenu = not glossary.enableExportMenu }


{-| Enable or disable showing of buttons for ordering items.
-}
setEnableOrderItemsButtons : Bool -> Glossary -> Glossary
setEnableOrderItemsButtons enable (Glossary glossary) =
    Glossary { glossary | enableOrderItemsButtons = enable }


{-| Toggle showing of buttons for ordering items.
-}
toggleEnableOrderItemsButtons : Glossary -> Glossary
toggleEnableOrderItemsButtons glossary =
    case glossary of
        Glossary glossary_ ->
            Glossary { glossary_ | enableOrderItemsButtons = not glossary_.enableOrderItemsButtons }


{-| Enable or disable showing help for making changes.
-}
setEnableHelpForMakingChanges : Bool -> Glossary -> Glossary
setEnableHelpForMakingChanges enable (Glossary glossary) =
    Glossary { glossary | enableHelpForMakingChanges = enable }


{-| Set the card width configuration for a glossary.
-}
setCardWidth : CardWidth -> Glossary -> Glossary
setCardWidth cardWidth_ (Glossary glossary) =
    Glossary { glossary | cardWidth = cardWidth_ }


{-| Set the title for a glossary.
-}
setTitle : GlossaryTitle -> Glossary -> Glossary
setTitle title_ (Glossary glossary) =
    Glossary { glossary | title = title_ }


{-| Set the about section for a glossary.
-}
setAboutSection : AboutSection -> Glossary -> Glossary
setAboutSection aboutSection_ (Glossary glossary) =
    Glossary { glossary | aboutSection = aboutSection_ }


{-| Sets the items for a glossary.
-}
setItems : GlossaryItems -> Glossary -> Glossary
setItems items_ (Glossary glossary) =
    Glossary { glossary | items = items_ }


{-| Creates a new glossary with the given configuration.
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
    -> List GlossaryItemForHtml
    -> GlossaryVersionNumber
    -> Glossary
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
    -> List GlossaryItemForHtml
    -> Maybe GlossaryVersionNumber
    -> Glossary
createWithDefaults enableLastUpdatedDates_ enableExportMenu_ enableOrderItemsButtons_ enableHelpForMakingChanges_ cardWidth_ title_ aboutParagraph aboutLinks describedTags itemsForHtml versionNumber_ =
    let
        aboutSection_ =
            { paragraph = Maybe.withDefault (AboutParagraph.fromMarkdown I18n.elementNotFound) aboutParagraph
            , links = Maybe.withDefault [] aboutLinks
            }

        items_ : Result String GlossaryItems
        items_ =
            GlossaryItems.fromList (Maybe.withDefault [] describedTags) itemsForHtml
    in
    Glossary
        { enableLastUpdatedDates = Maybe.withDefault False enableLastUpdatedDates_
        , enableExportMenu = Maybe.withDefault True enableExportMenu_
        , enableOrderItemsButtons = Maybe.withDefault True enableOrderItemsButtons_
        , enableHelpForMakingChanges = Maybe.withDefault False enableHelpForMakingChanges_
        , cardWidth = Maybe.withDefault CardWidth.Compact cardWidth_
        , title = Maybe.withDefault (GlossaryTitle.fromMarkdown I18n.elementNotFound) title_
        , aboutSection = aboutSection_
        , items = Result.withDefault GlossaryItems.empty items_
        , versionNumber = Maybe.withDefault GlossaryVersionNumber.initial versionNumber_
        }


{-| An encoder/decoder for a glossary.
-}
codec : Codec Glossary
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
            (items >> GlossaryItems.tagsWithDescriptions >> Just)
            (Codec.list
                (Codec.object
                    (\tagString descriptionString ->
                        DescribedTag.create
                            (Tag.fromMarkdown tagString)
                            (TagDescription.fromMarkdown descriptionString)
                    )
                    |> Codec.field "tag" (DescribedTag.tag >> Tag.raw) Codec.string
                    |> Codec.field "description" (DescribedTag.description >> TagDescription.raw) Codec.string
                    |> Codec.buildObject
                )
            )
        |> Codec.field "glossaryItems"
            (items >> GlossaryItems.orderedAlphabetically Nothing >> List.map Tuple.second)
            (Codec.list GlossaryItemForHtml.codec)
        |> Codec.optionalNullableField "versionNumber" (versionNumber >> Just) GlossaryVersionNumber.codec
        |> Codec.buildObject


{-| The result of applying a sequence of changes to a glossary.
-}
type ApplyChangesResult
    = VersionsDoNotMatch
    | LogicalErrorWhenApplyingChanges String
    | ChangesApplied ( Maybe GlossaryItemId, Glossary )


{-| Apply a sequence of changes to a glossary, returning a new glossary or an error message.

A change can be inserting, updating, or removing an item, or modifying tags.

If the change is successful, the new glossary is returned along with the ID of the
item that was inserted, if any.

-}
applyChanges : GlossaryChangelist -> Glossary -> ApplyChangesResult
applyChanges changes glossary =
    if GlossaryChangelist.applyToVersionNumber changes /= versionNumber glossary then
        VersionsDoNotMatch

    else
        changes
            |> GlossaryChangelist.body
            |> List.foldl
                (\change -> Result.andThen (Tuple.second >> applyChange change))
                (Ok ( Nothing, incrementVersionNumber glossary ))
            |> (\result ->
                    case result of
                        Ok result_ ->
                            ChangesApplied result_

                        Err err ->
                            LogicalErrorWhenApplyingChanges err
               )


applyChange : GlossaryChange -> Glossary -> Result String ( Maybe GlossaryItemId, Glossary )
applyChange change glossary =
    case change of
        ToggleEnableLastUpdatedDates ->
            Ok <| ( Nothing, toggleEnableLastUpdatedDates glossary )

        ToggleEnableExportMenu ->
            Ok <| ( Nothing, toggleEnableExportMenu glossary )

        ToggleEnableOrderItemsButtons ->
            Ok ( Nothing, toggleEnableOrderItemsButtons glossary )

        SetTitle title_ ->
            Ok ( Nothing, setTitle title_ glossary )

        SetAboutSection aboutSection_ ->
            Ok ( Nothing, setAboutSection aboutSection_ glossary )

        SetCardWidth cardWidth_ ->
            Ok ( Nothing, setCardWidth cardWidth_ glossary )

        ChangeTags tagsChanges ->
            applyTagsChanges tagsChanges glossary
                |> Result.map (\newGlossary -> ( Nothing, newGlossary ))

        Insert item ->
            insert item glossary
                |> Result.map (\( newItemId, newGlossary ) -> ( Just newItemId, newGlossary ))

        Update item ->
            let
                itemId : GlossaryItemId
                itemId =
                    GlossaryItemForHtml.id item
            in
            update itemId item glossary
                |> Result.map (\newGlossary -> ( Just itemId, newGlossary ))

        Remove itemId ->
            remove itemId glossary
                |> Result.map (\newGlossary -> ( Nothing, newGlossary ))


applyTagsChanges : TagsChanges -> Glossary -> Result String Glossary
applyTagsChanges tagsChanges glossary =
    glossary
        |> items
        |> GlossaryItems.applyTagsChanges tagsChanges
        |> Result.map (\items_ -> setItems items_ glossary)


{-| Insert an item.
-}
insert : GlossaryItemForHtml -> Glossary -> Result String ( GlossaryItemId, Glossary )
insert item glossary =
    glossary
        |> items
        |> GlossaryItems.insert item
        |> Result.map (\( newItemId, items_ ) -> ( newItemId, setItems items_ glossary ))


{-| Update an item. Do nothing if there is no item with the given ID.
-}
update : GlossaryItemId -> GlossaryItemForHtml -> Glossary -> Result String Glossary
update itemId item glossary =
    glossary
        |> items
        |> GlossaryItems.update itemId item
        |> Result.map (\items_ -> setItems items_ glossary)


{-| Remove the item associated with an ID. Do nothing if the ID is not found.
-}
remove : GlossaryItemId -> Glossary -> Result String Glossary
remove itemId glossary =
    glossary
        |> items
        |> GlossaryItems.remove itemId
        |> Result.map (\items_ -> setItems items_ glossary)


{-| Represent this glossary as an HTML tree, ready for writing back to the glossary's HTML file.
-}
toHtmlTree : Glossary -> HtmlTree
toHtmlTree (Glossary glossary) =
    let
        tagsWithDescriptions =
            GlossaryItems.tagsWithDescriptions glossary.items
    in
    HtmlTree.Node "div"
        True
        [ HtmlTree.Attribute "id" ElementIds.container
        , HtmlTree.boolAttribute "data-enable-help-for-making-changes" glossary.enableHelpForMakingChanges
        , HtmlTree.boolAttribute "data-enable-export-menu" glossary.enableExportMenu
        , HtmlTree.boolAttribute "data-enable-order-items-buttons" glossary.enableOrderItemsButtons
        , HtmlTree.boolAttribute "data-enable-last-updated-dates" glossary.enableLastUpdatedDates
        , CardWidth.toHtmlTreeAttribute glossary.cardWidth
        , GlossaryVersionNumber.toHtmlTreeAttribute glossary.versionNumber
        ]
        [ HtmlTree.Node "header"
            True
            []
            [ HtmlTree.Node "h1"
                True
                [ HtmlTree.Attribute "id" ElementIds.title ]
                [ HtmlTree.Leaf <| GlossaryTitle.raw glossary.title ]
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
                    [ HtmlTree.Leaf <| AboutParagraph.raw glossary.aboutSection.paragraph ]
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
                        glossary.aboutSection.links
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
                            (\describedTag ->
                                HtmlTree.Node "div"
                                    True
                                    []
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
                            tagsWithDescriptions
                        )
                    ]
            , HtmlTree.Node "article"
                True
                [ HtmlTree.Attribute "id" ElementIds.items ]
                [ HtmlTree.Node "dl"
                    True
                    []
                    (glossary.items
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
