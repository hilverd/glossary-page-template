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
import Data.AboutLink as AboutLink
import Data.AboutParagraph as AboutParagraph
import Data.CardWidth as CardWidth exposing (CardWidth)
import Data.DescribedTag as DescribedTag
import Data.DescribedTagFromDom as DescribedTagFromDom exposing (DescribedTagFromDom)
import Data.GlossaryChange exposing (GlossaryChange(..))
import Data.GlossaryItem.Tag as Tag
import Data.GlossaryItemFromDom as GlossaryItemFromDom exposing (GlossaryItemFromDom)
import Data.GlossaryItemId as GlossaryItemId exposing (GlossaryItemId)
import Data.GlossaryTitle as GlossaryTitle
import Data.TagDescription as TagDescription
import Data.TagId as TagId
import Data.TagsChanges as TagsChanges exposing (TagsChanges)
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
    , cardWidth : CardWidth
    , title : String
    , aboutParagraph : String
    , aboutLinks : List { href : String, body : String }
    , tags : List DescribedTagFromDom
    , items : List GlossaryItemFromDom
    , versionNumber : Int
    }


{-| Create a GlossaryFromDom from its parts.
-}
create :
    Bool
    -> Bool
    -> Bool
    -> Bool
    -> CardWidth
    -> String
    -> String
    -> List { href : String, body : String }
    -> List DescribedTagFromDom
    -> List GlossaryItemFromDom
    -> Int
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
        |> Codec.field "cardWidth" .cardWidth CardWidth.codec
        |> Codec.field "titleString" .title Codec.string
        |> Codec.field "aboutParagraph" .aboutParagraph Codec.string
        |> Codec.field "aboutLinks" .aboutLinks (Codec.list aboutLinkCodec)
        |> Codec.field "tagsWithDescriptions" .tags (Codec.list DescribedTagFromDom.codec)
        |> Codec.field "glossaryItems" .items (Codec.list GlossaryItemFromDom.codec)
        |> Codec.field "versionNumber" .versionNumber Codec.int
        |> Codec.buildObject


applyTagsChanges : TagsChanges -> GlossaryFromDom -> Result String GlossaryFromDom
applyTagsChanges tagsChanges glossaryFromDom =
    tagsChanges
        |> TagsChanges.toList
        |> List.foldl
            (\tagsChange result ->
                case ( tagsChange, result ) of
                    ( TagsChanges.Insertion describedTag, Ok glossaryFromDom_ ) ->
                        let
                            id : String
                            id =
                                describedTag |> DescribedTag.id |> TagId.toString

                            tag : String
                            tag =
                                describedTag |> DescribedTag.tag |> Tag.raw

                            description : String
                            description =
                                describedTag |> DescribedTag.description |> TagDescription.raw

                            describedTagFromDom : DescribedTagFromDom
                            describedTagFromDom =
                                { id = id
                                , tag = tag
                                , description = description
                                }
                        in
                        if List.any (.id >> (==) id) glossaryFromDom_.tags then
                            Err <| I18n.thereIsAlreadyATagWithId id

                        else if List.any (.tag >> (==) tag) glossaryFromDom_.tags then
                            Err <| I18n.thereIsAlreadyATag tag

                        else
                            let
                                tags_ : List DescribedTagFromDom
                                tags_ =
                                    describedTagFromDom :: glossaryFromDom_.tags
                            in
                            Ok { glossaryFromDom_ | tags = tags_ }

                    ( TagsChanges.Update tagId describedTag, Ok glossaryFromDom_ ) ->
                        let
                            id : String
                            id =
                                tagId |> TagId.toString

                            updatedTag : String
                            updatedTag =
                                describedTag |> DescribedTag.tag |> Tag.raw

                            updatedDescription : String
                            updatedDescription =
                                describedTag |> DescribedTag.description |> TagDescription.raw
                        in
                        if
                            List.any
                                (\existingTagFromDom ->
                                    existingTagFromDom.id == id && existingTagFromDom.tag == updatedTag
                                )
                                glossaryFromDom_.tags
                        then
                            Err <| I18n.thereIsAlreadyATag updatedTag

                        else if List.all (.id >> (/=) id) glossaryFromDom_.tags then
                            Err <| I18n.thereIsNoTagWithId id

                        else
                            glossaryFromDom_.tags
                                |> List.filter (.id >> (==) id)
                                |> List.head
                                |> Maybe.map
                                    (\oldDescribedTagFromDom ->
                                        let
                                            items_ : List GlossaryItemFromDom
                                            items_ =
                                                glossaryFromDom.items
                                                    |> List.map
                                                        (\glossaryItemFromDom ->
                                                            let
                                                                disambiguationTag_ : Maybe String
                                                                disambiguationTag_ =
                                                                    if glossaryItemFromDom.disambiguationTag == Just oldDescribedTagFromDom.tag then
                                                                        Just updatedTag

                                                                    else
                                                                        glossaryItemFromDom.disambiguationTag

                                                                normalTags_ =
                                                                    glossaryItemFromDom.normalTags
                                                                        |> List.map
                                                                            (\normalTag ->
                                                                                if normalTag == oldDescribedTagFromDom.tag then
                                                                                    updatedTag

                                                                                else
                                                                                    normalTag
                                                                            )
                                                            in
                                                            { glossaryItemFromDom
                                                                | disambiguationTag = disambiguationTag_
                                                                , normalTags = normalTags_
                                                            }
                                                        )

                                            tags_ =
                                                glossaryFromDom_.tags
                                                    |> List.map
                                                        (\existingTagFromDom ->
                                                            if existingTagFromDom.id == id then
                                                                { existingTagFromDom
                                                                    | tag = updatedTag
                                                                    , description = updatedDescription
                                                                }

                                                            else
                                                                existingTagFromDom
                                                        )
                                        in
                                        Ok
                                            { glossaryFromDom_
                                                | tags = tags_
                                                , items = items_
                                            }
                                    )
                                |> Maybe.withDefault
                                    (Err <| I18n.thereIsNoTagWithId id)

                    ( TagsChanges.Removal tagId, Ok glossaryFromDom_ ) ->
                        let
                            id : String
                            id =
                                tagId |> TagId.toString
                        in
                        if List.all (.id >> (/=) id) glossaryFromDom_.tags then
                            Err <| I18n.thereIsNoTagWithId id

                        else
                            let
                                tags_ =
                                    List.filter (.id >> (/=) id) glossaryFromDom_.tags
                            in
                            Ok { glossaryFromDom_ | tags = tags_ }

                    ( _, Err err ) ->
                        Err err
            )
            (Ok glossaryFromDom)


{-| Insert an item.
-}
insert : GlossaryItemFromDom -> GlossaryFromDom -> Result String ( GlossaryItemId, GlossaryFromDom )
insert glossaryItemFromDom glossaryFromDom =
    if List.any (.id >> (==) glossaryItemFromDom.id) glossaryFromDom.items then
        Err <| I18n.thereIsAlreadyAnItemWithId glossaryItemFromDom.id

    else
        let
            fragmentIdentifierForDisambiguatedPreferredTerm : String
            fragmentIdentifierForDisambiguatedPreferredTerm =
                GlossaryItemFromDom.disambiguatedPreferredTermIdString glossaryItemFromDom
        in
        if
            List.any
                (\glossaryItemFromDom_ ->
                    GlossaryItemFromDom.disambiguatedPreferredTermIdString glossaryItemFromDom_
                        == fragmentIdentifierForDisambiguatedPreferredTerm
                )
                glossaryFromDom.items
        then
            Err <| I18n.thereIsAlreadyAnItemWithDisambiguatedPreferredTermId fragmentIdentifierForDisambiguatedPreferredTerm

        else
            Ok
                ( glossaryItemFromDom.id |> GlossaryItemId.create
                , { glossaryFromDom
                    | items = glossaryItemFromDom :: glossaryFromDom.items
                  }
                )


{-| Update an item.
-}
update : GlossaryItemId -> GlossaryItemFromDom -> GlossaryFromDom -> Result String GlossaryFromDom
update itemId glossaryItemFromDom glossaryFromDom =
    glossaryFromDom
        |> remove itemId
        |> Result.andThen (insert glossaryItemFromDom)
        |> Result.map Tuple.second


{-| Remove the item associated with an ID.
-}
remove : GlossaryItemId -> GlossaryFromDom -> Result String GlossaryFromDom
remove itemId glossaryFromDom =
    let
        itemIdString =
            GlossaryItemId.toString itemId

        items_ =
            List.filter (.id >> (/=) itemIdString) glossaryFromDom.items
    in
    if List.length items_ == List.length glossaryFromDom.items then
        Ok { glossaryFromDom | items = items_ }

    else
        Err <| I18n.thereIsNoItemWithId itemIdString


applyChange : GlossaryChange -> GlossaryFromDom -> Result String ( Maybe GlossaryItemId, GlossaryFromDom )
applyChange change glossaryFromDom =
    case change of
        ToggleEnableLastUpdatedDates ->
            Ok <|
                ( Nothing
                , { glossaryFromDom | enableLastUpdatedDates = not glossaryFromDom.enableLastUpdatedDates }
                )

        ToggleEnableExportMenu ->
            Ok <| ( Nothing, { glossaryFromDom | enableExportMenu = not glossaryFromDom.enableExportMenu } )

        ToggleEnableOrderItemsButtons ->
            Ok ( Nothing, { glossaryFromDom | enableOrderItemsButtons = not glossaryFromDom.enableOrderItemsButtons } )

        SetTitle title_ ->
            Ok ( Nothing, { glossaryFromDom | title = GlossaryTitle.raw title_ } )

        SetAboutSection aboutSection_ ->
            Ok
                ( Nothing
                , { glossaryFromDom
                    | aboutParagraph = aboutSection_.paragraph |> AboutParagraph.raw
                    , aboutLinks =
                        aboutSection_.links
                            |> List.map
                                (\aboutLink ->
                                    { href = AboutLink.href aboutLink
                                    , body = AboutLink.body aboutLink
                                    }
                                )
                  }
                )

        SetCardWidth cardWidth_ ->
            Ok ( Nothing, { glossaryFromDom | cardWidth = cardWidth_ } )

        ChangeTags tagsChanges ->
            applyTagsChanges tagsChanges glossaryFromDom
                |> Result.map (\newGlossary -> ( Nothing, newGlossary ))

        Insert item ->
            insert item glossaryFromDom
                |> Result.map (\( newItemId, newGlossary ) -> ( Just newItemId, newGlossary ))

        Update item ->
            let
                itemId : GlossaryItemId
                itemId =
                    GlossaryItemId.create item.id
            in
            update itemId item glossaryFromDom
                |> Result.map (\newGlossary -> ( Just itemId, newGlossary ))

        Remove itemId ->
            remove itemId glossaryFromDom
                |> Result.map (\newGlossary -> ( Nothing, newGlossary ))


{-| Represent this GlossaryFromDom as an HTML tree, ready for writing back to the glossary's HTML file.
-}
toHtmlTree : GlossaryFromDom -> HtmlTree
toHtmlTree glossaryFromDom =
    HtmlTree.Node "div"
        True
        [ HtmlTree.Attribute "id" ElementIds.container
        , HtmlTree.boolAttribute "data-enable-help-for-making-changes" glossaryFromDom.enableHelpForMakingChanges
        , HtmlTree.boolAttribute "data-enable-export-menu" glossaryFromDom.enableExportMenu
        , HtmlTree.boolAttribute "data-enable-order-items-buttons" glossaryFromDom.enableOrderItemsButtons
        , HtmlTree.boolAttribute "data-enable-last-updated-dates" glossaryFromDom.enableLastUpdatedDates
        , CardWidth.toHtmlTreeAttribute glossaryFromDom.cardWidth
        , HtmlTree.Attribute "data-version-number" <| String.fromInt glossaryFromDom.versionNumber
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
            , HtmlTree.showIf (not <| List.isEmpty glossaryFromDom.tags) <|
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
                            glossaryFromDom.tags
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
