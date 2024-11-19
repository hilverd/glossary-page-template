module Data.GlossaryFromDom exposing
    ( GlossaryFromDom
    , codec
    , ApplyChangesResult(..), applyChanges
    , toHtmlTree
    )

{-| A glossary as sent to Elm by JavaScript when read from the DOM.

@docs GlossaryFromDom


# Build

@docs codec


# Apply Changes

@docs ApplyChangesResult, applyChanges


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
import Data.GlossaryChangelist as GlossaryChangelist exposing (GlossaryChangelist)
import Data.GlossaryItem.Tag as Tag
import Data.GlossaryItem.TermFromDom as TermFromDom
import Data.GlossaryItemFromDom as GlossaryItemFromDom exposing (GlossaryItemFromDom)
import Data.GlossaryItemId as GlossaryItemId exposing (GlossaryItemId)
import Data.GlossaryTitle as GlossaryTitle
import Data.GlossaryVersionNumber as GlossaryVersionNumber
import Data.TagDescription as TagDescription
import Data.TagId as TagId
import Data.TagsChanges as TagsChanges exposing (TagsChanges)
import Data.Theme as Theme exposing (Theme)
import Dict exposing (Dict)
import DuplicateRejectingDict
import ElementIds
import Extras.HtmlTree as HtmlTree exposing (HtmlTree)
import Internationalisation as I18n
import Set exposing (Set)


{-| A glossary read from the DOM.
-}
type alias GlossaryFromDom =
    { enableLastUpdatedDates : Bool
    , enableExportMenu : Bool
    , enableOrderItemsButtons : Bool
    , enableHelpForMakingChanges : Bool
    , cardWidth : CardWidth
    , defaultTheme : Theme
    , title : String
    , aboutParagraph : String
    , aboutLinks : List { href : String, body : String }
    , tags : List DescribedTagFromDom
    , items : List GlossaryItemFromDom
    , versionNumber : Int
    }


create :
    Bool
    -> Bool
    -> Bool
    -> Bool
    -> CardWidth
    -> Theme
    -> String
    -> String
    -> List { href : String, body : String }
    -> List DescribedTagFromDom
    -> List GlossaryItemFromDom
    -> Int
    -> GlossaryFromDom
create enableLastUpdatedDates_ enableExportMenu_ enableOrderItemsButtons_ enableHelpForMakingChanges_ cardWidth_ defaultTheme_ title_ aboutParagraph_ aboutLinks_ tags_ items_ versionNumber_ =
    { enableLastUpdatedDates = enableLastUpdatedDates_
    , enableExportMenu = enableExportMenu_
    , enableOrderItemsButtons = enableOrderItemsButtons_
    , enableHelpForMakingChanges = enableHelpForMakingChanges_
    , cardWidth = cardWidth_
    , defaultTheme = defaultTheme_
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
        |> Codec.field "defaultTheme" .defaultTheme Theme.codec
        |> Codec.field "titleString" .title Codec.string
        |> Codec.field "aboutParagraph" .aboutParagraph Codec.string
        |> Codec.field "aboutLinks" .aboutLinks (Codec.list aboutLinkCodec)
        |> Codec.field "tagsWithDescriptions" .tags (Codec.list DescribedTagFromDom.codec)
        |> Codec.field "glossaryItems" .items (Codec.list GlossaryItemFromDom.codec)
        |> Codec.field "versionNumber" .versionNumber Codec.int
        |> Codec.buildObject


applyTagsChanges : TagsChanges -> GlossaryFromDom -> Result String GlossaryFromDom
applyTagsChanges tagsChanges glossaryFromDom =
    let
        originalTagToTagId : Dict String String
        originalTagToTagId =
            glossaryFromDom.tags
                |> List.foldl
                    (\describedTag -> Dict.insert describedTag.tag describedTag.id)
                    Dict.empty

        tagIdToDescribedTagFromDom0 : Dict String DescribedTagFromDom
        tagIdToDescribedTagFromDom0 =
            List.foldl
                (\describedTag result ->
                    Dict.insert describedTag.id describedTag result
                )
                Dict.empty
                glossaryFromDom.tags

        tagIdToDescribedTagFromDom1 : Result String (Dict String DescribedTagFromDom)
        tagIdToDescribedTagFromDom1 =
            tagsChanges
                |> TagsChanges.toList
                |> List.foldl
                    (\tagsChange result ->
                        case ( tagsChange, result ) of
                            ( TagsChanges.Insertion describedTag, Ok tagIdToDescribedTagFromDom ) ->
                                let
                                    id : String
                                    id =
                                        describedTag |> DescribedTag.id |> TagId.toString

                                    tag : String
                                    tag =
                                        describedTag |> DescribedTag.tag |> Tag.raw

                                    describedTagFromDom : DescribedTagFromDom
                                    describedTagFromDom =
                                        { id = id
                                        , tag = tag
                                        , description = describedTag |> DescribedTag.description |> TagDescription.raw
                                        }
                                in
                                if Dict.member id tagIdToDescribedTagFromDom then
                                    Err <| I18n.thereIsAlreadyATagWithId id

                                else if tagIdToDescribedTagFromDom |> Dict.toList |> List.map Tuple.second |> List.any (.tag >> (==) tag) then
                                    Err <| I18n.thereIsAlreadyATag tag

                                else
                                    Ok <| Dict.insert id describedTagFromDom tagIdToDescribedTagFromDom

                            ( TagsChanges.Update tagId describedTag, Ok tagIdToDescribedTagFromDom ) ->
                                let
                                    id : String
                                    id =
                                        tagId |> TagId.toString

                                    updatedTag : String
                                    updatedTag =
                                        describedTag |> DescribedTag.tag |> Tag.raw

                                    describedTagFromDom : DescribedTagFromDom
                                    describedTagFromDom =
                                        { id = id
                                        , tag = updatedTag
                                        , description = describedTag |> DescribedTag.description |> TagDescription.raw
                                        }
                                in
                                if tagIdToDescribedTagFromDom |> Dict.keys |> List.all ((/=) id) then
                                    Err <| I18n.thereIsNoTagWithId id

                                else
                                    Ok (Dict.update id (always <| Just describedTagFromDom) tagIdToDescribedTagFromDom)

                            ( TagsChanges.Removal tagId, Ok tagIdToDescribedTagFromDom ) ->
                                let
                                    id : String
                                    id =
                                        tagId |> TagId.toString
                                in
                                if tagIdToDescribedTagFromDom |> Dict.keys |> List.all ((/=) id) then
                                    Err <| I18n.thereIsNoTagWithId id

                                else
                                    Ok <| Dict.remove id tagIdToDescribedTagFromDom

                            ( _, Err _ ) ->
                                result
                    )
                    (Ok tagIdToDescribedTagFromDom0)

        currentTagForPreviousTag : String -> Maybe String
        currentTagForPreviousTag previousTag =
            tagIdToDescribedTagFromDom1
                |> Result.map
                    (\tagIdToDescribedTagFromDom ->
                        Dict.get previousTag originalTagToTagId
                            |> Maybe.andThen (\originalTagId -> Dict.get originalTagId tagIdToDescribedTagFromDom)
                            |> Maybe.map .tag
                    )
                |> Result.withDefault (Just previousTag)
    in
    tagIdToDescribedTagFromDom1
        |> Result.map
            (\tagIdToDescribedTagFromDom ->
                let
                    items1 : List GlossaryItemFromDom
                    items1 =
                        glossaryFromDom.items
                            |> List.map
                                (\item ->
                                    { item
                                        | disambiguationTag = item.disambiguationTag |> Maybe.andThen currentTagForPreviousTag
                                        , normalTags = item.normalTags |> List.filterMap currentTagForPreviousTag
                                    }
                                )

                    tags1 : List DescribedTagFromDom
                    tags1 =
                        Dict.values tagIdToDescribedTagFromDom
                in
                { glossaryFromDom | tags = tags1, items = items1 }
            )


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
    if List.length items_ < List.length glossaryFromDom.items then
        Ok { glossaryFromDom | items = items_ }

    else
        Err <| I18n.thereIsNoItemWithId itemIdString


{-| Increment the version number for a GlossaryForUi.
-}
incrementVersionNumber : GlossaryFromDom -> GlossaryFromDom
incrementVersionNumber glossaryFromDom =
    { glossaryFromDom
        | versionNumber =
            glossaryFromDom.versionNumber
                |> GlossaryVersionNumber.create
                |> GlossaryVersionNumber.increment
                |> GlossaryVersionNumber.toInt
    }


{-| The result of applying a sequence of changes to a glossary.
-}
type ApplyChangesResult
    = VersionsDoNotMatch
    | LogicalErrorWhenApplyingChanges String
    | ChangesApplied ( Maybe GlossaryItemId, GlossaryFromDom )


{-| Apply a sequence of changes to a glossary, returning a new glossary or an error message.

A change can be inserting, updating, or removing an item, or modifying tags.

If the change is successful, the new glossary is returned along with the ID of the
item that was inserted, if any.

-}
applyChanges : GlossaryChangelist -> GlossaryFromDom -> ApplyChangesResult
applyChanges changes glossaryFromDom =
    let
        currentVersionNumber =
            GlossaryVersionNumber.create glossaryFromDom.versionNumber
    in
    if GlossaryChangelist.applyToVersionNumber changes /= currentVersionNumber then
        VersionsDoNotMatch

    else
        changes
            |> GlossaryChangelist.body
            |> List.foldl
                (\change -> Result.andThen (Tuple.second >> applyChange change))
                (Ok ( Nothing, incrementVersionNumber glossaryFromDom ))
            |> Result.andThen validateAfterApplyingChanges
            |> (\result ->
                    case result of
                        Ok result_ ->
                            let
                                sortedItems =
                                    result_
                                        |> Tuple.second
                                        |> .items
                                        |> List.sortBy GlossaryItemFromDom.disambiguatedPreferredTermIdString
                            in
                            result_
                                |> Tuple.mapSecond (\glossaryFromDom_ -> { glossaryFromDom_ | items = sortedItems })
                                |> ChangesApplied

                        Err err ->
                            LogicalErrorWhenApplyingChanges err
               )


validateAfterApplyingChanges : ( Maybe GlossaryItemId, GlossaryFromDom ) -> Result String ( Maybe GlossaryItemId, GlossaryFromDom )
validateAfterApplyingChanges ( maybeGlossaryItemId, glossaryFromDom ) =
    let
        reservedTerms : List String
        reservedTerms =
            glossaryFromDom.items
                |> List.map (\item -> item.preferredTerm :: item.alternativeTerms)
                |> List.concat
                |> List.filter (TermFromDom.id >> ElementIds.reserved)
                |> List.map .body

        itemsWithDuplicateAlternativeTerms : List { preferredTerm : String, alternativeTerm : String }
        itemsWithDuplicateAlternativeTerms =
            glossaryFromDom.items
                |> List.filterMap
                    (\glossaryItemFromDom ->
                        case
                            glossaryItemFromDom.alternativeTerms
                                |> List.foldl
                                    (\alternativeTermFromDom ->
                                        DuplicateRejectingDict.insert alternativeTermFromDom.body ()
                                    )
                                    DuplicateRejectingDict.empty
                                |> DuplicateRejectingDict.toResult
                        of
                            Ok _ ->
                                Nothing

                            Err { key } ->
                                Just
                                    { preferredTerm = glossaryItemFromDom.preferredTerm.body
                                    , alternativeTerm = key
                                    }
                    )

        duplicateDisambiguatedPreferredTermFragmentIdentifier : Maybe String
        duplicateDisambiguatedPreferredTermFragmentIdentifier =
            case
                glossaryFromDom.items
                    |> List.foldl
                        (\glossaryItemFromDom ->
                            let
                                fragmentIdentifier =
                                    GlossaryItemFromDom.disambiguatedPreferredTermIdString glossaryItemFromDom
                            in
                            DuplicateRejectingDict.insert
                                fragmentIdentifier
                                ()
                        )
                        DuplicateRejectingDict.empty
                    |> DuplicateRejectingDict.toResult
            of
                Ok _ ->
                    Nothing

                Err { key } ->
                    Just key

        alternativeTerms : Set String
        alternativeTerms =
            glossaryFromDom.items
                |> List.foldl
                    (\glossaryItemFromDom result ->
                        glossaryItemFromDom.alternativeTerms
                            |> List.foldl
                                (.body >> Set.insert)
                                result
                    )
                    Set.empty

        preferredTermsThatAlsoAppearAsAnAlternativeTerm : List String
        preferredTermsThatAlsoAppearAsAnAlternativeTerm =
            glossaryFromDom.items
                |> List.filterMap
                    (\glossaryItemFromDom ->
                        let
                            preferredTerm =
                                glossaryItemFromDom.preferredTerm.body
                        in
                        if Set.member preferredTerm alternativeTerms then
                            Just preferredTerm

                        else
                            Nothing
                    )
    in
    case
        ( ( List.head reservedTerms
          , List.head itemsWithDuplicateAlternativeTerms
          )
        , ( duplicateDisambiguatedPreferredTermFragmentIdentifier
          , List.head preferredTermsThatAlsoAppearAsAnAlternativeTerm
          )
        )
    of
        ( ( Just reservedTerm, _ ), _ ) ->
            Err <| I18n.thisTermIsReserved ++ ": " ++ reservedTerm

        ( ( _, Just { preferredTerm, alternativeTerm } ), _ ) ->
            Err <| I18n.thisAlternativeTermOccursMultipleTimes alternativeTerm preferredTerm

        ( _, ( Just duplicateDisambiguatedPreferredTermFragmentIdentifier_, _ ) ) ->
            Err <| I18n.thereAreMultipleItemsWithTheSameDisambiguatedPreferredTerm ++ " \"" ++ duplicateDisambiguatedPreferredTermFragmentIdentifier_ ++ "\""

        ( _, ( _, Just preferredTermThatAlsoAppearsAsAnAlternativeTerm ) ) ->
            Err <| I18n.preferredTermCannotAlsoAppearAsAnAlternativeTerm preferredTermThatAlsoAppearsAsAnAlternativeTerm

        _ ->
            Ok ( maybeGlossaryItemId, glossaryFromDom )


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

        SetDefaultTheme theme_ ->
            Ok ( Nothing, { glossaryFromDom | defaultTheme = theme_ } )

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
        , Theme.toHtmlTreeAttribute glossaryFromDom.defaultTheme
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
