module Data.TagsChanges exposing
    ( TagsChange(..), TagsChanges
    , empty, insert, update, remove, codec
    , toList
    )

{-| A set of changes to the tags in a glossary.
This mainly exists to support tricky (but legal) combinations of changes such as a simultaneously renaming a tag X to Y and vice versa.


# Tags Changes

@docs TagsChange, TagsChanges


# Build

@docs empty, insert, update, remove, codec


# Query

@docs toList

-}

import Codec exposing (Codec)
import Data.GlossaryItem.Tag as Tag exposing (Tag)
import Data.TagDescription as TagDescription exposing (TagDescription)
import Data.TagId as TagId exposing (TagId)
import Data.TagIdDict as TagIdDict exposing (TagIdDict)


{-| A single change to the tags in a glossary.
-}
type TagsChange
    = Insertion Tag TagDescription
    | Update TagId Tag TagDescription
    | Removal TagId


tagsChangeCodec : Codec TagsChange
tagsChangeCodec =
    Codec.custom
        (\insertion_ update_ removal_ value ->
            case value of
                Insertion tag tagDescription ->
                    insertion_ tag tagDescription

                Update tagId tag tagDescription ->
                    update_ tagId tag tagDescription

                Removal tagId ->
                    removal_ tagId
        )
        |> Codec.variant2 "Insertion" Insertion Tag.codec TagDescription.codec
        |> Codec.variant3 "Update" Update TagId.codec Tag.codec TagDescription.codec
        |> Codec.variant1 "Removal" Removal TagId.codec
        |> Codec.buildCustom


type UpdateOrRemoval_
    = Update_ Tag TagDescription
    | Removal_


{-| A set of tags changes.
-}
type TagsChanges
    = TagsChanges
        { insertions : List { tag : Tag, description : TagDescription }
        , updatesAndRemovals : TagIdDict UpdateOrRemoval_
        }


{-| An empty set of tags changes.
-}
empty : TagsChanges
empty =
    TagsChanges { insertions = [], updatesAndRemovals = TagIdDict.empty }


{-| The list of changes in this `TagsChanges`.
-}
toList : TagsChanges -> List TagsChange
toList tagsChanges =
    case tagsChanges of
        TagsChanges changes ->
            let
                insertions : List TagsChange
                insertions =
                    changes.insertions
                        |> List.map (\{ tag, description } -> Insertion tag description)

                updatesAndRemovals : List TagsChange
                updatesAndRemovals =
                    changes.updatesAndRemovals
                        |> TagIdDict.toList
                        |> List.map
                            (\( tagId, updateOrRemoval ) ->
                                case updateOrRemoval of
                                    Update_ tag tagDescription ->
                                        Update tagId tag tagDescription

                                    Removal_ ->
                                        Removal tagId
                            )
            in
            insertions ++ updatesAndRemovals


{-| Add an insertion to this set of tags changes.
-}
insert : Tag -> TagDescription -> TagsChanges -> TagsChanges
insert tag tagDescription tagsChanges =
    case tagsChanges of
        TagsChanges changes ->
            TagsChanges <|
                { changes
                    | insertions =
                        { tag = tag, description = tagDescription }
                            :: changes.insertions
                }


{-| Add an update to this set of tags changes.
-}
update : TagId -> Tag -> TagDescription -> TagsChanges -> TagsChanges
update tagId tag tagDescription tagsChanges =
    case tagsChanges of
        TagsChanges changes ->
            TagsChanges <|
                { changes
                    | updatesAndRemovals =
                        changes.updatesAndRemovals
                            |> TagIdDict.insert
                                tagId
                                (Update_ tag tagDescription)
                }


{-| Add a removal to this set of tags changes.
-}
remove : TagId -> TagsChanges -> TagsChanges
remove tagId tagsChanges =
    case tagsChanges of
        TagsChanges changes ->
            TagsChanges <|
                { changes
                    | updatesAndRemovals =
                        TagIdDict.insert tagId Removal_ changes.updatesAndRemovals
                }


fromList : List TagsChange -> TagsChanges
fromList tagsChangeList =
    List.foldl
        (\tagsChange result ->
            case tagsChange of
                Insertion tag tagDescription ->
                    insert tag tagDescription result

                Update tagId tag tagDescription ->
                    update tagId tag tagDescription result

                Removal tagId ->
                    remove tagId result
        )
        empty
        tagsChangeList


{-| An encoder/decoder for sets of tags changes.
-}
codec : Codec TagsChanges
codec =
    Codec.map fromList toList (Codec.list tagsChangeCodec)
