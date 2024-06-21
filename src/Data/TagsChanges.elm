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
import Data.DescribedTag as DescribedTag exposing (DescribedTag)
import Data.TagId as TagId exposing (TagId)
import Data.TagIdDict as TagIdDict exposing (TagIdDict)


{-| A single change to the tags in a glossary.
-}
type TagsChange
    = Insertion DescribedTag
    | Update TagId DescribedTag
    | Removal TagId


tagsChangeCodec : Codec TagsChange
tagsChangeCodec =
    Codec.custom
        (\insertion_ update_ removal_ value ->
            case value of
                Insertion describedTag ->
                    insertion_ describedTag

                Update tagId describedTag ->
                    update_ tagId describedTag

                Removal tagId ->
                    removal_ tagId
        )
        |> Codec.variant1 "Insertion" Insertion DescribedTag.codec
        |> Codec.variant2 "Update" Update TagId.codec DescribedTag.codec
        |> Codec.variant1 "Removal" Removal TagId.codec
        |> Codec.buildCustom


type UpdateOrRemoval_
    = Update_ DescribedTag
    | Removal_


{-| A set of tags changes.
-}
type TagsChanges
    = TagsChanges
        { insertions : List DescribedTag
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
                    List.map Insertion changes.insertions

                updatesAndRemovals : List TagsChange
                updatesAndRemovals =
                    changes.updatesAndRemovals
                        |> TagIdDict.toList
                        |> List.map
                            (\( tagId, updateOrRemoval ) ->
                                case updateOrRemoval of
                                    Update_ describedTag ->
                                        Update tagId describedTag

                                    Removal_ ->
                                        Removal tagId
                            )
            in
            insertions ++ updatesAndRemovals


{-| Add an insertion to this set of tags changes.
-}
insert : DescribedTag -> TagsChanges -> TagsChanges
insert describedTag tagsChanges =
    case tagsChanges of
        TagsChanges changes ->
            TagsChanges { changes | insertions = describedTag :: changes.insertions }


{-| Add an update to this set of tags changes.
-}
update : TagId -> DescribedTag -> TagsChanges -> TagsChanges
update tagId describedTag tagsChanges =
    case tagsChanges of
        TagsChanges changes ->
            TagsChanges <|
                { changes
                    | updatesAndRemovals =
                        changes.updatesAndRemovals
                            |> TagIdDict.insert tagId (Update_ describedTag)
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
                Insertion describedTag ->
                    insert describedTag result

                Update tagId describedTag ->
                    update tagId describedTag result

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
