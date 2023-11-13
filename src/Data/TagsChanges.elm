module Data.TagsChanges exposing
    ( TagsChange(..), TagsChanges
    , empty, insert, update, remove
    , toList
    )

{-| A set of changes to the tags in a glossary.
This mainly exists to support tricky (but legal) combinations of changes such as a simultaneously renaming a tag X to Y and vice versa.


# Tags Changes

@docs TagsChange, TagsChanges


# Build

@docs empty, insert, update, remove


# Query

@docs toList

-}

import Data.GlossaryItem.Tag exposing (Tag)
import Data.TagDescription exposing (TagDescription)
import Data.TagId exposing (TagId)
import Data.TagIdDict as TagIdDict exposing (TagIdDict)


{-| A single change to the tags in a glossary.
-}
type TagsChange
    = Insertion Tag TagDescription
    | Update TagId Tag TagDescription
    | Removal TagId


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
