module Data.GlossaryTags exposing
    ( GlossaryTags
    , empty, fromList
    , tagById, tagIdByRawTag, tagDescriptionById
    )

{-| The tags for a glossary.


# Glossary Tags

@docs GlossaryTags


# Build

@docs empty, fromList


# Query

@docs tagById, tagIdByRawTag, tagDescriptionById

-}

import Data.DescribedTag as DescribedTag exposing (DescribedTag)
import Data.GlossaryItem.Tag as Tag exposing (Tag)
import Data.TagDescription exposing (TagDescription)
import Data.TagId exposing (TagId)
import Data.TagIdDict as TagIdDict exposing (TagIdDict)
import Dict exposing (Dict)
import DuplicateRejectingDict exposing (DuplicateRejectingDict)
import Internationalisation as I18n


{-| A set of tags for a glossary.
-}
type GlossaryTags
    = GlossaryTags
        { tagById : TagIdDict Tag
        , tagIdByRawTag : Dict String TagId
        , tagDescriptionById : TagIdDict TagDescription
        }


{-| The empty set of glossary tags.
-}
empty : GlossaryTags
empty =
    GlossaryTags
        { tagById = TagIdDict.empty
        , tagIdByRawTag = Dict.empty
        , tagDescriptionById = TagIdDict.empty
        }


{-| Convert a list of DescribedTags into a `GlossaryTags`.
-}
fromList : List DescribedTag -> Result String GlossaryTags
fromList describedTags_ =
    let
        tagById_ : TagIdDict Tag
        tagById_ =
            describedTags_
                |> List.map
                    (\describedTag ->
                        ( DescribedTag.id describedTag, DescribedTag.tag describedTag )
                    )
                |> TagIdDict.fromList

        tagIdByRawTagRejecting : DuplicateRejectingDict String TagId
        tagIdByRawTagRejecting =
            TagIdDict.foldl
                (\tagId tag ->
                    DuplicateRejectingDict.insert (Tag.raw tag) tagId
                )
                DuplicateRejectingDict.empty
                tagById_
    in
    tagIdByRawTagRejecting
        |> DuplicateRejectingDict.toResult
        |> Result.mapError (\{ key } -> I18n.tagAppearsMultipleTimes key)
        |> Result.map
            (\tagIdByRawTag_ ->
                let
                    tagDescriptionById_ : TagIdDict TagDescription
                    tagDescriptionById_ =
                        describedTags_
                            |> List.foldl
                                (\describedTag result ->
                                    tagIdByRawTagRejecting
                                        |> DuplicateRejectingDict.get (describedTag |> DescribedTag.tag |> Tag.raw)
                                        |> Maybe.map (\tagId -> TagIdDict.insert tagId (DescribedTag.description describedTag) result)
                                        |> Maybe.withDefault result
                                )
                                TagIdDict.empty
                in
                GlossaryTags
                    { tagById = tagById_
                    , tagIdByRawTag = tagIdByRawTag_
                    , tagDescriptionById = tagDescriptionById_
                    }
            )


{-| A dictionary mapping each tag ID to the corresponding tag.
-}
tagById : GlossaryTags -> TagIdDict Tag
tagById (GlossaryTags tags) =
    tags.tagById


{-| A dictionary mapping each tag ID to its raw tag.
-}
tagIdByRawTag : GlossaryTags -> Dict String TagId
tagIdByRawTag (GlossaryTags tags) =
    tags.tagIdByRawTag


{-| A dictionary mapping each tag ID to the corresponding tag description.
-}
tagDescriptionById : GlossaryTags -> TagIdDict TagDescription
tagDescriptionById (GlossaryTags tags) =
    tags.tagDescriptionById
