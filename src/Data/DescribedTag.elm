module Data.DescribedTag exposing (DescribedTag, create, id, tag, description, codec)

{-| A tag used in a glossary together with its description.

@docs DescribedTag, create, id, tag, description, codec

-}

import Codec exposing (Codec)
import Data.GlossaryItem.Tag as Tag exposing (Tag)
import Data.TagDescription as TagDescription exposing (TagDescription)
import Data.TagId as TagId exposing (TagId)


{-| A tag together with its description.
-}
type DescribedTag
    = DescribedTag
        { id : TagId
        , tag : Tag
        , description : TagDescription
        }


{-| Create a DescribedTag from its parts.
-}
create : TagId -> Tag -> TagDescription -> DescribedTag
create id_ tag_ description_ =
    DescribedTag
        { id = id_
        , tag = tag_
        , description = description_
        }


{-| The ID of a DescribedTag.
-}
id : DescribedTag -> TagId
id (DescribedTag describedTag) =
    describedTag.id


{-| The tag of a DescribedTag.
-}
tag : DescribedTag -> Tag
tag (DescribedTag describedTag) =
    describedTag.tag


{-| The description of a DescribedTag.
-}
description : DescribedTag -> TagDescription
description (DescribedTag describedTag) =
    describedTag.description


{-| Encode/decode a DescribedTag to/from its JSON representation.
-}
codec : Codec DescribedTag
codec =
    Codec.object
        create
        |> Codec.field "id" id TagId.codec
        |> Codec.field "tag" tag Tag.codec
        |> Codec.field "description" description TagDescription.codec
        |> Codec.buildObject
