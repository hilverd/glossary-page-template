module Data.DescribedTag exposing (DescribedTag, create, tag, description, codec)

{-| A tag used in a glossary together with its description.

@docs DescribedTag, create, tag, description, codec

-}

import Codec exposing (Codec)
import Data.GlossaryItem.Tag as Tag exposing (Tag)
import Data.TagDescription as TagDescription exposing (TagDescription)


{-| A tag together with its description.
-}
type DescribedTag
    = DescribedTag
        { tag : Tag
        , description : TagDescription
        }


{-| Create a DescribedTag from its parts.
-}
create : Tag -> TagDescription -> DescribedTag
create tag_ description_ =
    DescribedTag
        { tag = tag_
        , description = description_
        }


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
        |> Codec.field "tag" tag Tag.codec
        |> Codec.field "description" description TagDescription.codec
        |> Codec.buildObject
