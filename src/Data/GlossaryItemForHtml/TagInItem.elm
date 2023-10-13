module Data.GlossaryItemForHtml.TagInItem exposing (TagInItem(..), decode, tag)

{-| A tag in a glossary item as retrieved from the HTML source, and/or suitable for representing as HTML.
This can be either a _normal_ tag or a _disambiguation_ tag.
A disambiguation tag can be used to distinguish items that have the same preferred term.


# Tags in Items

@docs TagInItem, decode, tag

-}

import Data.GlossaryItem.Tag as Tag exposing (Tag)
import Json.Decode as Decode exposing (Decoder)


{-| A tag in an item.
-}
type TagInItem
    = NormalTag Tag
    | DisambiguationTag Tag


{-| Decode a tag in an item from its JSON representation.
-}
decode : Bool -> Decoder TagInItem
decode enableMarkdownBasedSyntax =
    Decode.map2
        (\isDisambiguationTag tag_ ->
            if isDisambiguationTag then
                DisambiguationTag tag_

            else
                NormalTag tag_
        )
        (Decode.field "isDisambiguationTag" Decode.bool)
        (Decode.field "tag" <| Tag.decode enableMarkdownBasedSyntax)


{-| The underlying tag for a tag in an item.
-}
tag : TagInItem -> Tag
tag tagInItem =
    case tagInItem of
        NormalTag tag_ ->
            tag_

        DisambiguationTag tag_ ->
            tag_
