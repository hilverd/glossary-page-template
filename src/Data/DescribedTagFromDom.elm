module Data.DescribedTagFromDom exposing
    ( DescribedTagFromDom
    , create, codec
    )

{-| A tag used in a glossary together with its description, as sent to Elm by JavaScript when read from the DOM.

@docs DescribedTagFromDom


# Build

@docs create, codec

-}

import Codec exposing (Codec)


{-| A DescribedTagFromDom read from the DOM.
-}
type alias DescribedTagFromDom =
    { id : String
    , tag : String
    , description : String
    }


{-| Create a DescribedTagFromDom from its parts.
-}
create : String -> String -> String -> DescribedTagFromDom
create id_ tag_ description_ =
    { id = id_
    , tag = tag_
    , description = description_
    }


{-| Convert a DescribedTagFromDom to/from its JSON representation.
-}
codec : Codec DescribedTagFromDom
codec =
    Codec.object
        create
        |> Codec.field "id" .id Codec.string
        |> Codec.field "tag" .tag Codec.string
        |> Codec.field "description" .description Codec.string
        |> Codec.buildObject
