module Data.GlossaryItem.TermFromDom exposing
    ( TermFromDom
    , create, codec, id
    )

{-| A glossary term as sent to Elm by JavaScript when read from the DOM.

@docs TermFromDom


# Terms from the DOM

@docs create, codec, id

-}

import Codec exposing (Codec)


{-| A glossary term read from the DOM.
-}
type alias TermFromDom =
    { isAbbreviation : Bool
    , body : String
    }


{-| Create a term from its parts.
-}
create : Bool -> String -> TermFromDom
create isAbbreviation_ body_ =
    { isAbbreviation = isAbbreviation_
    , body = body_
    }


{-| Return the fragment identifier for this term.
-}
id : TermFromDom -> String
id termFromDom =
    termFromDom.body |> String.replace " " "_"


{-| Convert a TermFromDom to/from its JSON representation.
-}
codec : Codec TermFromDom
codec =
    Codec.object
        create
        |> Codec.field "isAbbreviation" .isAbbreviation Codec.bool
        |> Codec.field "body" .body Codec.string
        |> Codec.buildObject
