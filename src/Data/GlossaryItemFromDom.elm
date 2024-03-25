module Data.GlossaryItemFromDom exposing
    ( GlossaryItemFromDom
    , create, codec
    )

{-| A glossary item as sent to Elm by JavaScript when read from the DOM.

@docs GlossaryItemFromDom


# Build

@docs create, codec

-}

import Codec exposing (Codec)
import Data.GlossaryItemId as GlossaryItemId exposing (GlossaryItemId)


{-| A glossary item read from the DOM.
-}
type alias GlossaryItemFromDom =
    { id : GlossaryItemId
    , preferredTerm : String
    , alternativeTerms : List String
    , disambiguationTag : Maybe String
    , normalTags : List String
    , definition : Maybe String
    , relatedPreferredTerms : List String
    , needsUpdating : Bool
    , lastUpdatedDateAsIso8601 : Maybe String
    , lastUpdatedByName : Maybe String
    , lastUpdatedByEmailAddress : Maybe String
    }


{-| Create a glossary item from its parts.
-}
create :
    GlossaryItemId
    -> String
    -> List String
    -> Maybe String
    -> List String
    -> Maybe String
    -> List String
    -> Bool
    -> Maybe String
    -> Maybe String
    -> Maybe String
    -> GlossaryItemFromDom
create id_ preferredTerm_ alternativeTerms_ disambiguationTag_ normalTags_ definition_ relatedPreferredTerms_ needsUpdating_ lastUpdatedDateAsIso8601_ lastUpdatedByName_ lastUpdatedByEmailAddress_ =
    { id = id_
    , preferredTerm = preferredTerm_
    , alternativeTerms = alternativeTerms_
    , disambiguationTag = disambiguationTag_
    , normalTags = normalTags_
    , definition = definition_
    , relatedPreferredTerms = relatedPreferredTerms_
    , needsUpdating = needsUpdating_
    , lastUpdatedDateAsIso8601 = lastUpdatedDateAsIso8601_
    , lastUpdatedByName = lastUpdatedByName_
    , lastUpdatedByEmailAddress = lastUpdatedByEmailAddress_
    }


{-| Convert a GlossaryItemFromDom to/from its JSON representation.
-}
codec : Codec GlossaryItemFromDom
codec =
    Codec.object
        create
        |> Codec.field "id" .id GlossaryItemId.codec
        |> Codec.field "preferredTerm" .preferredTerm Codec.string
        |> Codec.field "alternativeTerms" .alternativeTerms (Codec.list Codec.string)
        |> Codec.field "disambiguationTag" .disambiguationTag (Codec.nullable Codec.string)
        |> Codec.field "normalTags" .normalTags (Codec.list Codec.string)
        |> Codec.field "definition" .definition (Codec.nullable Codec.string)
        |> Codec.field "relatedTerms" .relatedPreferredTerms (Codec.list Codec.string)
        |> Codec.field "needsUpdating" .needsUpdating Codec.bool
        |> Codec.field "lastUpdatedDate" .lastUpdatedDateAsIso8601 (Codec.maybe Codec.string)
        |> Codec.field "lastUpdatedByName" .lastUpdatedByName (Codec.maybe Codec.string)
        |> Codec.field "lastUpdatedByEmailAddress" .lastUpdatedByEmailAddress (Codec.maybe Codec.string)
        |> Codec.buildObject
