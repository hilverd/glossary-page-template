module Data.GlossaryItemForUi exposing
    ( GlossaryItemForUi
    , create, codec, fromGlossaryItemFromDom
    , disambiguatedPreferredTerm, id, nonDisambiguatedPreferredTerm, alternativeTerms, allTerms, disambiguationTag, normalTags, allTags, definition, relatedPreferredTerms, needsUpdating, lastUpdatedDateAsIso8601, lastUpdatedByName, lastUpdatedByEmailAddress
    , toGlossaryItemFromDom
    , disambiguatedTerm
    )

{-| An item in a glossary ready to be used in a view function.


# Glossary Items for the UI

@docs GlossaryItemForUi


# Build

@docs create, codec, fromGlossaryItemFromDom


# Query

@docs disambiguatedPreferredTerm, id, nonDisambiguatedPreferredTerm, alternativeTerms, allTerms, disambiguationTag, normalTags, allTags, definition, relatedPreferredTerms, needsUpdating, lastUpdatedDateAsIso8601, lastUpdatedByName, lastUpdatedByEmailAddress


# Exporting

@docs toGlossaryItemFromDom


# Utilities

@docs disambiguatedTerm

-}

import Codec exposing (Codec)
import Data.GlossaryItem.Definition as Definition exposing (Definition)
import Data.GlossaryItem.DisambiguatedTerm as DisambiguatedTerm exposing (DisambiguatedTerm)
import Data.GlossaryItem.Tag as Tag exposing (Tag)
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItemFromDom exposing (GlossaryItemFromDom)
import Data.GlossaryItemId as GlossaryItemId exposing (GlossaryItemId)
import List


{-| A glossary item for the UI.
-}
type GlossaryItemForUi
    = GlossaryItemForUi
        { id : GlossaryItemId
        , preferredTerm : Term
        , alternativeTerms : List Term
        , disambiguationTag : Maybe Tag
        , normalTags : List Tag
        , definition : Maybe Definition
        , relatedPreferredTerms : List DisambiguatedTerm
        , needsUpdating : Bool
        , lastUpdatedDateAsIso8601 : Maybe String
        , lastUpdatedByName : Maybe String
        , lastUpdatedByEmailAddress : Maybe String
        }


{-| Build a GlossaryItemForUi from a GlossaryItemFromDom.
-}
fromGlossaryItemFromDom : GlossaryItemFromDom -> GlossaryItemForUi
fromGlossaryItemFromDom glossaryItemFromDom =
    create
        (GlossaryItemId.create glossaryItemFromDom.id)
        (Term.fromMarkdown
            glossaryItemFromDom.preferredTerm.body
            glossaryItemFromDom.preferredTerm.isAbbreviation
        )
        (List.map
            (\termFromDom ->
                Term.fromMarkdown
                    termFromDom.body
                    termFromDom.isAbbreviation
            )
            glossaryItemFromDom.alternativeTerms
        )
        (Maybe.map Tag.fromMarkdown glossaryItemFromDom.disambiguationTag)
        (List.map Tag.fromMarkdown glossaryItemFromDom.normalTags)
        (Maybe.map Definition.fromMarkdown glossaryItemFromDom.definition)
        (List.map
            (\termFromDom ->
                Term.fromMarkdown
                    termFromDom.body
                    termFromDom.isAbbreviation
                    |> DisambiguatedTerm.fromTerm
            )
            glossaryItemFromDom.relatedPreferredTerms
        )
        glossaryItemFromDom.needsUpdating
        glossaryItemFromDom.lastUpdatedDateAsIso8601
        glossaryItemFromDom.lastUpdatedByName
        glossaryItemFromDom.lastUpdatedByEmailAddress


{-| Create a glossary item from its parts.
-}
create :
    GlossaryItemId
    -> Term
    -> List Term
    -> Maybe Tag
    -> List Tag
    -> Maybe Definition
    -> List DisambiguatedTerm
    -> Bool
    -> Maybe String
    -> Maybe String
    -> Maybe String
    -> GlossaryItemForUi
create id_ preferredTerm_ alternativeTerms_ disambiguationTag_ normalTags_ definition_ relatedPreferredTerms_ needsUpdating_ lastUpdatedDateAsIso8601_ lastUpdatedByName_ lastUpdatedByEmailAddress_ =
    GlossaryItemForUi
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


{-| Encode/decode a glossary item to/from its JSON representation.
-}
codec : Codec GlossaryItemForUi
codec =
    Codec.object
        create
        |> Codec.field "id" id GlossaryItemId.codec
        |> Codec.field "preferredTerm" nonDisambiguatedPreferredTerm Term.codec
        |> Codec.field "alternativeTerms" alternativeTerms (Codec.list Term.codec)
        |> Codec.field "disambiguationTag" disambiguationTag (Codec.nullable Tag.codec)
        |> Codec.field "normalTags" normalTags (Codec.list Tag.codec)
        |> Codec.field "definition"
            definition
            (Codec.nullable <|
                Codec.map Definition.fromMarkdown Definition.raw Codec.string
            )
        |> Codec.field "relatedTerms" relatedPreferredTerms (Codec.list DisambiguatedTerm.codec)
        |> Codec.field "needsUpdating" needsUpdating Codec.bool
        |> Codec.field "lastUpdatedDate" lastUpdatedDateAsIso8601 (Codec.maybe Codec.string)
        |> Codec.field "lastUpdatedByName" lastUpdatedByName (Codec.maybe Codec.string)
        |> Codec.field "lastUpdatedByEmailAddress" lastUpdatedByEmailAddress (Codec.maybe Codec.string)
        |> Codec.buildObject


{-| The disambiguated preferred term for this glossary item.
-}
disambiguatedPreferredTerm : GlossaryItemForUi -> DisambiguatedTerm
disambiguatedPreferredTerm (GlossaryItemForUi item) =
    item.disambiguationTag
        |> Maybe.map
            (\disambiguationTag_ ->
                disambiguatedTerm disambiguationTag_ item.preferredTerm
            )
        |> Maybe.withDefault (DisambiguatedTerm.fromTerm item.preferredTerm)


{-| The ID for this glossary item.
-}
id : GlossaryItemForUi -> GlossaryItemId
id (GlossaryItemForUi item) =
    item.id


{-| The (non-disambiguated) preferred term for this glossary item.
-}
nonDisambiguatedPreferredTerm : GlossaryItemForUi -> Term
nonDisambiguatedPreferredTerm (GlossaryItemForUi item) =
    item.preferredTerm


{-| The alternative terms for this glossary item.
-}
alternativeTerms : GlossaryItemForUi -> List Term
alternativeTerms (GlossaryItemForUi item) =
    item.alternativeTerms


{-| The terms of the glossary item, both (disambiguated) preferred and alternative.
-}
allTerms : GlossaryItemForUi -> List Term
allTerms glossaryItemForUi =
    (glossaryItemForUi
        |> disambiguatedPreferredTerm
        |> DisambiguatedTerm.toTerm
    )
        :: alternativeTerms glossaryItemForUi


{-| The disambiguation tag for this glossary item.
-}
disambiguationTag : GlossaryItemForUi -> Maybe Tag
disambiguationTag (GlossaryItemForUi item) =
    item.disambiguationTag


{-| The normal tags for this glossary item.
-}
normalTags : GlossaryItemForUi -> List Tag
normalTags (GlossaryItemForUi item) =
    item.normalTags


{-| All tags for this glossary item.
-}
allTags : GlossaryItemForUi -> List Tag
allTags (GlossaryItemForUi item) =
    item.disambiguationTag
        |> Maybe.map (\disambiguationTag_ -> disambiguationTag_ :: item.normalTags)
        |> Maybe.withDefault item.normalTags


{-| The definition for this glossary item.
-}
definition : GlossaryItemForUi -> Maybe Definition
definition (GlossaryItemForUi item) =
    item.definition


{-| The related preferred terms for this glossary item.
-}
relatedPreferredTerms : GlossaryItemForUi -> List DisambiguatedTerm
relatedPreferredTerms (GlossaryItemForUi item) =
    item.relatedPreferredTerms


{-| The "needs updating" flag for this glossary item.
-}
needsUpdating : GlossaryItemForUi -> Bool
needsUpdating (GlossaryItemForUi item) =
    item.needsUpdating


{-| The last updated date for this glossary item, in ISO 8601 format.
-}
lastUpdatedDateAsIso8601 : GlossaryItemForUi -> Maybe String
lastUpdatedDateAsIso8601 (GlossaryItemForUi item) =
    item.lastUpdatedDateAsIso8601


{-| The name of the person who last updated this glossary item.
-}
lastUpdatedByName : GlossaryItemForUi -> Maybe String
lastUpdatedByName (GlossaryItemForUi item) =
    item.lastUpdatedByName


{-| The email address of the person who last updated this glossary item.
-}
lastUpdatedByEmailAddress : GlossaryItemForUi -> Maybe String
lastUpdatedByEmailAddress (GlossaryItemForUi item) =
    item.lastUpdatedByEmailAddress


{-| Disambiguate a term by appending the given tag in parentheses.

    import Data.GlossaryItem.Tag as Tag exposing (Tag)
    import Data.GlossaryItem.Term as Term exposing (Term)
    import Data.GlossaryItem.DisambiguatedTerm as DisambiguatedTerm exposing (DisambiguatedTerm)

    tag : Tag
    tag = Tag.fromMarkdown "Finance"

    term : Term
    term = Term.fromMarkdown "Default" False

    disambiguatedTerm tag term
    --> DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Default (Finance)" False

-}
disambiguatedTerm : Tag -> Term -> DisambiguatedTerm
disambiguatedTerm tag term =
    term
        |> Term.updateRaw
            (\raw0 -> raw0 ++ " (" ++ Tag.raw tag ++ ")")
        |> DisambiguatedTerm.fromTerm


{-| Convert this glossary item to a GlossaryItemFromDom.
-}
toGlossaryItemFromDom : GlossaryItemForUi -> GlossaryItemFromDom
toGlossaryItemFromDom (GlossaryItemForUi item) =
    { id = GlossaryItemId.toString item.id
    , preferredTerm = Term.toTermFromDom item.preferredTerm
    , alternativeTerms = List.map Term.toTermFromDom item.alternativeTerms
    , disambiguationTag = Maybe.map Tag.raw item.disambiguationTag
    , normalTags = List.map Tag.raw item.normalTags
    , definition = Maybe.map Definition.raw item.definition
    , relatedPreferredTerms =
        List.map
            (DisambiguatedTerm.toTerm >> Term.toTermFromDom)
            item.relatedPreferredTerms
    , needsUpdating = item.needsUpdating
    , lastUpdatedDateAsIso8601 = item.lastUpdatedDateAsIso8601
    , lastUpdatedByName = item.lastUpdatedByName
    , lastUpdatedByEmailAddress = item.lastUpdatedByEmailAddress
    }
