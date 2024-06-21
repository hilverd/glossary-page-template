module Data.GlossaryItem exposing
    ( GlossaryItem
    , init
    , id, preferredTerm, alternativeTerms, definition, needsUpdating, lastUpdatedDateAsIso8601, lastUpdatedByName, lastUpdatedByEmailAddress
    )

{-| An item in a glossary.


# Glossary Items

@docs GlossaryItem


# Build

@docs init


# Query

@docs id, preferredTerm, alternativeTerms, definition, needsUpdating, lastUpdatedDateAsIso8601, lastUpdatedByName, lastUpdatedByEmailAddress

-}

import Data.GlossaryItem.Definition exposing (Definition)
import Data.GlossaryItem.Term exposing (Term)
import Data.GlossaryItemId exposing (GlossaryItemId)


{-| A glossary item.
-}
type GlossaryItem
    = GlossaryItem
        { id : GlossaryItemId
        , preferredTerm : Term
        , alternativeTerms : List Term
        , definition : Maybe Definition
        , needsUpdating : Bool
        , lastUpdatedDateAsIso8601 : Maybe String
        , lastUpdatedByName : Maybe String
        , lastUpdatedByEmailAddress : Maybe String
        }


{-| Create a glossary item from its components.
-}
init :
    GlossaryItemId
    -> Term
    -> List Term
    -> Maybe Definition
    -> Bool
    -> Maybe String
    -> Maybe String
    -> Maybe String
    -> GlossaryItem
init id_ preferredTerm_ alternativeTerms_ definition_ needsUpdating_ lastUpdatedDateAsIso8601_ lastUpdatedByName_ lastUpdatedByEmailAddress_ =
    GlossaryItem
        { id = id_
        , preferredTerm = preferredTerm_
        , alternativeTerms = alternativeTerms_
        , definition = definition_
        , needsUpdating = needsUpdating_
        , lastUpdatedDateAsIso8601 = lastUpdatedDateAsIso8601_
        , lastUpdatedByName = lastUpdatedByName_
        , lastUpdatedByEmailAddress = lastUpdatedByEmailAddress_
        }


{-| The ID for this glossary item.
-}
id : GlossaryItem -> GlossaryItemId
id (GlossaryItem item) =
    item.id


{-| The preferred term for this glossary item.
-}
preferredTerm : GlossaryItem -> Term
preferredTerm (GlossaryItem item) =
    item.preferredTerm


{-| The alternative terms for this glossary item.
-}
alternativeTerms : GlossaryItem -> List Term
alternativeTerms (GlossaryItem item) =
    item.alternativeTerms


{-| The definition for this glossary item.
-}
definition : GlossaryItem -> Maybe Definition
definition (GlossaryItem item) =
    item.definition


{-| The "needs updating" flag for this glossary item.
-}
needsUpdating : GlossaryItem -> Bool
needsUpdating (GlossaryItem item) =
    item.needsUpdating


{-| The last updated date for this glossary item.
-}
lastUpdatedDateAsIso8601 : GlossaryItem -> Maybe String
lastUpdatedDateAsIso8601 (GlossaryItem item) =
    item.lastUpdatedDateAsIso8601


{-| The name of the person who last updated this glossary item.
-}
lastUpdatedByName : GlossaryItem -> Maybe String
lastUpdatedByName (GlossaryItem item) =
    item.lastUpdatedByName


{-| The email address of the person who last updated this glossary item.
-}
lastUpdatedByEmailAddress : GlossaryItem -> Maybe String
lastUpdatedByEmailAddress (GlossaryItem item) =
    item.lastUpdatedByEmailAddress
