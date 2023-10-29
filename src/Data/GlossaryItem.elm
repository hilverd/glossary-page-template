module Data.GlossaryItem exposing
    ( GlossaryItem
    , init
    , preferredTerm, alternativeTerms, definition, needsUpdating, lastUpdatedDateAsIso8601
    )

{-| An item in a glossary.


# Glossary Items

@docs GlossaryItem


# Build

@docs init


# Query

@docs preferredTerm, alternativeTerms, definition, needsUpdating, lastUpdatedDateAsIso8601

-}

import Data.GlossaryItem.Definition exposing (Definition)
import Data.GlossaryItem.Term exposing (Term)


{-| A glossary item.
-}
type GlossaryItem
    = GlossaryItem
        { preferredTerm : Term
        , alternativeTerms : List Term
        , definition : Maybe Definition
        , needsUpdating : Bool
        , lastUpdatedDateAsIso8601 : Maybe String
        }


{-| Create a glossary item from its components.
-}
init : Term -> List Term -> Maybe Definition -> Bool -> Maybe String -> GlossaryItem
init preferredTerm_ alternativeTerms_ definition_ needsUpdating_ lastUpdatedDateAsIso8601_ =
    GlossaryItem
        { preferredTerm = preferredTerm_
        , alternativeTerms = alternativeTerms_
        , definition = definition_
        , needsUpdating = needsUpdating_
        , lastUpdatedDateAsIso8601 = lastUpdatedDateAsIso8601_
        }


{-| The preferred term for this glossary item.
-}
preferredTerm : GlossaryItem -> Term
preferredTerm glossaryItem =
    case glossaryItem of
        GlossaryItem item ->
            item.preferredTerm


{-| The alternative terms for this glossary item.
-}
alternativeTerms : GlossaryItem -> List Term
alternativeTerms glossaryItem =
    case glossaryItem of
        GlossaryItem item ->
            item.alternativeTerms


{-| The definition for this glossary item.
-}
definition : GlossaryItem -> Maybe Definition
definition glossaryItem =
    case glossaryItem of
        GlossaryItem item ->
            item.definition


{-| The "needs updating" flag for this glossary item.
-}
needsUpdating : GlossaryItem -> Bool
needsUpdating glossaryItem =
    case glossaryItem of
        GlossaryItem item ->
            item.needsUpdating


{-| The last updated date for this glossary item.
-}
lastUpdatedDateAsIso8601 : GlossaryItem -> Maybe String
lastUpdatedDateAsIso8601 glossaryItem =
    case glossaryItem of
        GlossaryItem item ->
            item.lastUpdatedDateAsIso8601
