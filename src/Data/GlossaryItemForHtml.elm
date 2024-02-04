module Data.GlossaryItemForHtml exposing
    ( GlossaryItemForHtml
    , create, codec, setLastUpdatedBy
    , disambiguatedPreferredTerm, nonDisambiguatedPreferredTerm, alternativeTerms, allTerms, disambiguationTag, normalTags, allTags, definition, relatedPreferredTerms, needsUpdating, lastUpdatedDateAsIso8601, lastUpdatedByName, lastUpdatedByEmailAddress
    , toHtmlTree
    , disambiguatedTerm
    )

{-| An item in a glossary as retrieved from the HTML source, and/or suitable for representing as HTML.
This is also the initial representation obtained when the Elm application is invoked by JavaScript.
It is not the representation used by the editor UI when the application is running -- that is `GlossaryItem`.


# Glossary Items for HTML

@docs GlossaryItemForHtml


# Build

@docs create, codec, setLastUpdatedBy


# Query

@docs disambiguatedPreferredTerm, nonDisambiguatedPreferredTerm, alternativeTerms, allTerms, disambiguationTag, normalTags, allTags, definition, relatedPreferredTerms, needsUpdating, lastUpdatedDateAsIso8601, lastUpdatedByName, lastUpdatedByEmailAddress


# Converting to HTML

@docs toHtmlTree


# Utilities

@docs disambiguatedTerm

-}

import Codec exposing (Codec)
import Data.GlossaryItem.Definition as Definition exposing (Definition)
import Data.GlossaryItem.DisambiguatedTerm as DisambiguatedTerm exposing (DisambiguatedTerm)
import Data.GlossaryItem.Tag as Tag exposing (Tag)
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItem.TermId as TermId
import Extras.HtmlTree as HtmlTree exposing (HtmlTree)
import Extras.Url exposing (fragmentOnly)
import Internationalisation as I18n
import List


{-| A glossary item from/for HTML.
-}
type GlossaryItemForHtml
    = GlossaryItemForHtml
        { preferredTerm : Term
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


{-| Create a glossary item from its parts.
-}
create :
    Term
    -> List Term
    -> Maybe Tag
    -> List Tag
    -> Maybe Definition
    -> List DisambiguatedTerm
    -> Bool
    -> Maybe String
    -> Maybe String
    -> Maybe String
    -> GlossaryItemForHtml
create preferredTerm_ alternativeTerms_ disambiguationTag_ normalTags_ definition_ relatedPreferredTerms_ needsUpdating_ lastUpdatedDateAsIso8601_ lastUpdatedByName_ lastUpdatedByEmailAddress_ =
    GlossaryItemForHtml
        { preferredTerm = preferredTerm_
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
codec : Codec GlossaryItemForHtml
codec =
    Codec.object
        create
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
disambiguatedPreferredTerm : GlossaryItemForHtml -> DisambiguatedTerm
disambiguatedPreferredTerm (GlossaryItemForHtml item) =
    item.disambiguationTag
        |> Maybe.map
            (\disambiguationTag_ ->
                disambiguatedTerm disambiguationTag_ item.preferredTerm
            )
        |> Maybe.withDefault (DisambiguatedTerm.fromTerm item.preferredTerm)


{-| The (non-disambiguated) preferred term for this glossary item.
-}
nonDisambiguatedPreferredTerm : GlossaryItemForHtml -> Term
nonDisambiguatedPreferredTerm (GlossaryItemForHtml item) =
    item.preferredTerm


{-| The alternative terms for this glossary item.
-}
alternativeTerms : GlossaryItemForHtml -> List Term
alternativeTerms (GlossaryItemForHtml item) =
    item.alternativeTerms


{-| The terms of the glossary item, both (disambiguated) preferred and alternative.
-}
allTerms : GlossaryItemForHtml -> List Term
allTerms glossaryItemForHtml =
    (glossaryItemForHtml
        |> disambiguatedPreferredTerm
        |> DisambiguatedTerm.toTerm
    )
        :: alternativeTerms glossaryItemForHtml


{-| The disambiguation tag for this glossary item.
-}
disambiguationTag : GlossaryItemForHtml -> Maybe Tag
disambiguationTag (GlossaryItemForHtml item) =
    item.disambiguationTag


{-| The normal tags for this glossary item.
-}
normalTags : GlossaryItemForHtml -> List Tag
normalTags (GlossaryItemForHtml item) =
    item.normalTags


{-| All tags for this glossary item.
-}
allTags : GlossaryItemForHtml -> List Tag
allTags (GlossaryItemForHtml item) =
    item.disambiguationTag
        |> Maybe.map (\disambiguationTag_ -> disambiguationTag_ :: item.normalTags)
        |> Maybe.withDefault item.normalTags


{-| Whether or not the glossary item has a definition.
Some items may not have one and instead point to a related item that is preferred.
-}
hasADefinition : GlossaryItemForHtml -> Bool
hasADefinition (GlossaryItemForHtml item) =
    item.definition /= Nothing


{-| The definition for this glossary item.
-}
definition : GlossaryItemForHtml -> Maybe Definition
definition (GlossaryItemForHtml item) =
    item.definition


{-| The related preferred terms for this glossary item.
-}
relatedPreferredTerms : GlossaryItemForHtml -> List DisambiguatedTerm
relatedPreferredTerms (GlossaryItemForHtml item) =
    item.relatedPreferredTerms


{-| The "needs updating" flag for this glossary item.
-}
needsUpdating : GlossaryItemForHtml -> Bool
needsUpdating (GlossaryItemForHtml item) =
    item.needsUpdating


{-| The last updated date for this glossary item, in ISO 8601 format.
-}
lastUpdatedDateAsIso8601 : GlossaryItemForHtml -> Maybe String
lastUpdatedDateAsIso8601 (GlossaryItemForHtml item) =
    item.lastUpdatedDateAsIso8601


{-| The name of the person who last updated this glossary item.
-}
lastUpdatedByName : GlossaryItemForHtml -> Maybe String
lastUpdatedByName (GlossaryItemForHtml item) =
    item.lastUpdatedByName


{-| The email address of the person who last updated this glossary item.
-}
lastUpdatedByEmailAddress : GlossaryItemForHtml -> Maybe String
lastUpdatedByEmailAddress (GlossaryItemForHtml item) =
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


preferredTermToHtmlTree : Maybe Tag -> Term -> HtmlTree
preferredTermToHtmlTree disambiguationTag_ term =
    let
        disambiguatedTermIdString : String
        disambiguatedTermIdString =
            disambiguationTag_
                |> Maybe.map
                    (\tag ->
                        disambiguatedTerm tag term
                    )
                |> Maybe.map DisambiguatedTerm.toTerm
                |> Maybe.withDefault term
                |> Term.id
                |> TermId.toString
    in
    HtmlTree.Node "dt"
        True
        []
        [ HtmlTree.Node "dfn"
            True
            [ HtmlTree.Attribute "id" disambiguatedTermIdString ]
            [ (disambiguationTag_
                |> Maybe.map
                    (\disambiguationTag0 ->
                        [ HtmlTree.Node "span"
                            False
                            []
                            [ HtmlTree.Leaf <| Term.raw term ]
                        , HtmlTree.Node "span"
                            False
                            [ HtmlTree.Attribute "class" "disambiguation" ]
                            [ HtmlTree.Leaf <| "(" ++ Tag.raw disambiguationTag0 ++ ")" ]
                        ]
                    )
                |> Maybe.withDefault
                    [ HtmlTree.Leaf (Term.raw term) ]
              )
                |> (\inner ->
                        let
                            linkedTerm : HtmlTree
                            linkedTerm =
                                HtmlTree.Node "a"
                                    True
                                    [ HtmlTree.Attribute "href" <| fragmentOnly disambiguatedTermIdString ]
                                    inner
                        in
                        if Term.isAbbreviation term then
                            HtmlTree.Node "abbr" True [] [ linkedTerm ]

                        else
                            linkedTerm
                   )
            ]
        ]


alternativeTermToHtmlTree : Term -> HtmlTree
alternativeTermToHtmlTree term =
    HtmlTree.Node "dt"
        True
        []
        [ HtmlTree.Node "dfn"
            True
            []
            [ let
                termHtmlTree : HtmlTree
                termHtmlTree =
                    HtmlTree.Leaf (Term.raw term)
              in
              if Term.isAbbreviation term then
                HtmlTree.Node "abbr" True [] [ termHtmlTree ]

              else
                termHtmlTree
            ]
        ]


definitionToHtmlTree : String -> HtmlTree
definitionToHtmlTree definition_ =
    HtmlTree.Node "dd"
        False
        []
        [ HtmlTree.Leaf definition_ ]


relatedTermToHtmlTree : DisambiguatedTerm -> HtmlTree
relatedTermToHtmlTree disambiguatedTerm_ =
    let
        term =
            DisambiguatedTerm.toTerm disambiguatedTerm_
    in
    HtmlTree.Node "a"
        True
        [ hrefFromRelatedTerm term ]
        [ HtmlTree.Leaf <| Term.raw term ]


nonemptyRelatedTermsToHtmlTree : Bool -> List DisambiguatedTerm -> HtmlTree
nonemptyRelatedTermsToHtmlTree itemHasADefinition relatedTerms_ =
    HtmlTree.Node "dd"
        False
        [ HtmlTree.Attribute "class" "related-terms" ]
        (HtmlTree.Leaf
            (if itemHasADefinition then
                I18n.seeAlso ++ ": "

             else
                I18n.see ++ ": "
            )
            :: (relatedTerms_
                    |> List.map relatedTermToHtmlTree
                    |> List.intersperse (HtmlTree.Leaf ", ")
               )
        )


hrefFromRelatedTerm : Term -> HtmlTree.Attribute
hrefFromRelatedTerm term =
    HtmlTree.Attribute "href" <| fragmentOnly <| TermId.toString <| Term.id term


{-| Set the name and email address of the person who last updated this glossary item.
-}
setLastUpdatedBy : { name : String, emailAddress : String } -> GlossaryItemForHtml -> GlossaryItemForHtml
setLastUpdatedBy { name, emailAddress } (GlossaryItemForHtml item) =
    GlossaryItemForHtml
        { item
            | lastUpdatedByName = Just name
            , lastUpdatedByEmailAddress = Just emailAddress
        }


{-| Represent this glossary item as an HTML tree, ready for writing back to the glossary's HTML file.
-}
toHtmlTree : GlossaryItemForHtml -> HtmlTree
toHtmlTree ((GlossaryItemForHtml item) as glossaryItem) =
    let
        allTags_ =
            allTags glossaryItem
    in
    HtmlTree.Node "div"
        True
        [ HtmlTree.showAttributeMaybe "data-last-updated" identity item.lastUpdatedDateAsIso8601
        , HtmlTree.showAttributeMaybe "data-last-updated-by-name" identity item.lastUpdatedByName
        , HtmlTree.showAttributeMaybe "data-last-updated-by-email-address" identity item.lastUpdatedByEmailAddress
        ]
        (preferredTermToHtmlTree item.disambiguationTag item.preferredTerm
            :: List.map alternativeTermToHtmlTree item.alternativeTerms
            ++ (if item.needsUpdating then
                    [ HtmlTree.Node "dd"
                        False
                        [ HtmlTree.Attribute "class" "needs-updating" ]
                        [ HtmlTree.Node "span"
                            False
                            []
                            [ HtmlTree.Leaf <| "[" ++ I18n.needsUpdating ++ "]" ]
                        ]
                    ]

                else
                    []
               )
            ++ (if List.isEmpty allTags_ then
                    []

                else
                    [ HtmlTree.Node "dd"
                        True
                        [ HtmlTree.Attribute "class" "tags" ]
                        (List.map
                            (\tag ->
                                HtmlTree.Node "button"
                                    False
                                    [ HtmlTree.Attribute "type" "button" ]
                                    [ HtmlTree.Leaf <| Tag.raw tag ]
                            )
                            allTags_
                        )
                    ]
               )
            ++ List.map
                (Definition.raw >> definitionToHtmlTree)
                (item.definition |> Maybe.map List.singleton |> Maybe.withDefault [])
            ++ (if List.isEmpty item.relatedPreferredTerms then
                    []

                else
                    [ nonemptyRelatedTermsToHtmlTree
                        (hasADefinition glossaryItem)
                        item.relatedPreferredTerms
                    ]
               )
        )
