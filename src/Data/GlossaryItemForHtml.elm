module Data.GlossaryItemForHtml exposing
    ( GlossaryItemForHtml
    , create, decode
    , disambiguatedPreferredTerm, nonDisambiguatedPreferredTerm, alternativeTerms, allTerms, disambiguationTag, normalTags, allTags, definition, relatedPreferredTerms, needsUpdating, lastUpdatedDateAsIso8601
    , toHtmlTree
    , disambiguatedTerm
    )

{-| An item in a glossary as retrieved from the HTML source, and/or suitable for representing as HTML.
This is also the initial representation obtained when the Elm application is invoked by JavaScript.
It is not the representation used by the editor UI when the application is running -- that is `GlossaryItem`.


# Glossary Items for HTML

@docs GlossaryItemForHtml


# Build

@docs create, decode


# Query

@docs disambiguatedPreferredTerm, nonDisambiguatedPreferredTerm, alternativeTerms, allTerms, disambiguationTag, normalTags, allTags, definition, relatedPreferredTerms, needsUpdating, lastUpdatedDateAsIso8601


# Converting to HTML

@docs toHtmlTree


# Utilities

@docs disambiguatedTerm

-}

import Data.GlossaryItem.Definition as Definition exposing (Definition)
import Data.GlossaryItem.Tag as Tag exposing (Tag)
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItem.TermId as TermId
import Extras.HtmlTree as HtmlTree exposing (HtmlTree)
import Extras.Url exposing (fragmentOnly)
import Internationalisation as I18n
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
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
        , relatedPreferredTerms : List Term
        , needsUpdating : Bool
        , lastUpdatedDateAsIso8601 : Maybe String
        }


{-| Create a glossary item from its parts.
-}
create : Term -> List Term -> Maybe Tag -> List Tag -> Maybe Definition -> List Term -> Bool -> Maybe String -> GlossaryItemForHtml
create preferredTerm_ alternativeTerms_ disambiguationTag_ normalTags_ definition_ relatedPreferredTerms_ needsUpdating_ lastUpdatedDateAsIso8601_ =
    GlossaryItemForHtml
        { preferredTerm = preferredTerm_
        , alternativeTerms = alternativeTerms_
        , disambiguationTag = disambiguationTag_
        , normalTags = normalTags_
        , definition = definition_
        , relatedPreferredTerms = relatedPreferredTerms_
        , needsUpdating = needsUpdating_
        , lastUpdatedDateAsIso8601 = lastUpdatedDateAsIso8601_
        }


{-| Decode a glossary item from its JSON representation.
-}
decode : Decoder GlossaryItemForHtml
decode =
    Decode.succeed create
        |> required "preferredTerm" Term.decode
        |> (required "alternativeTerms" <| Decode.list Term.decode)
        |> (required "disambiguationTag" <| Decode.nullable Tag.decode)
        |> (required "normalTags" <| Decode.list Tag.decode)
        |> (required "definition" <|
                Decode.nullable <|
                    Decode.map Definition.fromMarkdown <|
                        Decode.string
           )
        |> (required "relatedTerms" <| Decode.list Term.decode)
        |> required "needsUpdating" Decode.bool
        |> optional "lastUpdatedDate"
            (Decode.map Just Decode.string)
            Nothing


{-| The disambiguated preferred term for this glossary item.
-}
disambiguatedPreferredTerm : GlossaryItemForHtml -> Term
disambiguatedPreferredTerm glossaryItemForHtml =
    case glossaryItemForHtml of
        GlossaryItemForHtml item ->
            item.disambiguationTag
                |> Maybe.map
                    (\disambiguationTag_ ->
                        disambiguatedTerm disambiguationTag_ item.preferredTerm
                    )
                |> Maybe.withDefault item.preferredTerm


{-| The (non-disambiguated) preferred term for this glossary item.
-}
nonDisambiguatedPreferredTerm : GlossaryItemForHtml -> Term
nonDisambiguatedPreferredTerm glossaryItemForHtml =
    case glossaryItemForHtml of
        GlossaryItemForHtml item ->
            item.preferredTerm


{-| The alternative terms for this glossary item.
-}
alternativeTerms : GlossaryItemForHtml -> List Term
alternativeTerms glossaryItemForHtml =
    case glossaryItemForHtml of
        GlossaryItemForHtml item ->
            item.alternativeTerms


{-| The terms of the glossary item, both (disambiguated) preferred and alternative.
-}
allTerms : GlossaryItemForHtml -> List Term
allTerms glossaryItemForHtml =
    disambiguatedPreferredTerm glossaryItemForHtml :: alternativeTerms glossaryItemForHtml


{-| The disambiguation tag for this glossary item.
-}
disambiguationTag : GlossaryItemForHtml -> Maybe Tag
disambiguationTag glossaryItemForHtml =
    case glossaryItemForHtml of
        GlossaryItemForHtml item ->
            item.disambiguationTag


{-| The normal tags for this glossary item.
-}
normalTags : GlossaryItemForHtml -> List Tag
normalTags glossaryItemForHtml =
    case glossaryItemForHtml of
        GlossaryItemForHtml item ->
            item.normalTags


{-| All tags for this glossary item.
-}
allTags : GlossaryItemForHtml -> List Tag
allTags glossaryItemForHtml =
    case glossaryItemForHtml of
        GlossaryItemForHtml item ->
            item.disambiguationTag
                |> Maybe.map (\disambiguationTag_ -> disambiguationTag_ :: item.normalTags)
                |> Maybe.withDefault item.normalTags


{-| Whether or not the glossary item has a definition.
Some items may not have one and instead point to a related item that is preferred.
-}
hasADefinition : GlossaryItemForHtml -> Bool
hasADefinition glossaryItemForHtml =
    case glossaryItemForHtml of
        GlossaryItemForHtml item ->
            item.definition /= Nothing


{-| The definition for this glossary item.
-}
definition : GlossaryItemForHtml -> Maybe Definition
definition glossaryItemForHtml =
    case glossaryItemForHtml of
        GlossaryItemForHtml item ->
            item.definition


{-| The related preferred terms for this glossary item.
-}
relatedPreferredTerms : GlossaryItemForHtml -> List Term
relatedPreferredTerms glossaryItemForHtml =
    case glossaryItemForHtml of
        GlossaryItemForHtml item ->
            item.relatedPreferredTerms


{-| The "needs updating" flag for this glossary item.
-}
needsUpdating : GlossaryItemForHtml -> Bool
needsUpdating glossaryItemForHtml =
    case glossaryItemForHtml of
        GlossaryItemForHtml item ->
            item.needsUpdating


{-| The last updated date for this glossary item, in ISO 8601 format.
-}
lastUpdatedDateAsIso8601 : GlossaryItemForHtml -> Maybe String
lastUpdatedDateAsIso8601 glossaryItemForHtml =
    case glossaryItemForHtml of
        GlossaryItemForHtml item ->
            item.lastUpdatedDateAsIso8601


{-| Disambiguate a term by appending the given tag in parentheses.

    import Data.GlossaryItem.Tag as Tag exposing (Tag)
    import Data.GlossaryItem.Term as Term exposing (Term)

    tag : Tag
    tag = Tag.fromMarkdown "Finance"

    term : Term
    term = Term.fromMarkdown "Default" False

    disambiguatedTerm tag term
    --> Term.fromMarkdown "Default (Finance)" False

-}
disambiguatedTerm : Tag -> Term -> Term
disambiguatedTerm tag term =
    Term.updateRaw
        (\raw0 -> raw0 ++ " (" ++ Tag.raw tag ++ ")")
        term


preferredTermToHtmlTree : Maybe Tag -> Term -> HtmlTree
preferredTermToHtmlTree disambiguationTag_ term =
    let
        disambiguatedTermIdString =
            disambiguationTag_
                |> Maybe.map
                    (\tag ->
                        disambiguatedTerm tag term
                    )
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


relatedTermToHtmlTree : Term -> HtmlTree
relatedTermToHtmlTree term =
    HtmlTree.Node "a"
        True
        [ hrefFromRelatedTerm term ]
        [ HtmlTree.Leaf <| Term.raw term ]


nonemptyRelatedTermsToHtmlTree : Bool -> List Term -> HtmlTree
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


{-| Represent this glossary item as an HTML tree, ready for writing back to the glossary's HTML file.
-}
toHtmlTree : GlossaryItemForHtml -> HtmlTree
toHtmlTree glossaryItem =
    case glossaryItem of
        GlossaryItemForHtml item ->
            let
                allTags_ =
                    allTags glossaryItem
            in
            HtmlTree.Node "div"
                True
                (item.lastUpdatedDateAsIso8601
                    |> Maybe.map
                        (\lastUpdatedDate_ ->
                            [ HtmlTree.Attribute "data-last-updated" lastUpdatedDate_ ]
                        )
                    |> Maybe.withDefault []
                )
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
