module Data.GlossaryItemForHtml exposing
    ( GlossaryItemForHtml
    , decode
    , preferredTerm, disambiguatedPreferredTerm, alternativeTerms, tags, definition, relatedPreferredTerms, needsUpdating, lastUpdatedDateAsIso8601
    , toHtmlTree
    )

{-| An item in a glossary as retrieved from the HTML source, and/or suitable for representing as HTML.
This is also the initial representation obtained when the Elm application is invoked by JavaScript.
It is not the representation used by the editor UI when the application is running -- that is `GlossaryItem`.


# Glossary Items for HTML

@docs GlossaryItemForHtml


# Build

@docs decode


# Query

@docs preferredTerm, disambiguatedPreferredTerm, alternativeTerms, tags, definition, relatedPreferredTerms, needsUpdating, lastUpdatedDateAsIso8601


# Converting to HTML

@docs toHtmlTree

-}

import Data.GlossaryItem exposing (preferredTerm)
import Data.GlossaryItem.Definition as Definition exposing (Definition)
import Data.GlossaryItem.RelatedTerm as RelatedTerm exposing (RelatedTerm)
import Data.GlossaryItem.Tag as Tag exposing (Tag)
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItem.TermId as TermId
import Data.GlossaryItemForHtml.TagInItem as TagInItem exposing (TagInItem)
import Extras.HtmlTree as HtmlTree exposing (HtmlTree)
import Extras.Url exposing (fragmentOnly)
import Json.Decode as Decode exposing (Decoder)


{-| A glossary item from/for HTML.
-}
type GlossaryItemForHtml
    = GlossaryItemForHtml
        { preferredTerm : Term
        , alternativeTerms : List Term
        , tags : List TagInItem
        , definition : Maybe Definition
        , relatedPreferredTerms : List RelatedTerm
        , needsUpdating : Bool
        , lastUpdatedDateAsIso8601 : Maybe String
        }


{-| Decode a glossary item from its JSON representation.
-}
decode : Bool -> Decoder GlossaryItemForHtml
decode enableMarkdownBasedSyntax =
    Decode.map7
        (\preferredTerm_ alternativeTerms_ tags_ definition_ relatedPreferredTerms_ needsUpdating_ lastUpdatedDateAsIso8601_ ->
            GlossaryItemForHtml
                { preferredTerm = preferredTerm_
                , alternativeTerms = alternativeTerms_
                , tags = tags_
                , definition = definition_
                , relatedPreferredTerms = relatedPreferredTerms_
                , needsUpdating = needsUpdating_
                , lastUpdatedDateAsIso8601 = lastUpdatedDateAsIso8601_
                }
        )
        (Decode.field "preferredTerm" <| Term.decode enableMarkdownBasedSyntax)
        (Decode.field "alternativeTerms" <| Decode.list <| Term.decode enableMarkdownBasedSyntax)
        (Decode.field "tags" <| Decode.list <| TagInItem.decode enableMarkdownBasedSyntax)
        (Decode.field "definition" <|
            Decode.nullable <|
                Decode.map
                    (if enableMarkdownBasedSyntax then
                        Definition.fromMarkdown

                     else
                        Definition.fromPlaintext
                    )
                <|
                    Decode.string
        )
        (Decode.field "relatedTerms" <| Decode.list <| RelatedTerm.decode enableMarkdownBasedSyntax)
        (Decode.field "needsUpdating" Decode.bool)
        (Decode.maybe <| Decode.field "lastUpdatedDate" Decode.string)


{-| The preferred term for this glossary item.
-}
preferredTerm : GlossaryItemForHtml -> Term
preferredTerm glossaryItemForHtml =
    case glossaryItemForHtml of
        GlossaryItemForHtml item ->
            item.preferredTerm


disambiguationTagFromItemTags : List TagInItem -> Maybe Tag
disambiguationTagFromItemTags =
    List.filterMap
        (\tagInItem ->
            case tagInItem of
                TagInItem.DisambiguationTag tag ->
                    Just tag

                TagInItem.NormalTag _ ->
                    Nothing
        )
        >> List.head


{-| The disambiguated preferred term for this glossary item.
-}
disambiguatedPreferredTerm : GlossaryItemForHtml -> Term
disambiguatedPreferredTerm glossaryItemForHtml =
    case glossaryItemForHtml of
        GlossaryItemForHtml item ->
            item.tags
                |> disambiguationTagFromItemTags
                |> Maybe.map
                    (\disambiguationTag_ ->
                        item.preferredTerm
                            |> Term.updateRaw
                                (\raw0 -> raw0 ++ " (" ++ Tag.raw disambiguationTag_ ++ ")")
                    )
                |> Maybe.withDefault item.preferredTerm


{-| The alternative terms for this glossary item.
-}
alternativeTerms : GlossaryItemForHtml -> List Term
alternativeTerms glossaryItemForHtml =
    case glossaryItemForHtml of
        GlossaryItemForHtml item ->
            item.alternativeTerms


{-| The terms of the glossary item, both preferred and alternative.
-}
allTerms : GlossaryItemForHtml -> List Term
allTerms glossaryItemForHtml =
    preferredTerm glossaryItemForHtml :: alternativeTerms glossaryItemForHtml


{-| The tags for this glossary item.
-}
tags : GlossaryItemForHtml -> List TagInItem
tags glossaryItemForHtml =
    case glossaryItemForHtml of
        GlossaryItemForHtml item ->
            item.tags


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
relatedPreferredTerms : GlossaryItemForHtml -> List RelatedTerm
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


termToHtmlTree : Term -> HtmlTree
termToHtmlTree term =
    HtmlTree.Node "dt"
        True
        []
        [ HtmlTree.Node "dfn"
            True
            [ HtmlTree.Attribute "id" <| TermId.toString <| Term.id term ]
            [ HtmlTree.Leaf (Term.raw term)
                |> (\inner ->
                        let
                            linkedTerm : HtmlTree
                            linkedTerm =
                                HtmlTree.Node "a"
                                    True
                                    [ hrefToTerm term ]
                                    [ inner ]
                        in
                        if Term.isAbbreviation term then
                            HtmlTree.Node "abbr" True [] [ linkedTerm ]

                        else
                            linkedTerm
                   )
            ]
        ]


definitionToHtmlTree : String -> HtmlTree
definitionToHtmlTree definition_ =
    HtmlTree.Node "dd"
        False
        []
        [ HtmlTree.Leaf definition_ ]


relatedTermToHtmlTree : RelatedTerm -> HtmlTree
relatedTermToHtmlTree relatedTerm =
    HtmlTree.Node "a"
        True
        [ hrefFromRelatedTerm relatedTerm ]
        [ HtmlTree.Leaf <| RelatedTerm.raw relatedTerm ]


nonemptyRelatedTermsToHtmlTree : Bool -> List RelatedTerm -> HtmlTree
nonemptyRelatedTermsToHtmlTree itemHasSomeADefinition relatedTerms_ =
    HtmlTree.Node "dd"
        False
        [ HtmlTree.Attribute "class" "related-terms" ]
        (HtmlTree.Leaf
            (if itemHasSomeADefinition then
                "See also: "

             else
                "See: "
            )
            :: (relatedTerms_
                    |> List.map relatedTermToHtmlTree
                    |> List.intersperse (HtmlTree.Leaf ", ")
               )
        )


hrefToTerm : Term -> HtmlTree.Attribute
hrefToTerm term =
    HtmlTree.Attribute "href" <| fragmentOnly <| TermId.toString <| Term.id term


hrefFromRelatedTerm : RelatedTerm -> HtmlTree.Attribute
hrefFromRelatedTerm relatedTerm =
    HtmlTree.Attribute "href" <| fragmentOnly <| TermId.toString <| RelatedTerm.idReference relatedTerm


{-| Represent this glossary item as an HTML tree, ready for writing back to the glossary's HTML file.
-}
toHtmlTree : GlossaryItemForHtml -> HtmlTree
toHtmlTree glossaryItem =
    case glossaryItem of
        GlossaryItemForHtml item ->
            HtmlTree.Node "div"
                True
                (item.lastUpdatedDateAsIso8601
                    |> Maybe.map
                        (\lastUpdatedDate_ ->
                            [ HtmlTree.Attribute "data-last-updated" lastUpdatedDate_ ]
                        )
                    |> Maybe.withDefault []
                )
                -- TODO: treat preferred terms differently -- write separate span for disambiguation tag if there is one
                ((preferredTerm glossaryItem
                    |> termToHtmlTree
                 )
                    :: List.map termToHtmlTree (allTerms glossaryItem)
                    ++ (if item.needsUpdating then
                            [ HtmlTree.Node "dd"
                                False
                                [ HtmlTree.Attribute "class" "needs-updating" ]
                                [ HtmlTree.Node "span"
                                    False
                                    []
                                    [ HtmlTree.Leaf "[Needs updating]" ]
                                ]
                            ]

                        else
                            []
                       )
                    ++ (if List.isEmpty item.tags then
                            []

                        else
                            [ HtmlTree.Node "dd"
                                True
                                [ HtmlTree.Attribute "class" "tags" ]
                                (List.map
                                    (\tagInItem ->
                                        HtmlTree.Node "button"
                                            False
                                            [ HtmlTree.Attribute "type" "button" ]
                                            [ HtmlTree.Leaf <| Tag.raw <| TagInItem.tag tagInItem ]
                                    )
                                    item.tags
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
