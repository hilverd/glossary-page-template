module Data.GlossaryItem exposing
    ( GlossaryItem, init, decode, preferredTerm, nonDisambiguatedPreferredTerm, alternativeTerms, allTerms, tags, disambiguationTag, hasADefinition, definition, relatedPreferredTerms, needsUpdating, lastUpdatedDate, updateRelatedTerms, updateTags
    , toHtmlTree
    )

{-| An item in a glossary.


# Glossary Items

@docs GlossaryItem, init, decode, preferredTerm, nonDisambiguatedPreferredTerm, alternativeTerms, allTerms, tags, disambiguationTag, hasADefinition, definition, relatedPreferredTerms, needsUpdating, lastUpdatedDate, updateRelatedTerms, updateTags


# Converting to HTML

@docs toHtmlTree

-}

import Data.GlossaryItem.Definition as Definition exposing (Definition)
import Data.GlossaryItem.RelatedTerm as RelatedTerm exposing (RelatedTerm)
import Data.GlossaryItem.Tag as Tag exposing (Tag)
import Data.GlossaryItem.TagInItem as TagInItem exposing (TagInItem(..))
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItem.TermId as TermId
import Extras.HtmlTree as HtmlTree exposing (HtmlTree)
import Extras.Url exposing (fragmentOnly)
import Json.Decode as Decode exposing (Decoder)


{-| An item in a glossary, consisting mainly of a list of terms (synonyms) being defined and a definition for those terms.
-}
type GlossaryItem
    = GlossaryItem
        { nonDisambiguatedPreferredTerm : Term
        , disambiguatedPreferredTerm : Term
        , alternativeTerms : List Term
        , tags : List TagInItem
        , definition : Maybe Definition
        , relatedPreferredTerms : List RelatedTerm
        , needsUpdating : Bool
        , lastUpdatedDateAsIso8601 : Maybe String
        }


{-| Create a glossary item.
-}
init : Term -> List Term -> List TagInItem -> Maybe Definition -> List RelatedTerm -> Bool -> Maybe String -> GlossaryItem
init preferredTerm_ alternativeTerms_ tags_ definition_ relatedTerms_ needsUpdating_ lastUpdatedDate_ =
    let
        disambiguatedPreferredTerm_ =
            tags_
                |> disambiguationTagFromItemTags
                |> Maybe.map
                    (\disambiguationTag_ ->
                        preferredTerm_
                            |> Term.updateRaw
                                (\raw0 -> raw0 ++ " (" ++ Tag.raw disambiguationTag_ ++ ")")
                    )
                |> Maybe.withDefault preferredTerm_
    in
    GlossaryItem
        { nonDisambiguatedPreferredTerm = preferredTerm_
        , disambiguatedPreferredTerm = disambiguatedPreferredTerm_
        , alternativeTerms = alternativeTerms_
        , tags = tags_
        , definition = definition_
        , relatedPreferredTerms = relatedTerms_
        , needsUpdating = needsUpdating_
        , lastUpdatedDateAsIso8601 = lastUpdatedDate_
        }


{-| Decode a glossary item from its JSON representation.

    import Json.Decode as Decode exposing (Decoder)
    import Json.Encode as Encode
    import Data.GlossaryItem.Definition as Definition
    import Data.GlossaryItem.Term as Term

    rain : Encode.Value
    rain =
        Encode.object
            [ ( "preferredTerm"
            , Encode.object
                    [ ( "id", Encode.string "Rain" )
                    , ( "isAbbreviation", Encode.bool False )
                    , ( "body", Encode.string "Rain" )
                    ]
            )
            , ( "alternativeTerms", Encode.list Encode.object [] )
            , ( "tags", Encode.list Encode.object [] )
            , ( "definition", Encode.string "Condensed moisture." )
            , ( "relatedTerms", Encode.list Encode.object [] )
            , ( "needsUpdating", Encode.bool True )
            ]

    expected : GlossaryItem
    expected =
        init
            (Term.fromPlaintext "Rain" False)
            []
            []
            (Just <| Definition.fromPlaintext "Condensed moisture.")
            []
            True
            Nothing

    Decode.decodeValue (decode False) rain
    --> Ok expected

-}
decode : Bool -> Decoder GlossaryItem
decode enableMarkdownBasedSyntax =
    Decode.map7
        init
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


disambiguationTagFromItemTags : List TagInItem -> Maybe Tag
disambiguationTagFromItemTags =
    List.filterMap
        (\tagInItem ->
            case tagInItem of
                DisambiguationTag tag ->
                    Just tag

                NormalTag _ ->
                    Nothing
        )
        >> List.head


{-| The disambiguation tag for an item, if it has one.
-}
disambiguationTag : GlossaryItem -> Maybe Tag
disambiguationTag glossaryItem =
    case glossaryItem of
        GlossaryItem item ->
            item.tags |> disambiguationTagFromItemTags


{-| Whether or not the glossary item has a definition.
Some items may not have one and instead point to a related item that is preferred.

    import Data.GlossaryItem.Term as Term exposing (Term)

    empty : GlossaryItem
    empty =
        init
          Term.emptyPlaintext
          []
          []
          Nothing
          []
          False
          Nothing

    hasADefinition empty --> False

-}
hasADefinition : GlossaryItem -> Bool
hasADefinition glossaryItem =
    case glossaryItem of
        GlossaryItem item ->
            item.definition /= Nothing


{-| The disambiguated preferred term of the glossary item.
If the item has no disambiguation tag then this is just the preferred term.
Otherwise, it is the preferred term followed by the disambiguation tag in parenthesis.
-}
preferredTerm : GlossaryItem -> Term
preferredTerm glossaryItem =
    case glossaryItem of
        GlossaryItem item ->
            item.disambiguatedPreferredTerm


{-| The non-disambiguated preferred term of the glossary item.
-}
nonDisambiguatedPreferredTerm : GlossaryItem -> Term
nonDisambiguatedPreferredTerm glossaryItem =
    case glossaryItem of
        GlossaryItem item ->
            item.nonDisambiguatedPreferredTerm


{-| The alternative terms of the glossary item.
-}
alternativeTerms : GlossaryItem -> List Term
alternativeTerms glossaryItem =
    case glossaryItem of
        GlossaryItem item ->
            item.alternativeTerms


{-| The terms of the glossary item, both preferred and alternative.
-}
allTerms : GlossaryItem -> List Term
allTerms glossaryItem =
    preferredTerm glossaryItem :: alternativeTerms glossaryItem


{-| The tags of the glossary item.
-}
tags : GlossaryItem -> List TagInItem
tags glossaryItem =
    case glossaryItem of
        GlossaryItem item ->
            item.tags


{-| The definition of the glossary item.
-}
definition : GlossaryItem -> Maybe Definition
definition glossaryItem =
    case glossaryItem of
        GlossaryItem item ->
            item.definition


{-| The related preferred terms of the glossary item.
-}
relatedPreferredTerms : GlossaryItem -> List RelatedTerm
relatedPreferredTerms glossaryItem =
    case glossaryItem of
        GlossaryItem item ->
            item.relatedPreferredTerms


{-| Whether the glossary item is marked as needing updating.
-}
needsUpdating : GlossaryItem -> Bool
needsUpdating glossaryItem =
    case glossaryItem of
        GlossaryItem item ->
            item.needsUpdating


{-| The last updated date of the glossary item, if present.
-}
lastUpdatedDate : GlossaryItem -> Maybe String
lastUpdatedDate glossaryItem =
    case glossaryItem of
        GlossaryItem item ->
            item.lastUpdatedDateAsIso8601


{-| Update a glossary item's tags.
-}
updateTags : List TagInItem -> GlossaryItem -> GlossaryItem
updateTags newTags glossaryItem =
    case glossaryItem of
        GlossaryItem item ->
            init
                item.nonDisambiguatedPreferredTerm
                item.alternativeTerms
                newTags
                item.definition
                item.relatedPreferredTerms
                item.needsUpdating
                item.lastUpdatedDateAsIso8601


{-| Update a glossary item's related terms.
-}
updateRelatedTerms : List RelatedTerm -> GlossaryItem -> GlossaryItem
updateRelatedTerms newRelatedTerms glossaryItem =
    case glossaryItem of
        GlossaryItem item ->
            init
                item.nonDisambiguatedPreferredTerm
                item.alternativeTerms
                item.tags
                item.definition
                newRelatedTerms
                item.needsUpdating
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


{-| Represent this glossary item as an HTML tree, ready for writing back to the glossary's HTML file.
-}
toHtmlTree : GlossaryItem -> HtmlTree
toHtmlTree glossaryItem =
    case glossaryItem of
        GlossaryItem item ->
            HtmlTree.Node "div"
                True
                (item.lastUpdatedDateAsIso8601
                    |> Maybe.map
                        (\lastUpdatedDate_ ->
                            [ HtmlTree.Attribute "data-last-updated" lastUpdatedDate_ ]
                        )
                    |> Maybe.withDefault []
                )
                (List.map termToHtmlTree (allTerms glossaryItem)
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


hrefToTerm : Term -> HtmlTree.Attribute
hrefToTerm term =
    HtmlTree.Attribute "href" <| fragmentOnly <| TermId.toString <| Term.id term


hrefFromRelatedTerm : RelatedTerm -> HtmlTree.Attribute
hrefFromRelatedTerm relatedTerm =
    HtmlTree.Attribute "href" <| fragmentOnly <| TermId.toString <| RelatedTerm.idReference relatedTerm
