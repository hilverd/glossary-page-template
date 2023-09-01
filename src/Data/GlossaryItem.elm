module Data.GlossaryItem exposing
    ( GlossaryItem, init, decode, terms, hasSomeDefinitions, definitions, needsUpdating, lastUpdatedDate, updateRelatedTerms
    , toHtmlTree
    , relatedPreferredTerms
    )

{-| An item in a glossary.


# Glossary Items

@docs GlossaryItem, init, decode, terms, hasSomeDefinitions, definitions, relatedTerms, needsUpdating, lastUpdatedDate, updateRelatedTerms


# Converting to HTML

@docs toHtmlTree

-}

import Data.GlossaryItem.Definition as Definition exposing (Definition)
import Data.GlossaryItem.RelatedTerm as RelatedTerm exposing (RelatedTerm)
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItem.TermId as TermId
import Extras.HtmlTree as HtmlTree exposing (HtmlTree)
import Extras.Url exposing (fragmentOnly)
import Json.Decode as Decode exposing (Decoder)


{-| An item in a glossary, consisting of a list of terms (synonyms) being defined, a list of (alternative) definitions for those terms, and a list of related preferred terms.
It's probably unusual to have multiple definitions for a term (e.g. "Apple" being a fruit as well as a company) because a glossary would typically be focused on a single domain.
However, this is allowed in `<dl>` elements so it's also allowed here.
-}
type GlossaryItem
    = GlossaryItem
        { terms : List Term
        , definitions : List Definition
        , relatedPreferredTerms : List RelatedTerm
        , needsUpdating : Bool
        , lastUpdatedDateAsIso8601 : Maybe String
        }


{-| Create a glossary item.
-}
init : Maybe Term -> List Term -> List Definition -> List RelatedTerm -> Bool -> Maybe String -> GlossaryItem
init preferredTerm_ alternativeTerms_ definitions_ relatedTerms_ needsUpdating_ lastUpdatedDate_ =
    GlossaryItem
        { terms = (preferredTerm_ |> Maybe.map List.singleton |> Maybe.withDefault []) ++ alternativeTerms_
        , definitions = definitions_
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
            , ( "definitions", Encode.list Encode.string [ "Condensed moisture." ] )
            , ( "relatedTerms", Encode.list Encode.object [] )
            , ( "needsUpdating", Encode.bool True )
            ]

    expected : GlossaryItem
    expected =
        init
            (Just (Term.fromPlaintext "Rain" False))
            []
            [ Definition.fromPlaintext "Condensed moisture." ]
            []
            True
            Nothing

    Decode.decodeValue (decode False) rain
    --> Ok expected

-}
decode : Bool -> Decoder GlossaryItem
decode enableMarkdownBasedSyntax =
    Decode.map6
        (\preferredTerm_ alternativeTerms_ definitions_ relatedTerms_ needsUpdating_ lastUpdatedDate_ ->
            GlossaryItem
                { terms = preferredTerm_ :: alternativeTerms_
                , definitions = definitions_
                , relatedPreferredTerms = relatedTerms_
                , needsUpdating = needsUpdating_
                , lastUpdatedDateAsIso8601 = lastUpdatedDate_
                }
        )
        (Decode.field "preferredTerm" <| Term.decode enableMarkdownBasedSyntax)
        (Decode.field "alternativeTerms" <| Decode.list <| Term.decode enableMarkdownBasedSyntax)
        (Decode.field "definitions" <|
            Decode.list <|
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


{-| Whether or not the glossary item has any definitions.
Some items may not contain any definitions and instead point to a related item that is preferred.

    import Data.GlossaryItem.Term as Term exposing (Term)

    empty : GlossaryItem
    empty =
        init
          (Just (Term.emptyPlaintext))
          []
          []
          []
          False
          Nothing

    hasSomeDefinitions empty --> False

-}
hasSomeDefinitions : GlossaryItem -> Bool
hasSomeDefinitions glossaryItem =
    case glossaryItem of
        GlossaryItem item ->
            not <| List.isEmpty item.definitions


{-| The terms in the glossary item.
-}
terms : GlossaryItem -> List Term
terms glossaryItem =
    case glossaryItem of
        GlossaryItem item ->
            item.terms


{-| The definitions in the glossary item.
-}
definitions : GlossaryItem -> List Definition
definitions glossaryItem =
    case glossaryItem of
        GlossaryItem item ->
            item.definitions


{-| The related preferred terms in the glossary item.
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


{-| Update a glossary item's relatd terms.
-}
updateRelatedTerms : List RelatedTerm -> GlossaryItem -> GlossaryItem
updateRelatedTerms newRelatedTerms glossaryItem =
    case glossaryItem of
        GlossaryItem item ->
            GlossaryItem { item | relatedPreferredTerms = newRelatedTerms }


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
definitionToHtmlTree definition =
    HtmlTree.Node "dd"
        False
        []
        [ HtmlTree.Leaf definition ]


relatedTermToHtmlTree : RelatedTerm -> HtmlTree
relatedTermToHtmlTree relatedTerm =
    HtmlTree.Node "a"
        True
        [ hrefFromRelatedTerm relatedTerm ]
        [ HtmlTree.Leaf <| RelatedTerm.raw relatedTerm ]


nonemptyRelatedTermsToHtmlTree : Bool -> List RelatedTerm -> HtmlTree
nonemptyRelatedTermsToHtmlTree itemHasSomeDefinitions relatedTerms_ =
    HtmlTree.Node "dd"
        False
        [ HtmlTree.Attribute "class" "related-terms" ]
        (HtmlTree.Leaf
            (if itemHasSomeDefinitions then
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
                (List.map termToHtmlTree item.terms
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
                    ++ List.map (Definition.raw >> definitionToHtmlTree) item.definitions
                    ++ (if List.isEmpty item.relatedPreferredTerms then
                            []

                        else
                            [ nonemptyRelatedTermsToHtmlTree
                                (hasSomeDefinitions glossaryItem)
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
