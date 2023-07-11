module Data.GlossaryItem exposing
    ( GlossaryItem, decode, hasSomeDefinitions
    , toHtmlTree
    )

{-| An item in a glossary.


# Glossary Items

@docs GlossaryItem, decode, hasSomeDefinitions


# Converting to HTML

@docs toHtmlTree

-}

import Data.GlossaryItem.Definition as Definition exposing (Definition)
import Data.GlossaryItem.RelatedTerm as RelatedTerm exposing (RelatedTerm)
import Data.GlossaryItem.Term as Term exposing (Term)
import Extras.HtmlTree as HtmlTree exposing (HtmlTree)
import Extras.Url exposing (fragmentOnly)
import Json.Decode as Decode exposing (Decoder)


{-| An item in a glossary, consisting of a list of terms (synonyms) being defined, a list of (alternative) definitions for those terms, and a list of related terms.
It's probably unusual to have multiple definitions for a term (e.g. "Apple" being a fruit as well as a company) because a glossary would typically be focused on a single domain.
However, this is allowed in `<dl>` elements so it's also allowed here.
-}
type alias GlossaryItem =
    { terms : List Term
    , definitions : List Definition
    , relatedTerms : List RelatedTerm
    , needsUpdating : Bool
    , lastUpdatedDate : Maybe String -- expected to be in ISO 8601 format
    }


{-| Decode a glossary item from its JSON representation.

    import Json.Decode as Decode exposing (Decoder)
    import Json.Encode as Encode
    import Data.GlossaryItem.Definition as Definition
    import Data.GlossaryItem.Term as Term

    rain : Encode.Value
    rain =
        Encode.object
            [ ( "terms"
            , Encode.list Encode.object
                    [ [ ( "id", Encode.string "Rain" )
                      , ( "isAbbreviation", Encode.bool False )
                      , ( "body", Encode.string "Rain" )
                    ]
                    ]
            )
            , ( "definitions", Encode.list Encode.string [ "Condensed moisture." ] )
            , ( "relatedTerms", Encode.list Encode.object [] )
            , ( "needsUpdating", Encode.bool True )
            ]

    Decode.decodeValue (decode False) rain
    --> Ok
    -->     { terms = [ Term.fromPlaintext "Rain" False ]
    -->     , definitions = [ Definition.fromPlaintext "Condensed moisture." ]
    -->     , relatedTerms = []
    -->     , needsUpdating = True
    -->     , lastUpdatedDate = Nothing
    -->     }

-}
decode : Bool -> Decoder GlossaryItem
decode enableMarkdownBasedSyntax =
    Decode.map5 GlossaryItem
        (Decode.field "terms" <| Decode.list <| Term.decode enableMarkdownBasedSyntax)
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
        { terms = [ Term.emptyPlaintext ]
        , definitions = []
        , relatedTerms = []
        , needsUpdating = False
        , lastUpdatedDate = Nothing
        }

    hasSomeDefinitions empty --> False

-}
hasSomeDefinitions : GlossaryItem -> Bool
hasSomeDefinitions glossaryItem =
    not <| List.isEmpty glossaryItem.definitions


termToHtmlTree : Term -> HtmlTree
termToHtmlTree term =
    HtmlTree.Node "dt"
        True
        []
        [ HtmlTree.Node "dfn"
            True
            [ HtmlTree.Attribute "id" <| Term.id term ]
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
nonemptyRelatedTermsToHtmlTree itemHasSomeDefinitions relatedTerms =
    HtmlTree.Node "dd"
        False
        [ HtmlTree.Attribute "class" "related-terms" ]
        (HtmlTree.Leaf
            (if itemHasSomeDefinitions then
                "See also: "

             else
                "See: "
            )
            :: (relatedTerms
                    |> List.map relatedTermToHtmlTree
                    |> List.intersperse (HtmlTree.Leaf ", ")
               )
        )


{-| Represent this glossary item as an HTML tree, ready for writing back to the glossary's HTML file.
-}
toHtmlTree : GlossaryItem -> HtmlTree
toHtmlTree glossaryItem =
    HtmlTree.Node "div"
        True
        (glossaryItem.lastUpdatedDate
            |> Maybe.map
                (\lastUpdatedDate ->
                    [ HtmlTree.Attribute "data-last-updated" lastUpdatedDate ]
                )
            |> Maybe.withDefault []
        )
        (List.map termToHtmlTree glossaryItem.terms
            ++ (if glossaryItem.needsUpdating then
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
            ++ List.map (Definition.raw >> definitionToHtmlTree) glossaryItem.definitions
            ++ (if List.isEmpty glossaryItem.relatedTerms then
                    []

                else
                    [ nonemptyRelatedTermsToHtmlTree
                        (hasSomeDefinitions glossaryItem)
                        glossaryItem.relatedTerms
                    ]
               )
        )


hrefToTerm : Term -> HtmlTree.Attribute
hrefToTerm term =
    HtmlTree.Attribute "href" <| fragmentOnly <| Term.id term


hrefFromRelatedTerm : RelatedTerm -> HtmlTree.Attribute
hrefFromRelatedTerm relatedTerm =
    HtmlTree.Attribute "href" <| fragmentOnly <| RelatedTerm.idReference relatedTerm
