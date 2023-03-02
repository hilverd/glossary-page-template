module Data.GlossaryItem exposing
    ( GlossaryItem, empty, decode, hasSomeDetails
    , toHtmlTree
    )

{-| An item in a glossary.


# Glossary Items

@docs GlossaryItem, empty, decode, hasSomeDetails


# Converting to HTML

@docs toHtmlTree

-}

import Data.GlossaryItem.Details as Details exposing (Details)
import Data.GlossaryItem.RelatedTerm as RelatedTerm exposing (RelatedTerm)
import Data.GlossaryItem.Term as Term exposing (Term)
import Extras.HtmlTree as HtmlTree exposing (HtmlTree)
import Extras.Url exposing (fragmentOnly)
import Json.Decode as Decode exposing (Decoder)


{-| An item in a glossary, consisting of a list of terms (synonyms) being defined, a list of (alternative) definitions for those terms, and a list of related terms.
It's probably unusual to have multiple definitions for a term (e.g. "Apple" being a fruit as well as a company) because a glossary would typically be focused on a single domain.
However, this is allowed in the `<dl>` element so it's also allowed here.
-}
type alias GlossaryItem =
    { terms : List Term
    , details : List Details
    , relatedTerms : List RelatedTerm
    }


{-| An empty glossary item, used as a starting point when creating a new one from a form.
-}
empty : GlossaryItem
empty =
    { terms = [ Term.emptyPlaintext ]
    , details = []
    , relatedTerms = []
    }


{-| Decode a glossary item from its JSON representation.

    import Json.Decode as Decode exposing (Decoder)
    import Json.Encode as Encode
    import Data.GlossaryItem.Details as Details
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
            , ( "details", Encode.list Encode.string [ "Condensed moisture." ] )
            , ( "relatedTerms", Encode.list Encode.object [] )
            ]

    Decode.decodeValue (decode False) rain
    --> Ok
    -->     { terms = [ Term.fromPlaintext "Rain" False ]
    -->     , details = [ Details.fromPlaintext "Condensed moisture." ]
    -->     , relatedTerms = []
    -->     }

-}
decode : Bool -> Decoder GlossaryItem
decode enableMarkdownBasedSyntax =
    Decode.map3 GlossaryItem
        (Decode.field "terms" <| Decode.list <| Term.decode enableMarkdownBasedSyntax)
        (Decode.field "details" <|
            Decode.list <|
                Decode.map
                    (if enableMarkdownBasedSyntax then
                        Details.fromMarkdown

                     else
                        Details.fromPlaintext
                    )
                <|
                    Decode.string
        )
        (Decode.field "relatedTerms" <| Decode.list <| RelatedTerm.decode)


{-| Whether or not the glossary item has any details.
Some items may not contain any details and instead point to a related item that is preferred.

    hasSomeDetails empty --> False

-}
hasSomeDetails : GlossaryItem -> Bool
hasSomeDetails glossaryItem =
    not <| List.isEmpty glossaryItem.details


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


detailsToHtmlTree : String -> HtmlTree
detailsToHtmlTree details =
    HtmlTree.Node "dd"
        False
        []
        [ HtmlTree.Leaf details ]


relatedTermToHtmlTree : RelatedTerm -> HtmlTree
relatedTermToHtmlTree relatedTerm =
    HtmlTree.Node "a"
        True
        [ hrefFromRelatedTerm relatedTerm ]
        [ HtmlTree.Leaf <| RelatedTerm.raw relatedTerm ]


nonemptyRelatedTermsToHtmlTree : Bool -> List RelatedTerm -> HtmlTree
nonemptyRelatedTermsToHtmlTree itemHasSomeDetails relatedTerms =
    HtmlTree.Node "dd"
        False
        [ HtmlTree.Attribute "class" "related-terms" ]
        (HtmlTree.Leaf
            (if itemHasSomeDetails then
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
        []
        (List.map termToHtmlTree glossaryItem.terms
            ++ List.map (Details.raw >> detailsToHtmlTree) glossaryItem.details
            ++ (if List.isEmpty glossaryItem.relatedTerms then
                    []

                else
                    [ nonemptyRelatedTermsToHtmlTree
                        (hasSomeDetails glossaryItem)
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
