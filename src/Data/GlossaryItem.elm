module Data.GlossaryItem exposing (GlossaryItem, RelatedTerm, Term, decode, empty, hasSomeDetails, toHtmlTree)

import Extras.HtmlTree as HtmlTree exposing (HtmlTree)
import Json.Decode as Decode exposing (Decoder)


type alias Term =
    { id : String
    , isAbbreviation : Bool
    , body : String
    }


type alias RelatedTerm =
    { idReference : String
    , body : String
    }


type alias GlossaryItem =
    { terms : List Term
    , details : List String
    , relatedTerms : List RelatedTerm
    }


empty : GlossaryItem
empty =
    { terms = [ Term "" False "" ]
    , details = []
    , relatedTerms = []
    }


decodeTerm : Decoder Term
decodeTerm =
    Decode.map3 Term
        (Decode.field "id" <| Decode.string)
        (Decode.field "isAbbreviation" Decode.bool)
        (Decode.field "body" Decode.string)


decodeRelatedTerms : Decoder RelatedTerm
decodeRelatedTerms =
    Decode.map2 RelatedTerm
        (Decode.field "idReference" Decode.string)
        (Decode.field "body" Decode.string)


decode : Decoder GlossaryItem
decode =
    Decode.map3 GlossaryItem
        (Decode.field "terms" <| Decode.list <| decodeTerm)
        (Decode.field "details" <| Decode.list <| Decode.string)
        (Decode.field "relatedTerms" <| Decode.list <| decodeRelatedTerms)


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
            [ HtmlTree.Attribute "id" term.id ]
            [ HtmlTree.Leaf term.body
                |> (\inner ->
                        if term.isAbbreviation then
                            HtmlTree.Node "abbr" True [] [ inner ]

                        else
                            inner
                   )
            ]
        ]


detailsToHtmlTree : String -> HtmlTree
detailsToHtmlTree details =
    HtmlTree.Node "dd"
        True
        []
        [ HtmlTree.Leaf details ]


relatedTermToHtmlTree : RelatedTerm -> HtmlTree
relatedTermToHtmlTree relatedTerm =
    HtmlTree.Node "a"
        True
        [ HtmlTree.Attribute "href" ("#" ++ relatedTerm.idReference) ]
        [ HtmlTree.Leaf relatedTerm.body ]


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


toHtmlTree : GlossaryItem -> HtmlTree
toHtmlTree glossaryItem =
    HtmlTree.Node "div"
        True
        []
        (List.map termToHtmlTree glossaryItem.terms
            ++ List.map detailsToHtmlTree glossaryItem.details
            ++ (if List.isEmpty glossaryItem.relatedTerms then
                    []

                else
                    [ nonemptyRelatedTermsToHtmlTree
                        (hasSomeDetails glossaryItem)
                        glossaryItem.relatedTerms
                    ]
               )
        )
