module Data.GlossaryItemFromDom exposing
    ( GlossaryItemFromDom
    , create, codec
    , disambiguatedPreferredTermIdString
    , toHtmlTree
    )

{-| A glossary item as sent to Elm by JavaScript when read from the DOM.

@docs GlossaryItemFromDom


# Build

@docs create, codec


# Query

@docs disambiguatedPreferredTermIdString


# Export

@docs toHtmlTree

-}

import Codec exposing (Codec)
import Data.GlossaryItem.TermFromDom as TermFromDom exposing (TermFromDom)
import Extras.HtmlTree as HtmlTree exposing (HtmlTree)
import Extras.Url
import Internationalisation as I18n


{-| A glossary item read from the DOM.
-}
type alias GlossaryItemFromDom =
    { id : String
    , preferredTerm : TermFromDom
    , alternativeTerms : List TermFromDom
    , disambiguationTag : Maybe String
    , normalTags : List String
    , definition : Maybe String
    , relatedPreferredTerms : List TermFromDom
    , needsUpdating : Bool
    , lastUpdatedDateAsIso8601 : Maybe String
    , lastUpdatedByName : Maybe String
    , lastUpdatedByEmailAddress : Maybe String
    }


{-| Create a glossary item from its parts.
-}
create :
    String
    -> TermFromDom
    -> List TermFromDom
    -> Maybe String
    -> List String
    -> Maybe String
    -> List TermFromDom
    -> Bool
    -> Maybe String
    -> Maybe String
    -> Maybe String
    -> GlossaryItemFromDom
create id_ preferredTerm_ alternativeTerms_ disambiguationTag_ normalTags_ definition_ relatedPreferredTerms_ needsUpdating_ lastUpdatedDateAsIso8601_ lastUpdatedByName_ lastUpdatedByEmailAddress_ =
    { id = id_
    , preferredTerm = preferredTerm_
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


{-| Convert a GlossaryItemFromDom to/from its JSON representation.
-}
codec : Codec GlossaryItemFromDom
codec =
    Codec.object
        create
        |> Codec.field "id" .id Codec.string
        |> Codec.field "preferredTerm" .preferredTerm TermFromDom.codec
        |> Codec.field "alternativeTerms" .alternativeTerms (Codec.list TermFromDom.codec)
        |> Codec.field "disambiguationTag" .disambiguationTag (Codec.nullable Codec.string)
        |> Codec.field "normalTags" .normalTags (Codec.list Codec.string)
        |> Codec.field "definition" .definition (Codec.nullable Codec.string)
        |> Codec.field "relatedTerms" .relatedPreferredTerms (Codec.list TermFromDom.codec)
        |> Codec.field "needsUpdating" .needsUpdating Codec.bool
        |> Codec.field "lastUpdatedDate" .lastUpdatedDateAsIso8601 (Codec.maybe Codec.string)
        |> Codec.field "lastUpdatedByName" .lastUpdatedByName (Codec.maybe Codec.string)
        |> Codec.field "lastUpdatedByEmailAddress" .lastUpdatedByEmailAddress (Codec.maybe Codec.string)
        |> Codec.buildObject


allTags : GlossaryItemFromDom -> List String
allTags glossaryItemFromDom =
    glossaryItemFromDom.disambiguationTag
        |> Maybe.map (\disambiguationTag_ -> disambiguationTag_ :: glossaryItemFromDom.normalTags)
        |> Maybe.withDefault glossaryItemFromDom.normalTags


preferredTermToHtmlTree : Maybe String -> String -> TermFromDom -> HtmlTree
preferredTermToHtmlTree disambiguationTag_ disambiguatedTermIdString_ term =
    HtmlTree.Node "dt"
        True
        []
        [ HtmlTree.Node "dfn"
            True
            [ HtmlTree.Attribute "id" disambiguatedTermIdString_ ]
            [ (disambiguationTag_
                |> Maybe.map
                    (\disambiguationTag0 ->
                        [ HtmlTree.Node "span"
                            False
                            []
                            [ HtmlTree.Leaf term.body ]
                        , HtmlTree.Node "span"
                            False
                            [ HtmlTree.Attribute "class" "disambiguation" ]
                            [ HtmlTree.Leaf <| "(" ++ disambiguationTag0 ++ ")" ]
                        ]
                    )
                |> Maybe.withDefault [ HtmlTree.Leaf term.body ]
              )
                |> (\inner ->
                        let
                            linkedTerm : HtmlTree
                            linkedTerm =
                                HtmlTree.Node "a"
                                    True
                                    [ HtmlTree.Attribute "href" <| Extras.Url.fragmentOnly disambiguatedTermIdString_ ]
                                    inner
                        in
                        if term.isAbbreviation then
                            HtmlTree.Node "abbr" True [] [ linkedTerm ]

                        else
                            linkedTerm
                   )
            ]
        ]


alternativeTermToHtmlTree : TermFromDom -> HtmlTree
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
                    HtmlTree.Leaf term.body
              in
              if term.isAbbreviation then
                HtmlTree.Node "abbr" True [] [ termHtmlTree ]

              else
                termHtmlTree
            ]
        ]


relatedTermToHtmlTree : TermFromDom -> HtmlTree
relatedTermToHtmlTree disambiguatedTerm_ =
    let
        term =
            disambiguatedTerm_
    in
    HtmlTree.Node "a"
        True
        [ hrefFromRelatedTerm term ]
        [ HtmlTree.Leaf term.body ]


hrefFromRelatedTerm : TermFromDom -> HtmlTree.Attribute
hrefFromRelatedTerm term =
    HtmlTree.Attribute "href" <| Extras.Url.fragmentOnly <| TermFromDom.id term


{-| The HTML ID of the disambiguated preferred term.
-}
disambiguatedPreferredTermIdString : GlossaryItemFromDom -> String
disambiguatedPreferredTermIdString glossaryItemFromDom =
    glossaryItemFromDom.disambiguationTag
        |> Maybe.map (\tag -> disambiguatedTerm tag glossaryItemFromDom.preferredTerm)
        |> Maybe.withDefault glossaryItemFromDom.preferredTerm
        |> TermFromDom.id


{-| Disambiguate a term by appending the given tag in parentheses.
-}
disambiguatedTerm : String -> TermFromDom -> TermFromDom
disambiguatedTerm tag term =
    { isAbbreviation = term.isAbbreviation
    , body = term.body ++ " (" ++ tag ++ ")"
    }


definitionToHtmlTree : String -> HtmlTree
definitionToHtmlTree definition_ =
    HtmlTree.Node "dd"
        False
        []
        [ HtmlTree.Leaf definition_ ]


nonemptyRelatedTermsToHtmlTree : Bool -> List TermFromDom -> HtmlTree
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


{-| Whether or not the glossary item has a definition.
Some items may not have one and instead point to a related item that is preferred.
-}
hasADefinition : GlossaryItemFromDom -> Bool
hasADefinition glossaryItemFromDom =
    glossaryItemFromDom.definition /= Nothing


{-| Represent this GlossaryItemFromDom as an HTML tree, ready for writing back to the glossary's HTML file.
-}
toHtmlTree : GlossaryItemFromDom -> HtmlTree
toHtmlTree glossaryItemFromDom =
    let
        item =
            glossaryItemFromDom

        allTags_ : List String
        allTags_ =
            allTags glossaryItemFromDom
    in
    HtmlTree.Node "div"
        True
        [ HtmlTree.Attribute "data-id" item.id
        , HtmlTree.showAttributeMaybe "data-last-updated" identity item.lastUpdatedDateAsIso8601
        , HtmlTree.showAttributeMaybe "data-last-updated-by-name" identity item.lastUpdatedByName
        , HtmlTree.showAttributeMaybe "data-last-updated-by-email-address" identity item.lastUpdatedByEmailAddress
        ]
        (preferredTermToHtmlTree item.disambiguationTag (disambiguatedPreferredTermIdString glossaryItemFromDom) item.preferredTerm
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
                                    [ HtmlTree.Leaf tag ]
                            )
                            allTags_
                        )
                    ]
               )
            ++ List.map
                definitionToHtmlTree
                (item.definition |> Maybe.map List.singleton |> Maybe.withDefault [])
            ++ (if List.isEmpty item.relatedPreferredTerms then
                    []

                else
                    [ nonemptyRelatedTermsToHtmlTree
                        (hasADefinition glossaryItemFromDom)
                        item.relatedPreferredTerms
                    ]
               )
        )
