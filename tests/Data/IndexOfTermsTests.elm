module Data.IndexOfTermsTests exposing (suite)

import Data.DescribedTag as DescribedTag
import Data.GlossaryItem.Definition as Definition
import Data.GlossaryItem.DisambiguatedTerm as DisambiguatedTerm
import Data.GlossaryItem.Tag as Tag
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItemForUi as GlossaryItemForUi exposing (GlossaryItemForUi)
import Data.GlossaryItemId as GlossaryItemId
import Data.GlossaryItemsForUi as GlossaryItemsForUi exposing (GlossaryItemsForUi)
import Data.IndexOfTerms as IndexOfTerms
import Data.TagDescription as TagDescription
import Data.TagId as TagId
import Expect
import Test exposing (Test, describe, test)


termFromBody : String -> Term
termFromBody body =
    Term.fromMarkdown body False


glossaryItemForUi : String -> GlossaryItemForUi
glossaryItemForUi body =
    GlossaryItemForUi.create
        (GlossaryItemId.create body)
        (termFromBody body)
        []
        Nothing
        []
        (Just (Definition.fromMarkdown body))
        []
        False
        Nothing
        Nothing
        Nothing


glossaryItems : GlossaryItemsForUi
glossaryItems =
    [ glossaryItemForUi "007"
    , glossaryItemForUi "Óne"
    , glossaryItemForUi "Two"
    , glossaryItemForUi "3040"
    , glossaryItemForUi "3Three"
    , glossaryItemForUi "Ω"
    , glossaryItemForUi "_future_"
    ]
        |> GlossaryItemsForUi.fromList [] Nothing
        |> Result.withDefault GlossaryItemsForUi.empty


suite : Test
suite =
    describe "The Data.IndexOfTerms module"
        [ test "sorts terms alphabetically by their first alphabetic character (stripped of any diacritical marks)" <|
            \_ ->
                let
                    preferredTerm : String -> IndexOfTerms.Entry
                    preferredTerm body =
                        IndexOfTerms.PreferredTerm (GlossaryItemId.create body) (DisambiguatedTerm.fromTerm <| termFromBody body) False
                in
                glossaryItems
                    |> IndexOfTerms.fromGlossaryItems Nothing
                    |> IndexOfTerms.termGroups
                    |> Expect.equal
                        [ { label = "0–9"
                          , entries =
                                [ preferredTerm "007"
                                , preferredTerm "3040"
                                , preferredTerm "3Three"
                                ]
                          }
                        , { label = "A", entries = [] }
                        , { label = "B", entries = [] }
                        , { label = "C", entries = [] }
                        , { label = "D", entries = [] }
                        , { label = "E", entries = [] }
                        , { label = "F", entries = [ preferredTerm "_future_" ] }
                        , { label = "G", entries = [] }
                        , { label = "H", entries = [] }
                        , { label = "I", entries = [] }
                        , { label = "J", entries = [] }
                        , { label = "K", entries = [] }
                        , { label = "L", entries = [] }
                        , { label = "M", entries = [] }
                        , { label = "N", entries = [] }
                        , { label = "O", entries = [ preferredTerm "Óne" ] }
                        , { label = "P", entries = [] }
                        , { label = "Q", entries = [] }
                        , { label = "R", entries = [] }
                        , { label = "S", entries = [] }
                        , { label = "T", entries = [ preferredTerm "Two" ] }
                        , { label = "U", entries = [] }
                        , { label = "V", entries = [] }
                        , { label = "W", entries = [] }
                        , { label = "X", entries = [] }
                        , { label = "Y", entries = [] }
                        , { label = "Z", entries = [] }
                        , { label = "…", entries = [ preferredTerm "Ω" ] }
                        ]
        , test "doesn't include 0–9 and ellipsis if not needed" <|
            \_ ->
                []
                    |> GlossaryItemsForUi.fromList [] Nothing
                    |> Result.map (IndexOfTerms.fromGlossaryItems Nothing)
                    |> Result.map IndexOfTerms.termGroups
                    |> Expect.equal
                        (Ok
                            [ { label = "A", entries = [] }
                            , { label = "B", entries = [] }
                            , { label = "C", entries = [] }
                            , { label = "D", entries = [] }
                            , { label = "E", entries = [] }
                            , { label = "F", entries = [] }
                            , { label = "G", entries = [] }
                            , { label = "H", entries = [] }
                            , { label = "I", entries = [] }
                            , { label = "J", entries = [] }
                            , { label = "K", entries = [] }
                            , { label = "L", entries = [] }
                            , { label = "M", entries = [] }
                            , { label = "N", entries = [] }
                            , { label = "O", entries = [] }
                            , { label = "P", entries = [] }
                            , { label = "Q", entries = [] }
                            , { label = "R", entries = [] }
                            , { label = "S", entries = [] }
                            , { label = "T", entries = [] }
                            , { label = "U", entries = [] }
                            , { label = "V", entries = [] }
                            , { label = "W", entries = [] }
                            , { label = "X", entries = [] }
                            , { label = "Y", entries = [] }
                            , { label = "Z", entries = [] }
                            ]
                        )
        , test "marks items as isForTag when their disambiguated preferred term matches a tag" <|
            \_ ->
                let
                    -- Define the described tags first
                    appleTag =
                        DescribedTag.create
                            (TagId.create "apple-tag")
                            (Tag.fromMarkdown "Apple")
                            (TagDescription.fromMarkdown "")

                    fruitTag =
                        DescribedTag.create
                            (TagId.create "fruit-tag")
                            (Tag.fromMarkdown "Fruit")
                            (TagDescription.fromMarkdown "")

                    -- Create an item "Apple" with a tag "Apple"
                    itemWithMatchingTag : GlossaryItemForUi
                    itemWithMatchingTag =
                        GlossaryItemForUi.create
                            (GlossaryItemId.create "apple-id")
                            (termFromBody "Apple")
                            []
                            Nothing
                            [ Tag.fromMarkdown "Apple" ]
                            (Just (Definition.fromMarkdown "A fruit"))
                            []
                            False
                            Nothing
                            Nothing
                            Nothing

                    -- Create an item "Banana" without a matching tag
                    itemWithoutMatchingTag : GlossaryItemForUi
                    itemWithoutMatchingTag =
                        GlossaryItemForUi.create
                            (GlossaryItemId.create "banana-id")
                            (termFromBody "Banana")
                            []
                            Nothing
                            [ Tag.fromMarkdown "Fruit" ]
                            (Just (Definition.fromMarkdown "Another fruit"))
                            []
                            False
                            Nothing
                            Nothing
                            Nothing

                    items : GlossaryItemsForUi
                    items =
                        [ itemWithMatchingTag, itemWithoutMatchingTag ]
                            |> GlossaryItemsForUi.fromList [ appleTag, fruitTag ] Nothing
                            |> Result.withDefault GlossaryItemsForUi.empty

                    index : IndexOfTerms.IndexOfTerms
                    index =
                        IndexOfTerms.fromGlossaryItems Nothing items

                    appleEntry : IndexOfTerms.Entry
                    appleEntry =
                        IndexOfTerms.PreferredTerm
                            (GlossaryItemId.create "apple-id")
                            (DisambiguatedTerm.fromTerm <| termFromBody "Apple")
                            True

                    bananaEntry : IndexOfTerms.Entry
                    bananaEntry =
                        IndexOfTerms.PreferredTerm
                            (GlossaryItemId.create "banana-id")
                            (DisambiguatedTerm.fromTerm <| termFromBody "Banana")
                            False

                    -- Verify Apple has isForTag=True and Banana has isForTag=False
                    entriesByLabel : List ( String, List IndexOfTerms.Entry )
                    entriesByLabel =
                        index
                            |> IndexOfTerms.termGroups
                            |> List.filter (\group -> group.label == "A" || group.label == "B")
                            |> List.map (\group -> ( group.label, group.entries ))
                in
                Expect.all
                    [ \_ ->
                        entriesByLabel
                            |> Expect.equal
                                [ ( "A", [ appleEntry ] )
                                , ( "B", [ bananaEntry ] )
                                ]
                    , \_ ->
                        -- Verify the underlying isItemForTag function works correctly
                        GlossaryItemForUi.isItemForTag itemWithMatchingTag
                            |> Expect.equal True
                    , \_ ->
                        GlossaryItemForUi.isItemForTag itemWithoutMatchingTag
                            |> Expect.equal False
                    ]
                    ()
        ]
