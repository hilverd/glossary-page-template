module Data.GlossaryItemsTests exposing (suite)

import Data.GlossaryItem.DisambiguatedTerm as DisambiguatedTerm
import Data.GlossaryItem.Tag as Tag
import Data.GlossaryItem.Term as Term
import Data.GlossaryItem.TermId as TermId
import Data.GlossaryItemForHtml as GlossaryItemForHtml exposing (GlossaryItemForHtml)
import Data.GlossaryItemId as GlossaryItemId exposing (GlossaryItemId)
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Data.TagId as TagId
import Data.TagsChanges as TagsChanges exposing (TagsChanges)
import Expect
import Test exposing (Test, describe, test)
import TestData exposing (..)


suite : Test
suite =
    describe "The Data.GlossaryItems module"
        [ test "inserts tags" <|
            \_ ->
                let
                    tagsChanges : TagsChanges
                    tagsChanges =
                        TagsChanges.empty
                            |> TagsChanges.insert TestData.houseworkTag TestData.houseworkTagDescription
                in
                glossaryItems
                    |> GlossaryItems.applyTagsChanges tagsChanges
                    |> Result.map GlossaryItems.tagsWithDescriptions
                    |> Expect.equal
                        (Ok
                            [ ( TestData.computerScienceTag, TestData.computerScienceTagDescription )
                            , ( TestData.financeTag, TestData.financeTagDescription )
                            , ( TestData.gardeningTag, TestData.gardeningTagDescription )
                            , ( TestData.houseworkTag, TestData.houseworkTagDescription )
                            ]
                        )
        , test "updates tags" <|
            \_ ->
                let
                    tagsChanges : TagsChanges
                    tagsChanges =
                        TagsChanges.empty
                            |> TagsChanges.update (TagId.create 0) financeTag financeTagDescription
                            |> TagsChanges.update (TagId.create 1) computerScienceTag computerScienceTagDescription
                in
                glossaryItems
                    |> GlossaryItems.applyTagsChanges tagsChanges
                    |> Result.map (GlossaryItems.orderedAlphabetically Nothing >> List.map Tuple.second)
                    |> Result.map
                        (List.map
                            (\glossaryItemForHtml ->
                                ( GlossaryItemForHtml.disambiguatedPreferredTerm glossaryItemForHtml |> DisambiguatedTerm.toTerm |> Term.raw
                                , GlossaryItemForHtml.allTags glossaryItemForHtml |> List.map Tag.raw
                                )
                            )
                        )
                    |> Expect.equal
                        (Ok
                            [ ( "Default (Computer Science)", [ "Computer Science" ] )
                            , ( "Default (Finance)", [ "Finance" ] )
                            , ( "Information retrieval", [ "Finance" ] )
                            , ( "Interest rate", [ "Computer Science" ] )
                            , ( "Loan", [ "Computer Science" ] )
                            ]
                        )
        , test "updates tags in items" <|
            \_ ->
                let
                    tagsChanges : TagsChanges
                    tagsChanges =
                        TagsChanges.empty
                            |> TagsChanges.update (TagId.create 0) houseworkTag houseworkTagDescription
                in
                glossaryItems
                    |> GlossaryItems.applyTagsChanges tagsChanges
                    |> Result.map (GlossaryItems.get <| GlossaryItemId.create 0)
                    |> Expect.equal
                        (Ok <|
                            Just <|
                                GlossaryItemForHtml.create
                                    (Term.fromMarkdown "Default" False)
                                    [ Term.fromMarkdown "Preset" False
                                    , Term.fromMarkdown "Factory preset" False
                                    ]
                                    (Just houseworkTag)
                                    []
                                    (Just defaultComputerScienceDefinition)
                                    []
                                    False
                                    (Just "2023-09-15T19:58:59.573Z")
                                    Nothing
                                    Nothing
                        )
        , test "removes tags" <|
            \_ ->
                let
                    tagsChanges : TagsChanges
                    tagsChanges =
                        TagsChanges.empty
                            |> TagsChanges.remove (TagId.create 1)
                in
                glossaryItems
                    |> GlossaryItems.applyTagsChanges tagsChanges
                    |> Result.map GlossaryItems.tagsWithDescriptions
                    |> Expect.equal
                        (Ok
                            [ ( computerScienceTag, computerScienceTagDescription )
                            , ( gardeningTag, gardeningTagDescription )
                            ]
                        )
        , test "removes tags from items" <|
            \_ ->
                let
                    tagsChanges : TagsChanges
                    tagsChanges =
                        TagsChanges.empty
                            |> TagsChanges.remove (TagId.create 1)
                in
                glossaryItems
                    |> GlossaryItems.applyTagsChanges tagsChanges
                    |> Result.map (GlossaryItems.get <| GlossaryItemId.create 1)
                    |> Expect.equal
                        (Ok <|
                            Just <|
                                GlossaryItemForHtml.create
                                    (Term.fromMarkdown "Default" False)
                                    []
                                    Nothing
                                    []
                                    (Just defaultFinanceDefinition)
                                    [ DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Loan" False ]
                                    False
                                    (Just "2023-10-30T08:25:24.765Z")
                                    Nothing
                                    Nothing
                        )
        , test "removes and inserts items" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.remove (GlossaryItemId.create 0)
                    |> Result.andThen (GlossaryItems.insert defaultComputerScienceItem)
                    |> Result.map (\( _, updatedItems ) -> GlossaryItems.orderedAlphabetically Nothing updatedItems)
                    |> Expect.equal
                        (Ok
                            [ ( GlossaryItemId.create 0, defaultComputerScienceItem )
                            , ( GlossaryItemId.create 1, defaultFinanceItem )
                            , ( GlossaryItemId.create 2, informationRetrievalItem )
                            , ( GlossaryItemId.create 3, interestRateItem )
                            , ( GlossaryItemId.create 4, loanItem )
                            ]
                        )
        , test "updates items" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.update (GlossaryItemId.create 3) updatedInterestRateItem
                    |> Result.map (GlossaryItems.orderedAlphabetically Nothing)
                    |> Expect.equal
                        (Ok
                            [ ( GlossaryItemId.create 0, defaultComputerScienceItem )
                            , ( GlossaryItemId.create 1, defaultFinanceItem )
                            , ( GlossaryItemId.create 2, informationRetrievalItem )
                            , ( GlossaryItemId.create 3, updatedInterestRateItem )
                            , ( GlossaryItemId.create 4, updatedLoanItem )
                            ]
                        )
        , test "can start with an empty set and insert tags and items" <|
            \_ ->
                {- This test is not very readable.
                   That is mainly because it needs to cope with the fact that items being inserted cannot refer to other items that have not been inserted yet.
                   To work around this, it does a first pass over the items to insert them, then a second one to update them.
                -}
                let
                    tagsChanges =
                        TagsChanges.empty
                            |> TagsChanges.insert gardeningTag gardeningTagDescription
                            |> TagsChanges.insert financeTag financeTagDescription
                            |> TagsChanges.insert computerScienceTag computerScienceTagDescription

                    glossaryItemsForHtml =
                        [ defaultComputerScienceItem
                        , defaultFinanceItem
                        , informationRetrievalItem
                        , interestRateItem
                        , loanItem
                        ]
                in
                GlossaryItems.empty
                    |> GlossaryItems.applyTagsChanges tagsChanges
                    |> (\result ->
                            List.foldl
                                (\item ->
                                    Result.andThen
                                        (\items ->
                                            GlossaryItems.insert item items
                                                |> Result.map Tuple.second
                                        )
                                )
                                result
                                glossaryItemsForHtml
                       )
                    |> (\result ->
                            let
                                itemId : GlossaryItemForHtml -> GlossaryItems -> GlossaryItemId
                                itemId glossaryItemForHtml result_ =
                                    result_
                                        |> GlossaryItems.orderedAlphabetically Nothing
                                        |> List.filterMap
                                            (\( id, glossaryItemForHtml_ ) ->
                                                if GlossaryItemForHtml.definition glossaryItemForHtml_ == GlossaryItemForHtml.definition glossaryItemForHtml then
                                                    Just id

                                                else
                                                    Nothing
                                            )
                                        |> List.head
                                        |> Maybe.withDefault (GlossaryItemId.create -1)
                            in
                            glossaryItemsForHtml
                                |> List.foldl
                                    (\glossaryItemForHtml result1 ->
                                        Result.andThen
                                            (\result1_ ->
                                                GlossaryItems.update
                                                    (itemId glossaryItemForHtml result1_)
                                                    glossaryItemForHtml
                                                    result1_
                                            )
                                            result1
                                    )
                                    result
                       )
                    |> Result.map (GlossaryItems.orderedAlphabetically Nothing)
                    |> Expect.equal
                        (glossaryItems
                            |> GlossaryItems.orderedAlphabetically Nothing
                            |> Ok
                        )
        , test "gets items by ID" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.get (GlossaryItemId.create 0)
                    |> Expect.equal (Just defaultComputerScienceItem)
        , test "gets all tags" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.tags
                    |> Expect.equal [ computerScienceTag, financeTag, gardeningTag ]
        , test "gets all tags along with their descriptions" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.tagsWithDescriptions
                    |> Expect.equal
                        [ ( computerScienceTag, computerScienceTagDescription )
                        , ( financeTag, financeTagDescription )
                        , ( gardeningTag, gardeningTagDescription )
                        ]
        , test "gets all tags along with their tag IDs" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.tagByIdList
                    |> Expect.equal
                        [ ( TagId.create 0, computerScienceTag )
                        , ( TagId.create 1, financeTag )
                        , ( TagId.create 2, gardeningTag )
                        ]
        , test "looks up a tag ID from its contents" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.tagIdFromTag computerScienceTag
                    |> Expect.equal (Just <| TagId.create 0)
        , test "looks up a tag from its ID" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.tagFromId (TagId.create 1)
                    |> Expect.equal (Just financeTag)
        , test "looks up a tag description from its ID" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.tagDescriptionFromId (TagId.create 2)
                    |> Expect.equal (Just gardeningTagDescription)
        , test "returns disambiguated preferred term for item with given ID" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.disambiguatedPreferredTerm (GlossaryItemId.create 0)
                    |> Expect.equal (Just <| DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Default (Computer Science)" False)
        , test "returns all disambiguated preferred terms" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.disambiguatedPreferredTerms Nothing
                    |> Expect.equal
                        [ DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Default (Computer Science)" False
                        , DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Default (Finance)" False
                        , DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Information retrieval" False
                        , DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Interest rate" False
                        , DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Loan" False
                        ]
        , test "returns all disambiguated preferred terms with tag filter applied" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.disambiguatedPreferredTerms (Just <| TagId.create 1)
                    |> Expect.equal
                        [ DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Default (Finance)" False
                        , DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Interest rate" False
                        , DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Loan" False
                        ]
        , test "looks up the ID of the item whose disambiguated preferred term has the given ID" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.itemIdFromDisambiguatedPreferredTermId (TermId.fromString "Default_(Finance)")
                    |> Expect.equal (Just <| GlossaryItemId.create 1)
        , test "looks up the disambiguated preferred term of the item whose disambiguated preferred term has the given ID" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.disambiguatedPreferredTermFromId (TermId.fromString "Default_(Finance)")
                    |> Expect.equal (Just <| DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Default (Finance)" False)
        , test "returns all of the disambiguated preferred terms which have a definition" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.disambiguatedPreferredTermsWhichHaveDefinitions Nothing
                    |> Expect.equal
                        [ DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Default (Computer Science)" False
                        , DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Default (Finance)" False
                        , DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Information retrieval" False
                        , DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Interest rate" False
                        , DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Loan" False
                        ]
        , test "returns all of the disambiguated preferred terms which have a definition with tag filter applied" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.disambiguatedPreferredTermsWhichHaveDefinitions (Just <| TagId.create 1)
                    |> Expect.equal
                        [ DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Default (Finance)" False
                        , DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Interest rate" False
                        , DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Loan" False
                        ]
        , test "returns the IDs of the items that list this item as a related one" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.relatedForWhichItems (GlossaryItemId.create 4)
                    |> Expect.equal
                        [ GlossaryItemId.create 3
                        , GlossaryItemId.create 1
                        ]
        , test "looks up ID of item whose preferred term has given ID" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.itemIdFromDisambiguatedPreferredTermId (TermId.fromString "Default_(Finance)")
                    |> Expect.equal (Just <| GlossaryItemId.create 1)
        , test "looks up disambiguated preferred term of item whose disambiguated preferred term has given ID" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.disambiguatedPreferredTermFromId (TermId.fromString "Default_(Finance)")
                    |> Expect.equal (Just <| DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Default (Finance)" False)
        , test "returns a list of pairs associating each alternative term with the disambiguated preferred terms that it appears together with" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.disambiguatedPreferredTermsByAlternativeTerm Nothing
                    |> Expect.equal
                        [ ( Term.fromMarkdown "Preset" False
                          , [ DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Default (Computer Science)" False ]
                          )
                        , ( Term.fromMarkdown "IR" True
                          , [ DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Interest rate" False
                            , DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Information retrieval" False
                            ]
                          )
                        , ( Term.fromMarkdown "Factory preset" False
                          , [ DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Default (Computer Science)" False ]
                          )
                        ]
        , test "returns items in alphabetical order" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.orderedAlphabetically Nothing
                    |> Expect.equal
                        [ ( GlossaryItemId.create 0, defaultComputerScienceItem )
                        , ( GlossaryItemId.create 1, defaultFinanceItem )
                        , ( GlossaryItemId.create 2, informationRetrievalItem )
                        , ( GlossaryItemId.create 3, interestRateItem )
                        , ( GlossaryItemId.create 4, loanItem )
                        ]
        , test "returns items in alphabetical order with tag filter applied" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.orderedAlphabetically (Just <| TagId.create 0)
                    |> List.map Tuple.first
                    |> Expect.equal
                        [ GlossaryItemId.create 0
                        , GlossaryItemId.create 2
                        ]
        , test "when tag filter is applied, removes non-matching related items" <|
            \_ ->
                let
                    defaultComputerScienceItem_ : GlossaryItemForHtml
                    defaultComputerScienceItem_ =
                        GlossaryItemForHtml.create
                            (Term.fromMarkdown "Default" False)
                            []
                            (Just computerScienceTag)
                            []
                            (Just defaultComputerScienceDefinition)
                            [ DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Default (Finance)" False ]
                            False
                            (Just "2023-09-15T19:58:59.573Z")
                            Nothing
                            Nothing

                    defaultFinanceItem_ : GlossaryItemForHtml
                    defaultFinanceItem_ =
                        GlossaryItemForHtml.create
                            (Term.fromMarkdown "Default" False)
                            []
                            (Just financeTag)
                            []
                            (Just defaultFinanceDefinition)
                            []
                            False
                            (Just "2023-10-30T08:25:24.765Z")
                            Nothing
                            Nothing

                    glossaryItems_ : Result String GlossaryItems
                    glossaryItems_ =
                        GlossaryItems.fromList
                            [ ( computerScienceTag, computerScienceTagDescription )
                            , ( financeTag, financeTagDescription )
                            ]
                            [ defaultComputerScienceItem_
                            , defaultFinanceItem_
                            ]
                in
                glossaryItems_
                    |> Result.map (GlossaryItems.orderedAlphabetically (Just <| TagId.create 0))
                    |> Expect.equal
                        (Ok
                            [ ( GlossaryItemId.create 0
                              , GlossaryItemForHtml.create
                                    (Term.fromMarkdown "Default" False)
                                    []
                                    (Just computerScienceTag)
                                    []
                                    (Just defaultComputerScienceDefinition)
                                    []
                                    False
                                    (Just "2023-09-15T19:58:59.573Z")
                                    Nothing
                                    Nothing
                              )
                            ]
                        )
        , test "returns items ordered by most mentioned first" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.orderedByMostMentionedFirst Nothing
                    |> List.map Tuple.first
                    |> Expect.equal
                        [ GlossaryItemId.create 3
                        , GlossaryItemId.create 4
                        , GlossaryItemId.create 2
                        , GlossaryItemId.create 0
                        , GlossaryItemId.create 1
                        ]
        , test "returns items ordered by most mentioned first with tag filter applied" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.orderedByMostMentionedFirst (Just <| TagId.create 1)
                    |> List.map Tuple.first
                    |> Expect.equal
                        [ GlossaryItemId.create 3
                        , GlossaryItemId.create 4
                        , GlossaryItemId.create 1
                        ]
        , test "returns items ordered 'focused on' a specific item" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.orderedFocusedOn Nothing (GlossaryItemId.create 1)
                    |> (\( lhs, rhs ) -> ( List.map Tuple.first lhs, List.map Tuple.first rhs ))
                    |> Expect.equal
                        ( [ GlossaryItemId.create 1
                          , GlossaryItemId.create 4
                          , GlossaryItemId.create 3
                          ]
                        , [ GlossaryItemId.create 0
                          , GlossaryItemId.create 2
                          ]
                        )
        , test "returns items ordered 'focused on' a specific item with tag filter applied" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.orderedFocusedOn (Just <| TagId.create 1) (GlossaryItemId.create 1)
                    |> (\( lhs, rhs ) -> ( List.map Tuple.first lhs, List.map Tuple.first rhs ))
                    |> Expect.equal
                        ( [ GlossaryItemId.create 1
                          , GlossaryItemId.create 4
                          , GlossaryItemId.create 3
                          ]
                        , []
                        )
        , test "sorts tags in items alphabetically" <|
            \_ ->
                GlossaryItems.fromList
                    [ ( financeTag, financeTagDescription )
                    , ( houseworkTag, houseworkTagDescription )
                    , ( gardeningTag, gardeningTagDescription )
                    , ( computerScienceTag, computerScienceTagDescription )
                    ]
                    [ GlossaryItemForHtml.create
                        (Term.fromMarkdown "Foo" False)
                        []
                        (Just gardeningTag)
                        [ financeTag, houseworkTag, computerScienceTag ]
                        Nothing
                        []
                        False
                        (Just "2023-10-30T08:25:30.335Z")
                        Nothing
                        Nothing
                    ]
                    |> Result.map (GlossaryItems.get <| GlossaryItemId.create 0)
                    |> Expect.equal
                        (Ok <|
                            Just <|
                                GlossaryItemForHtml.create
                                    (Term.fromMarkdown "Foo" False)
                                    []
                                    (Just gardeningTag)
                                    [ computerScienceTag, financeTag, houseworkTag ]
                                    Nothing
                                    []
                                    False
                                    (Just "2023-10-30T08:25:30.335Z")
                                    Nothing
                                    Nothing
                        )
        , test "sorts tags alphabetically" <|
            \_ ->
                GlossaryItems.fromList
                    [ ( financeTag, financeTagDescription )
                    , ( houseworkTag, houseworkTagDescription )
                    , ( gardeningTag, gardeningTagDescription )
                    , ( computerScienceTag, computerScienceTagDescription )
                    ]
                    []
                    |> Result.map GlossaryItems.tags
                    |> Expect.equal (Ok [ computerScienceTag, financeTag, gardeningTag, houseworkTag ])
        , test "sorts tags with descriptions alphabetically" <|
            \_ ->
                GlossaryItems.fromList
                    [ ( financeTag, financeTagDescription )
                    , ( houseworkTag, houseworkTagDescription )
                    , ( gardeningTag, gardeningTagDescription )
                    , ( computerScienceTag, computerScienceTagDescription )
                    ]
                    []
                    |> Result.map GlossaryItems.tagsWithDescriptions
                    |> Expect.equal
                        (Ok
                            [ ( computerScienceTag, computerScienceTagDescription )
                            , ( financeTag, financeTagDescription )
                            , ( gardeningTag, gardeningTagDescription )
                            , ( houseworkTag, houseworkTagDescription )
                            ]
                        )
        , test "returns error for duplicate tags" <|
            \_ ->
                GlossaryItems.fromList
                    [ ( financeTag, financeTagDescription )
                    , ( financeTag, financeTagDescription )
                    ]
                    []
                    |> Expect.equal
                        (Err "tag \"Finance\" appears multiple times")
        , test "returns error for duplicate disambiguated preferred terms" <|
            \_ ->
                GlossaryItems.fromList
                    [ ( financeTag, financeTagDescription ) ]
                    [ GlossaryItemForHtml.create
                        (Term.fromMarkdown "Foo" False)
                        []
                        (Just financeTag)
                        []
                        Nothing
                        []
                        False
                        (Just "2023-10-30T08:25:30.335Z")
                        Nothing
                        Nothing
                    , GlossaryItemForHtml.create
                        (Term.fromMarkdown "Foo (Finance)" False)
                        []
                        Nothing
                        []
                        Nothing
                        []
                        False
                        (Just "2023-10-30T08:25:30.335Z")
                        Nothing
                        Nothing
                    ]
                    |> Expect.equal
                        (Err "there are multiple items with (disambiguated) preferred term \"Foo (Finance)\"")
        ]
