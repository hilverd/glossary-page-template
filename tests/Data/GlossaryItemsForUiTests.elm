module Data.GlossaryItemsForUiTests exposing (suite)

import Data.DescribedTag as DescribedTag
import Data.GlossaryItem.Definition as Definition
import Data.GlossaryItem.DisambiguatedTerm as DisambiguatedTerm
import Data.GlossaryItem.RawTerm as RawTerm
import Data.GlossaryItem.Term as Term
import Data.GlossaryItemForUi as GlossaryItemForUi exposing (GlossaryItemForUi)
import Data.GlossaryItemId as GlossaryItemId
import Data.GlossaryItemsForUi as GlossaryItemsForUi exposing (GlossaryItemsForUi)
import Data.TagId as TagId
import Expect
import Test exposing (Test, describe, test)
import TestData
    exposing
        ( computerScienceDescribedTag
        , computerScienceTag
        , computerScienceTagId
        , defaultComputerScienceDefinition
        , defaultComputerScienceItem
        , defaultFinanceDefinition
        , defaultFinanceItem
        , financeDescribedTag
        , financeTag
        , financeTagDescription
        , financeTagId
        , gardeningDescribedTag
        , gardeningTag
        , gardeningTagDescription
        , gardeningTagId
        , glossaryItemsForUi
        , houseworkTag
        , houseworkTagDescription
        , informationRetrievalItem
        , interestRateItem
        , loanItem
        )


houseworkTagId : TagId.TagId
houseworkTagId =
    TagId.create "housework"


houseworkDescribedTag : DescribedTag.DescribedTag
houseworkDescribedTag =
    DescribedTag.create houseworkTagId houseworkTag houseworkTagDescription


suite : Test
suite =
    describe "The Data.GlossaryItemsForUi module"
        [ test "gets items by ID" <|
            \_ ->
                glossaryItemsForUi
                    |> GlossaryItemsForUi.get (GlossaryItemForUi.id defaultComputerScienceItem)
                    |> Expect.equal (Just defaultComputerScienceItem)
        , test "gets all tags" <|
            \_ ->
                glossaryItemsForUi
                    |> GlossaryItemsForUi.tags
                    |> Expect.equal [ computerScienceTag, financeTag, gardeningTag ]
        , test "gets all tags along with their descriptions" <|
            \_ ->
                glossaryItemsForUi
                    |> GlossaryItemsForUi.describedTags
                    |> Expect.equal
                        [ computerScienceDescribedTag
                        , financeDescribedTag
                        , gardeningDescribedTag
                        ]
        , test "gets all tags along with their tag IDs" <|
            \_ ->
                glossaryItemsForUi
                    |> GlossaryItemsForUi.tagByIdList
                    |> Expect.equal
                        [ ( computerScienceTagId, computerScienceTag )
                        , ( financeTagId, financeTag )
                        , ( gardeningTagId, gardeningTag )
                        ]
        , test "looks up a tag ID from its contents" <|
            \_ ->
                glossaryItemsForUi
                    |> GlossaryItemsForUi.tagIdFromTag computerScienceTag
                    |> Expect.equal (Just computerScienceTagId)
        , test "looks up a tag from its ID" <|
            \_ ->
                glossaryItemsForUi
                    |> GlossaryItemsForUi.tagFromId financeTagId
                    |> Expect.equal (Just financeTag)
        , test "looks up a tag description from its ID" <|
            \_ ->
                glossaryItemsForUi
                    |> GlossaryItemsForUi.tagDescriptionFromId gardeningTagId
                    |> Expect.equal (Just gardeningTagDescription)
        , test "returns disambiguated preferred term for item with given ID" <|
            \_ ->
                glossaryItemsForUi
                    |> GlossaryItemsForUi.disambiguatedPreferredTerm (GlossaryItemForUi.id defaultComputerScienceItem)
                    |> Expect.equal (Just <| DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Default (Computer Science)" False)
        , test "returns all disambiguated preferred terms" <|
            \_ ->
                glossaryItemsForUi
                    |> GlossaryItemsForUi.disambiguatedPreferredTerms Nothing
                    |> List.map Tuple.second
                    |> Expect.equal
                        [ DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Default (Computer Science)" False
                        , DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Default (Finance)" False
                        , DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Information retrieval" False
                        , DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Interest rate" False
                        , DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Loan" False
                        ]
        , test "returns all disambiguated preferred terms with tag filter applied" <|
            \_ ->
                glossaryItemsForUi
                    |> GlossaryItemsForUi.disambiguatedPreferredTerms (Just financeTagId)
                    |> List.map Tuple.second
                    |> Expect.equal
                        [ DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Default (Finance)" False
                        , DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Interest rate" False
                        , DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Loan" False
                        ]
        , test "returns the outlines for all the items in the glossary" <|
            \_ ->
                glossaryItemsForUi
                    |> GlossaryItemsForUi.itemOutlines Nothing
                    |> Expect.equal
                        [ { allTags = [ "Computer Science" ]
                          , alternativeTerms = [ "Preset", "Factory preset" ]
                          , disambiguatedPreferredTerm = "Default (Computer Science)"
                          , preferredTerm = "Default"
                          }
                        , { allTags = [ "Finance" ]
                          , alternativeTerms = []
                          , disambiguatedPreferredTerm = "Default (Finance)"
                          , preferredTerm = "Default"
                          }
                        , { allTags = [ "Computer Science" ]
                          , alternativeTerms = [ "IR" ]
                          , disambiguatedPreferredTerm = "Information retrieval"
                          , preferredTerm = "Information retrieval"
                          }
                        , { allTags = [ "Finance" ]
                          , alternativeTerms = [ "IR" ]
                          , disambiguatedPreferredTerm = "Interest rate"
                          , preferredTerm = "Interest rate"
                          }
                        , { allTags = [ "Finance" ]
                          , alternativeTerms = []
                          , disambiguatedPreferredTerm = "Loan"
                          , preferredTerm = "Loan"
                          }
                        ]
        , test "returns the outlines for all the items in the glossary with tag filter applied" <|
            \_ ->
                glossaryItemsForUi
                    |> GlossaryItemsForUi.itemOutlines (Just financeTagId)
                    |> Expect.equal
                        [ { allTags = [ "Finance" ]
                          , alternativeTerms = []
                          , disambiguatedPreferredTerm = "Default (Finance)"
                          , preferredTerm = "Default"
                          }
                        , { allTags = [ "Finance" ]
                          , alternativeTerms = [ "IR" ]
                          , disambiguatedPreferredTerm = "Interest rate"
                          , preferredTerm = "Interest rate"
                          }
                        , { allTags = [ "Finance" ]
                          , alternativeTerms = []
                          , disambiguatedPreferredTerm = "Loan"
                          , preferredTerm = "Loan"
                          }
                        ]
        , test "looks up the ID of the item whose disambiguated preferred term has the given ID" <|
            \_ ->
                glossaryItemsForUi
                    |> GlossaryItemsForUi.itemIdFromRawDisambiguatedPreferredTerm (RawTerm.fromString "Default (Finance)")
                    |> Expect.equal (Just <| GlossaryItemForUi.id defaultFinanceItem)
        , test "looks up the ID of the item with the given fragment identifier" <|
            \_ ->
                glossaryItemsForUi
                    |> GlossaryItemsForUi.itemIdFromFragmentIdentifier "Default_(Finance)"
                    |> Expect.equal (Just <| GlossaryItemForUi.id defaultFinanceItem)
        , test "looks up the disambiguated preferred term of the item with the given raw disambiguated preferred term" <|
            \_ ->
                glossaryItemsForUi
                    |> GlossaryItemsForUi.disambiguatedPreferredTermFromRaw (RawTerm.fromString "Default (Finance)")
                    |> Expect.equal (Just <| DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Default (Finance)" False)
        , test "returns all of the disambiguated preferred terms which have a definition" <|
            \_ ->
                glossaryItemsForUi
                    |> GlossaryItemsForUi.disambiguatedPreferredTermsWhichHaveDefinitions Nothing
                    |> Expect.equal
                        [ DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Default (Computer Science)" False
                        , DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Default (Finance)" False
                        , DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Information retrieval" False
                        , DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Interest rate" False
                        , DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Loan" False
                        ]
        , test "returns all of the disambiguated preferred terms which have a definition with tag filter applied" <|
            \_ ->
                glossaryItemsForUi
                    |> GlossaryItemsForUi.disambiguatedPreferredTermsWhichHaveDefinitions (Just financeTagId)
                    |> Expect.equal
                        [ DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Default (Finance)" False
                        , DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Interest rate" False
                        , DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Loan" False
                        ]
        , test "returns the IDs of the items that list this item as a related one" <|
            \_ ->
                glossaryItemsForUi
                    |> GlossaryItemsForUi.relatedForWhichItems (GlossaryItemForUi.id loanItem)
                    |> Expect.equal
                        [ GlossaryItemForUi.id interestRateItem
                        , GlossaryItemForUi.id defaultFinanceItem
                        ]
        , test "looks up disambiguated preferred term of item whose disambiguated preferred term has given ID" <|
            \_ ->
                glossaryItemsForUi
                    |> GlossaryItemsForUi.disambiguatedPreferredTermFromRaw (RawTerm.fromString "Default (Finance)")
                    |> Expect.equal (Just <| DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Default (Finance)" False)
        , test "returns a list of pairs associating each alternative term with the disambiguated preferred terms that it appears together with" <|
            \_ ->
                glossaryItemsForUi
                    |> GlossaryItemsForUi.disambiguatedPreferredTermsByAlternativeTerm Nothing
                    |> Expect.equal
                        [ ( Term.fromMarkdown "Preset" False
                          , [ ( GlossaryItemForUi.id defaultComputerScienceItem, DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Default (Computer Science)" False ) ]
                          )
                        , ( Term.fromMarkdown "IR" True
                          , [ ( GlossaryItemForUi.id interestRateItem, DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Interest rate" False )
                            , ( GlossaryItemForUi.id informationRetrievalItem, DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Information retrieval" False )
                            ]
                          )
                        , ( Term.fromMarkdown "Factory preset" False
                          , [ ( GlossaryItemForUi.id defaultComputerScienceItem, DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Default (Computer Science)" False ) ]
                          )
                        ]
        , test "returns items in alphabetical order" <|
            \_ ->
                glossaryItemsForUi
                    |> GlossaryItemsForUi.orderedAlphabetically Nothing
                    |> Expect.equal
                        [ ( GlossaryItemForUi.id defaultComputerScienceItem, defaultComputerScienceItem )
                        , ( GlossaryItemForUi.id defaultFinanceItem, defaultFinanceItem )
                        , ( GlossaryItemForUi.id informationRetrievalItem, informationRetrievalItem )
                        , ( GlossaryItemForUi.id interestRateItem, interestRateItem )
                        , ( GlossaryItemForUi.id loanItem, loanItem )
                        ]
        , test "returns items in alphabetical order with tag filter applied" <|
            \_ ->
                glossaryItemsForUi
                    |> GlossaryItemsForUi.orderedAlphabetically (Just computerScienceTagId)
                    |> List.map Tuple.first
                    |> Expect.equal
                        [ GlossaryItemForUi.id defaultComputerScienceItem
                        , GlossaryItemForUi.id informationRetrievalItem
                        ]
        , test "when tag filter is applied, removes non-matching related items" <|
            \_ ->
                let
                    defaultComputerScienceItem_ : GlossaryItemForUi
                    defaultComputerScienceItem_ =
                        GlossaryItemForUi.create
                            (GlossaryItemId.create "Default (Computer Science)")
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

                    defaultFinanceItem_ : GlossaryItemForUi
                    defaultFinanceItem_ =
                        GlossaryItemForUi.create
                            (GlossaryItemId.create "Default (Finance)")
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

                    glossaryItems_ : Result String GlossaryItemsForUi
                    glossaryItems_ =
                        GlossaryItemsForUi.fromList
                            [ computerScienceDescribedTag
                            , financeDescribedTag
                            ]
                            Nothing
                            [ defaultComputerScienceItem_
                            , defaultFinanceItem_
                            ]
                in
                glossaryItems_
                    |> Result.map (GlossaryItemsForUi.orderedAlphabetically (Just computerScienceTagId))
                    |> Expect.equal
                        (Ok
                            [ ( GlossaryItemForUi.id defaultComputerScienceItem
                              , GlossaryItemForUi.create
                                    (GlossaryItemId.create "Default (Computer Science)")
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
                glossaryItemsForUi
                    |> GlossaryItemsForUi.orderedByMostMentionedFirst Nothing
                    |> List.map Tuple.first
                    |> Expect.equal
                        [ GlossaryItemForUi.id interestRateItem
                        , GlossaryItemForUi.id loanItem
                        , GlossaryItemForUi.id informationRetrievalItem
                        , GlossaryItemForUi.id defaultComputerScienceItem
                        , GlossaryItemForUi.id defaultFinanceItem
                        ]
        , test "returns items ordered by most mentioned first with tag filter applied" <|
            \_ ->
                glossaryItemsForUi
                    |> GlossaryItemsForUi.orderedByMostMentionedFirst (Just financeTagId)
                    |> List.map Tuple.first
                    |> Expect.equal
                        [ GlossaryItemForUi.id interestRateItem
                        , GlossaryItemForUi.id loanItem
                        , GlossaryItemForUi.id defaultFinanceItem
                        ]
        , test "returns items ordered 'focused on' a specific item" <|
            \_ ->
                glossaryItemsForUi
                    |> GlossaryItemsForUi.orderedFocusedOn Nothing (GlossaryItemForUi.id defaultFinanceItem)
                    |> (\( lhs, rhs ) -> ( List.map Tuple.first lhs, List.map Tuple.first rhs ))
                    |> Expect.equal
                        ( [ GlossaryItemForUi.id defaultFinanceItem
                          , GlossaryItemForUi.id loanItem
                          , GlossaryItemForUi.id interestRateItem
                          ]
                        , [ GlossaryItemForUi.id defaultComputerScienceItem
                          , GlossaryItemForUi.id informationRetrievalItem
                          ]
                        )
        , test "returns items ordered 'focused on' a specific item with tag filter applied" <|
            \_ ->
                glossaryItemsForUi
                    |> GlossaryItemsForUi.orderedFocusedOn (Just financeTagId) (GlossaryItemForUi.id defaultFinanceItem)
                    |> (\( lhs, rhs ) -> ( List.map Tuple.first lhs, List.map Tuple.first rhs ))
                    |> Expect.equal
                        ( [ GlossaryItemForUi.id defaultFinanceItem
                          , GlossaryItemForUi.id loanItem
                          , GlossaryItemForUi.id interestRateItem
                          ]
                        , []
                        )
        , test "sorts tags in items alphabetically" <|
            \_ ->
                GlossaryItemsForUi.fromList
                    [ financeDescribedTag
                    , houseworkDescribedTag
                    , gardeningDescribedTag
                    , computerScienceDescribedTag
                    ]
                    Nothing
                    [ GlossaryItemForUi.create
                        (GlossaryItemForUi.id defaultComputerScienceItem)
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
                    |> Result.map (GlossaryItemsForUi.get <| GlossaryItemForUi.id defaultComputerScienceItem)
                    |> Expect.equal
                        (Ok <|
                            Just <|
                                GlossaryItemForUi.create
                                    (GlossaryItemForUi.id defaultComputerScienceItem)
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
                GlossaryItemsForUi.fromList
                    [ financeDescribedTag
                    , houseworkDescribedTag
                    , gardeningDescribedTag
                    , computerScienceDescribedTag
                    ]
                    Nothing
                    []
                    |> Result.map GlossaryItemsForUi.tags
                    |> Expect.equal (Ok [ computerScienceTag, financeTag, gardeningTag, houseworkTag ])
        , test "sorts tags with descriptions alphabetically" <|
            \_ ->
                GlossaryItemsForUi.fromList
                    [ financeDescribedTag
                    , houseworkDescribedTag
                    , gardeningDescribedTag
                    , computerScienceDescribedTag
                    ]
                    Nothing
                    []
                    |> Result.map GlossaryItemsForUi.describedTags
                    |> Expect.equal
                        (Ok
                            [ computerScienceDescribedTag
                            , financeDescribedTag
                            , gardeningDescribedTag
                            , houseworkDescribedTag
                            ]
                        )
        , test "returns error for duplicate tags" <|
            \_ ->
                GlossaryItemsForUi.fromList
                    [ financeDescribedTag
                    , DescribedTag.create
                        (TagId.create "some-other-tag-id")
                        financeTag
                        financeTagDescription
                    ]
                    Nothing
                    []
                    |> Expect.equal
                        (Err "tag \"Finance\" appears multiple times")
        , test "returns error for duplicate disambiguated preferred terms" <|
            \_ ->
                -- TODO: is this test essentially the same as the one below?
                GlossaryItemsForUi.fromList
                    [ financeDescribedTag ]
                    Nothing
                    [ GlossaryItemForUi.create
                        (GlossaryItemId.create "Foo (Finance) 1")
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
                    , GlossaryItemForUi.create
                        (GlossaryItemId.create "Foo (Finance) 2")
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
                        (Err "There are multiple items with (disambiguated) preferred term identifier \"Foo_(Finance)\"")
        , test "returns error for multiple disambiguated preferred terms with the same fragment identifier" <|
            \_ ->
                GlossaryItemsForUi.fromList
                    [ financeDescribedTag ]
                    Nothing
                    [ GlossaryItemForUi.create
                        (GlossaryItemId.create "Foo (Finance)")
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
                    , GlossaryItemForUi.create
                        (GlossaryItemId.create "Foo_(Finance)")
                        (Term.fromMarkdown "Foo_(Finance)" False)
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
                        (Err "There are multiple items with (disambiguated) preferred term identifier \"Foo_(Finance)\"")
        , test "when starting item has no related items, falls back to items whose disambiguated preferred term matches a tag they have" <|
            \_ ->
                let
                    -- Create a starting item with no related items
                    startingItem_ : GlossaryItemForUi
                    startingItem_ =
                        GlossaryItemForUi.create
                            (GlossaryItemId.create "Start")
                            (Term.fromMarkdown "Start" False)
                            []
                            Nothing
                            []
                            (Just <| Definition.fromMarkdown "Starting item")
                            []
                            False
                            (Just "2023-10-30T08:25:30.335Z")
                            Nothing
                            Nothing

                    -- Create items whose disambiguated preferred terms match tags
                    -- Finance has 3 items (defaultFinance, interestRate, loan)
                    financeItem_ : GlossaryItemForUi
                    financeItem_ =
                        GlossaryItemForUi.create
                            (GlossaryItemId.create "Finance")
                            (Term.fromMarkdown "Finance" False)
                            []
                            Nothing
                            [ financeTag ]
                            (Just <| Definition.fromMarkdown "About finance")
                            []
                            False
                            (Just "2023-10-30T08:25:30.335Z")
                            Nothing
                            Nothing

                    -- Computer Science has 2 items (defaultComputerScience, informationRetrieval)
                    computerScienceItem_ : GlossaryItemForUi
                    computerScienceItem_ =
                        GlossaryItemForUi.create
                            (GlossaryItemId.create "Computer Science")
                            (Term.fromMarkdown "Computer Science" False)
                            []
                            Nothing
                            [ computerScienceTag ]
                            (Just <| Definition.fromMarkdown "About computer science")
                            []
                            False
                            (Just "2023-10-30T08:25:30.335Z")
                            Nothing
                            Nothing

                    -- Gardening has 1 item (only the Gardening item itself)
                    gardeningItem_ : GlossaryItemForUi
                    gardeningItem_ =
                        GlossaryItemForUi.create
                            (GlossaryItemId.create "Gardening")
                            (Term.fromMarkdown "Gardening" False)
                            []
                            Nothing
                            [ gardeningTag ]
                            (Just <| Definition.fromMarkdown "About gardening")
                            []
                            False
                            (Just "2023-10-30T08:25:30.335Z")
                            Nothing
                            Nothing

                    glossaryItems_ : Result String GlossaryItemsForUi
                    glossaryItems_ =
                        GlossaryItemsForUi.fromList
                            [ computerScienceDescribedTag
                            , financeDescribedTag
                            , gardeningDescribedTag
                            ]
                            (Just <| DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Start" False)
                            [ startingItem_
                            , defaultComputerScienceItem
                            , defaultFinanceItem
                            , informationRetrievalItem
                            , interestRateItem
                            , loanItem
                            , financeItem_
                            , computerScienceItem_
                            , gardeningItem_
                            ]
                in
                glossaryItems_
                    |> Result.map
                        (\items ->
                            GlossaryItemsForUi.relatedItems
                                (GlossaryItemId.create "Start")
                                Nothing
                                items
                                |> List.map GlossaryItemForUi.id
                        )
                    |> Expect.equal
                        (Ok
                            [ GlossaryItemId.create "Finance"
                            , GlossaryItemId.create "Computer Science"
                            , GlossaryItemId.create "Gardening"
                            ]
                        )
        , test "when starting item has no related items, respects tag filter" <|
            \_ ->
                let
                    startingItem_ : GlossaryItemForUi
                    startingItem_ =
                        GlossaryItemForUi.create
                            (GlossaryItemId.create "Start")
                            (Term.fromMarkdown "Start" False)
                            []
                            Nothing
                            []
                            (Just <| Definition.fromMarkdown "Starting item")
                            []
                            False
                            (Just "2023-10-30T08:25:30.335Z")
                            Nothing
                            Nothing

                    financeItem_ : GlossaryItemForUi
                    financeItem_ =
                        GlossaryItemForUi.create
                            (GlossaryItemId.create "Finance")
                            (Term.fromMarkdown "Finance" False)
                            []
                            Nothing
                            [ financeTag ]
                            (Just <| Definition.fromMarkdown "About finance")
                            []
                            False
                            (Just "2023-10-30T08:25:30.335Z")
                            Nothing
                            Nothing

                    computerScienceItem_ : GlossaryItemForUi
                    computerScienceItem_ =
                        GlossaryItemForUi.create
                            (GlossaryItemId.create "Computer Science")
                            (Term.fromMarkdown "Computer Science" False)
                            []
                            Nothing
                            [ computerScienceTag ]
                            (Just <| Definition.fromMarkdown "About computer science")
                            []
                            False
                            (Just "2023-10-30T08:25:30.335Z")
                            Nothing
                            Nothing

                    glossaryItems_ : Result String GlossaryItemsForUi
                    glossaryItems_ =
                        GlossaryItemsForUi.fromList
                            [ computerScienceDescribedTag
                            , financeDescribedTag
                            ]
                            (Just <| DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Start" False)
                            [ startingItem_
                            , defaultComputerScienceItem
                            , defaultFinanceItem
                            , informationRetrievalItem
                            , interestRateItem
                            , loanItem
                            , financeItem_
                            , computerScienceItem_
                            ]
                in
                glossaryItems_
                    |> Result.map
                        (\items ->
                            GlossaryItemsForUi.relatedItems
                                (GlossaryItemId.create "Start")
                                (Just financeTagId)
                                items
                                |> List.map GlossaryItemForUi.id
                        )
                    |> Expect.equal
                        (Ok
                            [ GlossaryItemId.create "Finance"
                            ]
                        )
        , test "when starting item has related items, uses them instead of fallback" <|
            \_ ->
                let
                    startingItem_ : GlossaryItemForUi
                    startingItem_ =
                        GlossaryItemForUi.create
                            (GlossaryItemId.create "Start")
                            (Term.fromMarkdown "Start" False)
                            []
                            Nothing
                            []
                            (Just <| Definition.fromMarkdown "Starting item")
                            [ DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Loan" False ]
                            False
                            (Just "2023-10-30T08:25:30.335Z")
                            Nothing
                            Nothing

                    financeItem_ : GlossaryItemForUi
                    financeItem_ =
                        GlossaryItemForUi.create
                            (GlossaryItemId.create "Finance")
                            (Term.fromMarkdown "Finance" False)
                            []
                            Nothing
                            [ financeTag ]
                            (Just <| Definition.fromMarkdown "About finance")
                            []
                            False
                            (Just "2023-10-30T08:25:30.335Z")
                            Nothing
                            Nothing

                    glossaryItems_ : Result String GlossaryItemsForUi
                    glossaryItems_ =
                        GlossaryItemsForUi.fromList
                            [ financeDescribedTag
                            ]
                            (Just <| DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Start" False)
                            [ startingItem_
                            , loanItem
                            , financeItem_
                            ]
                in
                glossaryItems_
                    |> Result.map
                        (\items ->
                            GlossaryItemsForUi.relatedItems
                                (GlossaryItemId.create "Start")
                                Nothing
                                items
                                |> List.map GlossaryItemForUi.id
                        )
                    |> Expect.equal
                        (Ok
                            [ GlossaryItemId.create "Loan"
                            ]
                        )
        , test "when non-starting item has no related items, returns empty list" <|
            \_ ->
                let
                    itemWithNoRelated : GlossaryItemForUi
                    itemWithNoRelated =
                        GlossaryItemForUi.create
                            (GlossaryItemId.create "NoRelated")
                            (Term.fromMarkdown "NoRelated" False)
                            []
                            Nothing
                            []
                            (Just <| Definition.fromMarkdown "An item with no related items")
                            []
                            False
                            (Just "2023-10-30T08:25:30.335Z")
                            Nothing
                            Nothing

                    glossaryItems_ : Result String GlossaryItemsForUi
                    glossaryItems_ =
                        GlossaryItemsForUi.fromList
                            []
                            Nothing
                            [ itemWithNoRelated
                            ]
                in
                glossaryItems_
                    |> Result.map
                        (\items ->
                            GlossaryItemsForUi.relatedItems
                                (GlossaryItemId.create "NoRelated")
                                Nothing
                                items
                                |> List.map GlossaryItemForUi.id
                        )
                    |> Expect.equal (Ok [])
        , describe "itemWhoseDisambiguatedPreferredTermMatchesTag"
            [ test "finds item whose disambiguated preferred term matches tag (normal tag)" <|
                \_ ->
                    let
                        financeItem_ : GlossaryItemForUi
                        financeItem_ =
                            GlossaryItemForUi.create
                                (GlossaryItemId.create "Finance")
                                (Term.fromMarkdown "Finance" False)
                                []
                                Nothing
                                [ financeTag ]
                                (Just <| Definition.fromMarkdown "About finance")
                                []
                                False
                                (Just "2023-10-30T08:25:30.335Z")
                                Nothing
                                Nothing

                        glossaryItems_ : Result String GlossaryItemsForUi
                        glossaryItems_ =
                            GlossaryItemsForUi.fromList
                                [ financeDescribedTag ]
                                Nothing
                                [ financeItem_ ]
                    in
                    glossaryItems_
                        |> Result.toMaybe
                        |> Maybe.andThen
                            (\items ->
                                GlossaryItemsForUi.itemWhoseDisambiguatedPreferredTermMatchesTag financeTagId items
                            )
                        |> Maybe.map GlossaryItemForUi.id
                        |> Expect.equal (Just <| GlossaryItemId.create "Finance")
            , test "finds item whose disambiguated preferred term matches tag (disambiguation tag)" <|
                \_ ->
                    let
                        -- When an item has "Finance" as both its preferred term and disambiguation tag,
                        -- the disambiguated preferred term becomes "Finance (Finance)"
                        financeItem_ : GlossaryItemForUi
                        financeItem_ =
                            GlossaryItemForUi.create
                                (GlossaryItemId.create "Finance")
                                (Term.fromMarkdown "Finance" False)
                                []
                                (Just financeTag)
                                []
                                (Just <| Definition.fromMarkdown "About finance")
                                []
                                False
                                (Just "2023-10-30T08:25:30.335Z")
                                Nothing
                                Nothing

                        glossaryItems_ : Result String GlossaryItemsForUi
                        glossaryItems_ =
                            GlossaryItemsForUi.fromList
                                [ financeDescribedTag ]
                                Nothing
                                [ financeItem_ ]
                    in
                    glossaryItems_
                        |> Result.toMaybe
                        |> Maybe.andThen
                            (\items ->
                                -- This should NOT find the item because the disambiguated term
                                -- is "Finance (Finance)", not "Finance"
                                GlossaryItemsForUi.itemWhoseDisambiguatedPreferredTermMatchesTag financeTagId items
                            )
                        |> Expect.equal Nothing
            , test "returns Nothing when tag doesn't exist" <|
                \_ ->
                    glossaryItemsForUi
                        |> GlossaryItemsForUi.itemWhoseDisambiguatedPreferredTermMatchesTag (TagId.create "nonexistent")
                        |> Expect.equal Nothing
            , test "returns Nothing when no item matches tag" <|
                \_ ->
                    let
                        -- Create an item that doesn't match the gardening tag
                        financeItem_ : GlossaryItemForUi
                        financeItem_ =
                            GlossaryItemForUi.create
                                (GlossaryItemId.create "Finance")
                                (Term.fromMarkdown "Finance" False)
                                []
                                Nothing
                                [ financeTag ]
                                (Just <| Definition.fromMarkdown "About finance")
                                []
                                False
                                (Just "2023-10-30T08:25:30.335Z")
                                Nothing
                                Nothing

                        glossaryItems_ : Result String GlossaryItemsForUi
                        glossaryItems_ =
                            GlossaryItemsForUi.fromList
                                [ financeDescribedTag
                                , gardeningDescribedTag
                                ]
                                Nothing
                                [ financeItem_ ]
                    in
                    glossaryItems_
                        |> Result.toMaybe
                        |> Maybe.andThen
                            (\items ->
                                GlossaryItemsForUi.itemWhoseDisambiguatedPreferredTermMatchesTag gardeningTagId items
                            )
                        |> Expect.equal Nothing
            , test "returns Nothing when item term matches tag but item doesn't have that tag" <|
                \_ ->
                    let
                        -- Create an item with term "Finance" but without the finance tag
                        financeItem_ : GlossaryItemForUi
                        financeItem_ =
                            GlossaryItemForUi.create
                                (GlossaryItemId.create "Finance")
                                (Term.fromMarkdown "Finance" False)
                                []
                                Nothing
                                []
                                (Just <| Definition.fromMarkdown "About finance")
                                []
                                False
                                (Just "2023-10-30T08:25:30.335Z")
                                Nothing
                                Nothing

                        glossaryItems_ : Result String GlossaryItemsForUi
                        glossaryItems_ =
                            GlossaryItemsForUi.fromList
                                [ financeDescribedTag ]
                                Nothing
                                [ financeItem_ ]
                    in
                    glossaryItems_
                        |> Result.toMaybe
                        |> Maybe.andThen
                            (\items ->
                                GlossaryItemsForUi.itemWhoseDisambiguatedPreferredTermMatchesTag financeTagId items
                            )
                        |> Expect.equal Nothing
            , test "returns first matching item when multiple items match" <|
                \_ ->
                    let
                        financeItem1 : GlossaryItemForUi
                        financeItem1 =
                            GlossaryItemForUi.create
                                (GlossaryItemId.create "Finance1")
                                (Term.fromMarkdown "Finance" False)
                                []
                                Nothing
                                [ financeTag ]
                                (Just <| Definition.fromMarkdown "About finance 1")
                                []
                                False
                                (Just "2023-10-30T08:25:30.335Z")
                                Nothing
                                Nothing

                        financeItem2 : GlossaryItemForUi
                        financeItem2 =
                            GlossaryItemForUi.create
                                (GlossaryItemId.create "Finance2")
                                (Term.fromMarkdown "Finance" False)
                                []
                                (Just financeTag)
                                []
                                (Just <| Definition.fromMarkdown "About finance 2")
                                []
                                False
                                (Just "2023-10-30T08:25:30.335Z")
                                Nothing
                                Nothing

                        glossaryItems_ : Result String GlossaryItemsForUi
                        glossaryItems_ =
                            GlossaryItemsForUi.fromList
                                [ financeDescribedTag ]
                                Nothing
                                [ financeItem1, financeItem2 ]
                    in
                    glossaryItems_
                        |> Result.toMaybe
                        |> Maybe.andThen
                            (\items ->
                                GlossaryItemsForUi.itemWhoseDisambiguatedPreferredTermMatchesTag financeTagId items
                            )
                        |> Maybe.map GlossaryItemForUi.id
                        |> Maybe.map (\id -> List.member id [ GlossaryItemId.create "Finance1", GlossaryItemId.create "Finance2" ])
                        |> Expect.equal (Just True)
            ]
        ]
