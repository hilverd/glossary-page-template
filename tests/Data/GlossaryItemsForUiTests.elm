module Data.GlossaryItemsForUiTests exposing (houseworkTagId, suite)

import Data.DescribedTag as DescribedTag
import Data.GlossaryItem.DisambiguatedTerm as DisambiguatedTerm
import Data.GlossaryItem.RawTerm as RawTerm
import Data.GlossaryItem.Tag as Tag
import Data.GlossaryItem.Term as Term
import Data.GlossaryItemForUi as GlossaryItemForUi exposing (GlossaryItemForUi)
import Data.GlossaryItemId as GlossaryItemId exposing (GlossaryItemId)
import Data.GlossaryItemsForUi as GlossaryItemsForUi exposing (GlossaryItemsForUi)
import Data.TagId as TagId
import Data.TagsChanges as TagsChanges exposing (TagsChanges)
import Expect
import Test exposing (Test, describe, test)
import TestData exposing (..)


houseworkTagId : TagId.TagId
houseworkTagId =
    TagId.create "housework"


houseworkDescribedTag : DescribedTag.DescribedTag
houseworkDescribedTag =
    DescribedTag.create houseworkTagId houseworkTag houseworkTagDescription


suite : Test
suite =
    describe "The Data.GlossaryItemsForUi module"
        [ test "inserts tags" <|
            \_ ->
                let
                    tagsChanges : TagsChanges
                    tagsChanges =
                        TagsChanges.empty
                            |> TagsChanges.insert houseworkDescribedTag
                in
                glossaryItemsForUi
                    |> GlossaryItemsForUi.applyTagsChanges tagsChanges
                    |> Result.map GlossaryItemsForUi.describedTags
                    |> Expect.equal
                        (Ok
                            [ computerScienceDescribedTag
                            , financeDescribedTag
                            , gardeningDescribedTag
                            , houseworkDescribedTag
                            ]
                        )
        , test "updates tags" <|
            \_ ->
                let
                    tagsChanges : TagsChanges
                    tagsChanges =
                        TagsChanges.empty
                            |> TagsChanges.update computerScienceTagId financeDescribedTag
                            |> TagsChanges.update financeTagId computerScienceDescribedTag
                in
                glossaryItemsForUi
                    |> GlossaryItemsForUi.applyTagsChanges tagsChanges
                    |> Result.map (GlossaryItemsForUi.orderedAlphabetically Nothing >> List.map Tuple.second)
                    |> Result.map
                        (List.map
                            (\glossaryItemForUi ->
                                ( GlossaryItemForUi.disambiguatedPreferredTerm glossaryItemForUi
                                    |> DisambiguatedTerm.toTerm
                                    |> Term.raw
                                    |> RawTerm.toString
                                , GlossaryItemForUi.allTags glossaryItemForUi |> List.map Tag.raw
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
                            |> TagsChanges.update computerScienceTagId houseworkDescribedTag
                in
                glossaryItemsForUi
                    |> GlossaryItemsForUi.applyTagsChanges tagsChanges
                    |> Result.map (GlossaryItemsForUi.get <| GlossaryItemForUi.id defaultComputerScienceItem)
                    |> Expect.equal
                        (Ok <|
                            Just <|
                                GlossaryItemForUi.create
                                    (GlossaryItemForUi.id defaultComputerScienceItem)
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
                            |> TagsChanges.remove financeTagId
                in
                glossaryItemsForUi
                    |> GlossaryItemsForUi.applyTagsChanges tagsChanges
                    |> Result.map GlossaryItemsForUi.describedTags
                    |> Expect.equal
                        (Ok
                            [ computerScienceDescribedTag
                            , gardeningDescribedTag
                            ]
                        )
        , test "removes tags from items" <|
            \_ ->
                let
                    tagsChanges : TagsChanges
                    tagsChanges =
                        TagsChanges.empty
                            |> TagsChanges.remove financeTagId
                in
                glossaryItemsForUi
                    |> GlossaryItemsForUi.applyTagsChanges tagsChanges
                    |> Result.map (GlossaryItemsForUi.get <| GlossaryItemForUi.id defaultFinanceItem)
                    |> Expect.equal
                        (Ok <|
                            Just <|
                                GlossaryItemForUi.create
                                    (GlossaryItemForUi.id defaultFinanceItem)
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
                glossaryItemsForUi
                    |> GlossaryItemsForUi.remove (GlossaryItemForUi.id defaultComputerScienceItem)
                    |> Result.andThen (GlossaryItemsForUi.insert defaultComputerScienceItem)
                    |> Result.map (\( _, updatedItems ) -> GlossaryItemsForUi.orderedAlphabetically Nothing updatedItems)
                    |> Expect.equal
                        (Ok
                            [ ( GlossaryItemForUi.id defaultComputerScienceItem, defaultComputerScienceItem )
                            , ( GlossaryItemForUi.id defaultFinanceItem, defaultFinanceItem )
                            , ( GlossaryItemForUi.id informationRetrievalItem, informationRetrievalItem )
                            , ( GlossaryItemForUi.id interestRateItem, interestRateItem )
                            , ( GlossaryItemForUi.id loanItem, loanItem )
                            ]
                        )
        , test "updates items" <|
            \_ ->
                glossaryItemsForUi
                    |> GlossaryItemsForUi.update (GlossaryItemForUi.id interestRateItem) updatedInterestRateItem
                    |> Result.map (GlossaryItemsForUi.orderedAlphabetically Nothing)
                    |> Expect.equal
                        (Ok
                            [ ( GlossaryItemForUi.id defaultComputerScienceItem, defaultComputerScienceItem )
                            , ( GlossaryItemForUi.id defaultFinanceItem, defaultFinanceItem )
                            , ( GlossaryItemForUi.id informationRetrievalItem, informationRetrievalItem )
                            , ( GlossaryItemForUi.id updatedInterestRateItem, updatedInterestRateItem )
                            , ( GlossaryItemForUi.id updatedLoanItem, updatedLoanItem )
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
                            |> TagsChanges.insert gardeningDescribedTag
                            |> TagsChanges.insert financeDescribedTag
                            |> TagsChanges.insert computerScienceDescribedTag

                    glossaryItems =
                        [ defaultComputerScienceItem
                        , defaultFinanceItem
                        , informationRetrievalItem
                        , interestRateItem
                        , loanItem
                        ]
                in
                GlossaryItemsForUi.empty
                    |> GlossaryItemsForUi.applyTagsChanges tagsChanges
                    |> (\result ->
                            List.foldl
                                (\item ->
                                    Result.andThen
                                        (\items ->
                                            GlossaryItemsForUi.insert item items
                                                |> Result.map Tuple.second
                                        )
                                )
                                result
                                glossaryItems
                       )
                    |> (\result ->
                            let
                                itemId : GlossaryItemForUi -> GlossaryItemsForUi -> GlossaryItemId
                                itemId glossaryItemForUi result_ =
                                    result_
                                        |> GlossaryItemsForUi.orderedAlphabetically Nothing
                                        |> List.filterMap
                                            (\( id, glossaryItemForUi_ ) ->
                                                if GlossaryItemForUi.definition glossaryItemForUi_ == GlossaryItemForUi.definition glossaryItemForUi then
                                                    Just id

                                                else
                                                    Nothing
                                            )
                                        |> List.head
                                        |> Maybe.withDefault (GlossaryItemId.create "")
                            in
                            glossaryItems
                                |> List.foldl
                                    (\glossaryItemForUi result1 ->
                                        Result.andThen
                                            (\result1_ ->
                                                GlossaryItemsForUi.update
                                                    (itemId glossaryItemForUi result1_)
                                                    glossaryItemForUi
                                                    result1_
                                            )
                                            result1
                                    )
                                    result
                       )
                    |> Result.map (GlossaryItemsForUi.orderedAlphabetically Nothing)
                    |> Expect.equal
                        (glossaryItemsForUi
                            |> GlossaryItemsForUi.orderedAlphabetically Nothing
                            |> Ok
                        )
        , test "gets items by ID" <|
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
                    []
                    |> Expect.equal
                        (Err "tag \"Finance\" appears multiple times")
        , test "returns error for duplicate disambiguated preferred terms" <|
            \_ ->
                -- TODO: is this test essentially the same as the one below?
                GlossaryItemsForUi.fromList
                    [ financeDescribedTag ]
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
        ]
