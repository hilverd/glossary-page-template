module Data.GlossaryItemsTests exposing (houseworkTagId, suite)

import Data.DescribedTag as DescribedTag
import Data.GlossaryItem.DisambiguatedTerm as DisambiguatedTerm
import Data.GlossaryItem.RawTerm as RawTerm
import Data.GlossaryItem.Tag as Tag
import Data.GlossaryItem.Term as Term
import Data.GlossaryItemForHtml as GlossaryItemForHtml exposing (GlossaryItemForHtml)
import Data.GlossaryItemId as GlossaryItemId exposing (GlossaryItemId)
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
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
    DescribedTag.create (Just <| houseworkTagId) houseworkTag houseworkTagDescription


suite : Test
suite =
    describe "The Data.GlossaryItems module"
        [ test "inserts tags" <|
            \_ ->
                let
                    tagsChanges : TagsChanges
                    tagsChanges =
                        TagsChanges.empty
                            |> TagsChanges.insert houseworkDescribedTag
                in
                glossaryItems
                    |> GlossaryItems.applyTagsChanges tagsChanges
                    |> Result.map GlossaryItems.tagsWithDescriptions
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
                glossaryItems
                    |> GlossaryItems.applyTagsChanges tagsChanges
                    |> Result.map (GlossaryItems.orderedAlphabetically Nothing >> List.map Tuple.second)
                    |> Result.map
                        (List.map
                            (\glossaryItemForHtml ->
                                ( GlossaryItemForHtml.disambiguatedPreferredTerm glossaryItemForHtml
                                    |> DisambiguatedTerm.toTerm
                                    |> Term.raw
                                    |> RawTerm.toString
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
                            |> TagsChanges.update computerScienceTagId houseworkDescribedTag
                in
                glossaryItems
                    |> GlossaryItems.applyTagsChanges tagsChanges
                    |> Result.map (GlossaryItems.get <| GlossaryItemForHtml.id defaultComputerScienceItem)
                    |> Expect.equal
                        (Ok <|
                            Just <|
                                GlossaryItemForHtml.create
                                    (GlossaryItemForHtml.id defaultComputerScienceItem)
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
                glossaryItems
                    |> GlossaryItems.applyTagsChanges tagsChanges
                    |> Result.map GlossaryItems.tagsWithDescriptions
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
                glossaryItems
                    |> GlossaryItems.applyTagsChanges tagsChanges
                    |> Result.map (GlossaryItems.get <| GlossaryItemForHtml.id defaultFinanceItem)
                    |> Expect.equal
                        (Ok <|
                            Just <|
                                GlossaryItemForHtml.create
                                    (GlossaryItemForHtml.id defaultFinanceItem)
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
                    |> GlossaryItems.remove (GlossaryItemForHtml.id defaultComputerScienceItem)
                    |> Result.andThen (GlossaryItems.insert defaultComputerScienceItem)
                    |> Result.map (\( _, updatedItems ) -> GlossaryItems.orderedAlphabetically Nothing updatedItems)
                    |> Expect.equal
                        (Ok
                            [ ( GlossaryItemForHtml.id defaultComputerScienceItem, defaultComputerScienceItem )
                            , ( GlossaryItemForHtml.id defaultFinanceItem, defaultFinanceItem )
                            , ( GlossaryItemForHtml.id informationRetrievalItem, informationRetrievalItem )
                            , ( GlossaryItemForHtml.id interestRateItem, interestRateItem )
                            , ( GlossaryItemForHtml.id loanItem, loanItem )
                            ]
                        )
        , test "updates items" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.update (GlossaryItemForHtml.id interestRateItem) updatedInterestRateItem
                    |> Result.map (GlossaryItems.orderedAlphabetically Nothing)
                    |> Expect.equal
                        (Ok
                            [ ( GlossaryItemForHtml.id defaultComputerScienceItem, defaultComputerScienceItem )
                            , ( GlossaryItemForHtml.id defaultFinanceItem, defaultFinanceItem )
                            , ( GlossaryItemForHtml.id informationRetrievalItem, informationRetrievalItem )
                            , ( GlossaryItemForHtml.id updatedInterestRateItem, updatedInterestRateItem )
                            , ( GlossaryItemForHtml.id updatedLoanItem, updatedLoanItem )
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
                                        |> Maybe.withDefault (GlossaryItemId.create "")
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
                    |> GlossaryItems.get (GlossaryItemForHtml.id defaultComputerScienceItem)
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
                        [ computerScienceDescribedTag
                        , financeDescribedTag
                        , gardeningDescribedTag
                        ]
        , test "gets all tags along with their tag IDs" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.tagByIdList
                    |> Expect.equal
                        [ ( computerScienceTagId, computerScienceTag )
                        , ( financeTagId, financeTag )
                        , ( gardeningTagId, gardeningTag )
                        ]
        , test "looks up a tag ID from its contents" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.tagIdFromTag computerScienceTag
                    |> Expect.equal (Just computerScienceTagId)
        , test "looks up a tag from its ID" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.tagFromId financeTagId
                    |> Expect.equal (Just financeTag)
        , test "looks up a tag description from its ID" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.tagDescriptionFromId gardeningTagId
                    |> Expect.equal (Just gardeningTagDescription)
        , test "returns disambiguated preferred term for item with given ID" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.disambiguatedPreferredTerm (GlossaryItemForHtml.id defaultComputerScienceItem)
                    |> Expect.equal (Just <| DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Default (Computer Science)" False)
        , test "returns all disambiguated preferred terms" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.disambiguatedPreferredTerms Nothing
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
                glossaryItems
                    |> GlossaryItems.disambiguatedPreferredTerms (Just financeTagId)
                    |> List.map Tuple.second
                    |> Expect.equal
                        [ DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Default (Finance)" False
                        , DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Interest rate" False
                        , DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Loan" False
                        ]
        , test "looks up the ID of the item whose disambiguated preferred term has the given ID" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.itemIdFromRawDisambiguatedPreferredTerm (RawTerm.fromString "Default (Finance)")
                    |> Expect.equal (Just <| GlossaryItemForHtml.id defaultFinanceItem)
        , test "looks up the ID of the item with the given fragment identifier" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.itemIdFromFragmentIdentifier "Default_(Finance)"
                    |> Expect.equal (Just <| GlossaryItemForHtml.id defaultFinanceItem)
        , test "looks up the disambiguated preferred term of the item with the given raw disambiguated preferred term" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.disambiguatedPreferredTermFromRaw (RawTerm.fromString "Default (Finance)")
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
                    |> GlossaryItems.disambiguatedPreferredTermsWhichHaveDefinitions (Just financeTagId)
                    |> Expect.equal
                        [ DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Default (Finance)" False
                        , DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Interest rate" False
                        , DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Loan" False
                        ]
        , test "returns the IDs of the items that list this item as a related one" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.relatedForWhichItems (GlossaryItemForHtml.id loanItem)
                    |> Expect.equal
                        [ GlossaryItemForHtml.id interestRateItem
                        , GlossaryItemForHtml.id defaultFinanceItem
                        ]
        , test "looks up disambiguated preferred term of item whose disambiguated preferred term has given ID" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.disambiguatedPreferredTermFromRaw (RawTerm.fromString "Default (Finance)")
                    |> Expect.equal (Just <| DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Default (Finance)" False)
        , test "returns a list of pairs associating each alternative term with the disambiguated preferred terms that it appears together with" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.disambiguatedPreferredTermsByAlternativeTerm Nothing
                    |> Expect.equal
                        [ ( Term.fromMarkdown "Preset" False
                          , [ ( GlossaryItemForHtml.id defaultComputerScienceItem, DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Default (Computer Science)" False ) ]
                          )
                        , ( Term.fromMarkdown "IR" True
                          , [ ( GlossaryItemForHtml.id interestRateItem, DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Interest rate" False )
                            , ( GlossaryItemForHtml.id informationRetrievalItem, DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Information retrieval" False )
                            ]
                          )
                        , ( Term.fromMarkdown "Factory preset" False
                          , [ ( GlossaryItemForHtml.id defaultComputerScienceItem, DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Default (Computer Science)" False ) ]
                          )
                        ]
        , test "returns items in alphabetical order" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.orderedAlphabetically Nothing
                    |> Expect.equal
                        [ ( GlossaryItemForHtml.id defaultComputerScienceItem, defaultComputerScienceItem )
                        , ( GlossaryItemForHtml.id defaultFinanceItem, defaultFinanceItem )
                        , ( GlossaryItemForHtml.id informationRetrievalItem, informationRetrievalItem )
                        , ( GlossaryItemForHtml.id interestRateItem, interestRateItem )
                        , ( GlossaryItemForHtml.id loanItem, loanItem )
                        ]
        , test "returns items in alphabetical order with tag filter applied" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.orderedAlphabetically (Just computerScienceTagId)
                    |> List.map Tuple.first
                    |> Expect.equal
                        [ GlossaryItemForHtml.id defaultComputerScienceItem
                        , GlossaryItemForHtml.id informationRetrievalItem
                        ]
        , test "when tag filter is applied, removes non-matching related items" <|
            \_ ->
                let
                    defaultComputerScienceItem_ : GlossaryItemForHtml
                    defaultComputerScienceItem_ =
                        GlossaryItemForHtml.create
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

                    defaultFinanceItem_ : GlossaryItemForHtml
                    defaultFinanceItem_ =
                        GlossaryItemForHtml.create
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

                    glossaryItems_ : Result String GlossaryItems
                    glossaryItems_ =
                        GlossaryItems.fromList
                            [ computerScienceDescribedTag
                            , financeDescribedTag
                            ]
                            [ defaultComputerScienceItem_
                            , defaultFinanceItem_
                            ]
                in
                glossaryItems_
                    |> Result.map (GlossaryItems.orderedAlphabetically (Just computerScienceTagId))
                    |> Expect.equal
                        (Ok
                            [ ( GlossaryItemForHtml.id defaultComputerScienceItem
                              , GlossaryItemForHtml.create
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
                glossaryItems
                    |> GlossaryItems.orderedByMostMentionedFirst Nothing
                    |> List.map Tuple.first
                    |> Expect.equal
                        [ GlossaryItemForHtml.id interestRateItem
                        , GlossaryItemForHtml.id loanItem
                        , GlossaryItemForHtml.id informationRetrievalItem
                        , GlossaryItemForHtml.id defaultComputerScienceItem
                        , GlossaryItemForHtml.id defaultFinanceItem
                        ]
        , test "returns items ordered by most mentioned first with tag filter applied" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.orderedByMostMentionedFirst (Just financeTagId)
                    |> List.map Tuple.first
                    |> Expect.equal
                        [ GlossaryItemForHtml.id interestRateItem
                        , GlossaryItemForHtml.id loanItem
                        , GlossaryItemForHtml.id defaultFinanceItem
                        ]
        , test "returns items ordered 'focused on' a specific item" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.orderedFocusedOn Nothing (GlossaryItemForHtml.id defaultFinanceItem)
                    |> (\( lhs, rhs ) -> ( List.map Tuple.first lhs, List.map Tuple.first rhs ))
                    |> Expect.equal
                        ( [ GlossaryItemForHtml.id defaultFinanceItem
                          , GlossaryItemForHtml.id loanItem
                          , GlossaryItemForHtml.id interestRateItem
                          ]
                        , [ GlossaryItemForHtml.id defaultComputerScienceItem
                          , GlossaryItemForHtml.id informationRetrievalItem
                          ]
                        )
        , test "returns items ordered 'focused on' a specific item with tag filter applied" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.orderedFocusedOn (Just financeTagId) (GlossaryItemForHtml.id defaultFinanceItem)
                    |> (\( lhs, rhs ) -> ( List.map Tuple.first lhs, List.map Tuple.first rhs ))
                    |> Expect.equal
                        ( [ GlossaryItemForHtml.id defaultFinanceItem
                          , GlossaryItemForHtml.id loanItem
                          , GlossaryItemForHtml.id interestRateItem
                          ]
                        , []
                        )
        , test "sorts tags in items alphabetically" <|
            \_ ->
                GlossaryItems.fromList
                    [ financeDescribedTag
                    , houseworkDescribedTag
                    , gardeningDescribedTag
                    , computerScienceDescribedTag
                    ]
                    [ GlossaryItemForHtml.create
                        (GlossaryItemForHtml.id defaultComputerScienceItem)
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
                    |> Result.map (GlossaryItems.get <| GlossaryItemForHtml.id defaultComputerScienceItem)
                    |> Expect.equal
                        (Ok <|
                            Just <|
                                GlossaryItemForHtml.create
                                    (GlossaryItemForHtml.id defaultComputerScienceItem)
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
                    [ financeDescribedTag
                    , houseworkDescribedTag
                    , gardeningDescribedTag
                    , computerScienceDescribedTag
                    ]
                    []
                    |> Result.map GlossaryItems.tags
                    |> Expect.equal (Ok [ computerScienceTag, financeTag, gardeningTag, houseworkTag ])
        , test "sorts tags with descriptions alphabetically" <|
            \_ ->
                GlossaryItems.fromList
                    [ financeDescribedTag
                    , houseworkDescribedTag
                    , gardeningDescribedTag
                    , computerScienceDescribedTag
                    ]
                    []
                    |> Result.map GlossaryItems.tagsWithDescriptions
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
                GlossaryItems.fromList
                    [ financeDescribedTag
                    , DescribedTag.create
                        (Just <| TagId.create "some-other-tag-id")
                        financeTag
                        financeTagDescription
                    ]
                    []
                    |> Expect.equal
                        (Err "tag \"Finance\" appears multiple times")
        , test "returns error for duplicate disambiguated preferred terms" <|
            \_ ->
                GlossaryItems.fromList
                    [ financeDescribedTag ]
                    [ GlossaryItemForHtml.create
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
                    , GlossaryItemForHtml.create
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
                        (Err "there are multiple items with (disambiguated) preferred term identifier \"Foo_(Finance)\"")
        , test "returns error for multiple disambiguated preferred terms with the same fragment identifier" <|
            \_ ->
                GlossaryItems.fromList
                    [ financeDescribedTag ]
                    [ GlossaryItemForHtml.create
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
                    , GlossaryItemForHtml.create
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
                        (Err "there are multiple items with (disambiguated) preferred term identifier \"Foo_(Finance)\"")
        ]
