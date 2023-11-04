module GlossaryItemsTests exposing (suite)

import Data.GlossaryItem.Definition as Definition exposing (Definition)
import Data.GlossaryItem.Tag as Tag exposing (Tag)
import Data.GlossaryItem.Term as Term
import Data.GlossaryItem.TermId as TermId
import Data.GlossaryItemForHtml as GlossaryItemForHtml exposing (GlossaryItemForHtml)
import Data.GlossaryItemId as GlossaryItemId
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Data.TagDescription as TagDescription exposing (TagDescription)
import Data.TagId as TagId
import Expect
import Test exposing (Test, describe, test)


computerScienceTag : Tag
computerScienceTag =
    Tag.fromMarkdown "Computer Science"


computerScienceTagDescription : TagDescription
computerScienceTagDescription =
    TagDescription.fromMarkdown "These are items about computer science — the study of computation, information, and automation."


financeTag : Tag
financeTag =
    Tag.fromMarkdown "Finance"


financeTagDescription : TagDescription
financeTagDescription =
    TagDescription.fromMarkdown "These are items about finance — the study and discipline of money, currency and capital assets."


gardeningTag : Tag
gardeningTag =
    Tag.fromMarkdown "Gardening"


gardeningTagDescription : TagDescription
gardeningTagDescription =
    TagDescription.fromMarkdown "These are items about gardening — the practice of growing and cultivating plants as part of horticulture."


houseworkTag : Tag
houseworkTag =
    Tag.fromMarkdown "Housework"


houseworkTagDescription : TagDescription
houseworkTagDescription =
    TagDescription.fromMarkdown "These are items about housework — the act of overseeing the organisational, day-to-day operations of a house or estate."


defaultComputerScienceDefinition : Definition
defaultComputerScienceDefinition =
    Definition.fromMarkdown "The preexisting value of a user-configurable setting that is assigned to a software application, computer program or device. Such settings are also called presets or factory presets, especially for electronic devices."


defaultComputerScienceItem : GlossaryItemForHtml
defaultComputerScienceItem =
    GlossaryItemForHtml.create
        (Term.fromMarkdown "Default" False)
        [ Term.fromMarkdown "Preset" False
        , Term.fromMarkdown "Factory preset" False
        ]
        (Just computerScienceTag)
        []
        (Just defaultComputerScienceDefinition)
        []
        False
        (Just "2023-09-15T19:58:59.573Z")


defaultFinanceDefinition : Definition
defaultFinanceDefinition =
    Definition.fromMarkdown "In finance, default is failure to meet the legal obligations (or conditions) of a loan, for example when a home buyer fails to make a mortgage payment, or when a corporation or government fails to pay a bond which has reached maturity. A national or sovereign default is the failure or refusal of a government to repay its national debt."


defaultFinanceItem : GlossaryItemForHtml
defaultFinanceItem =
    GlossaryItemForHtml.create
        (Term.fromMarkdown "Default" False)
        []
        (Just financeTag)
        []
        (Just defaultFinanceDefinition)
        [ Term.fromMarkdown "Loan" False ]
        False
        (Just "2023-10-30T08:25:24.765Z")


informationRetrievalDefinition : Definition
informationRetrievalDefinition =
    Definition.fromMarkdown "Information retrieval (IR) in computing and information science is the process of obtaining information system resources that are relevant to an information need from a collection of those resources. Searches can be based on full-text or other content-based indexing."


informationRetrievalItem : GlossaryItemForHtml
informationRetrievalItem =
    GlossaryItemForHtml.create
        (Term.fromMarkdown "Information retrieval" False)
        [ Term.fromMarkdown "IR" True ]
        Nothing
        [ computerScienceTag ]
        (Just informationRetrievalDefinition)
        []
        False
        (Just "2023-09-16T07:09:19.630Z")


interestRateDefinition : Definition
interestRateDefinition =
    Definition.fromMarkdown "An interest rate is the amount of interest due per period, as a proportion of the amount lent, deposited, or borrowed (called the principal sum). The total interest on an amount lent or borrowed depends on the principal sum, the interest rate, the compounding frequency, and the length of time over which it is lent, deposited, or borrowed."


interestRateItem : GlossaryItemForHtml
interestRateItem =
    GlossaryItemForHtml.create
        (Term.fromMarkdown "Interest rate" False)
        [ Term.fromMarkdown "IR" True ]
        Nothing
        [ financeTag ]
        (Just interestRateDefinition)
        [ Term.fromMarkdown "Loan" False ]
        False
        (Just "2023-10-30T08:25:30.335Z")


updatedInterestRateItem : GlossaryItemForHtml
updatedInterestRateItem =
    GlossaryItemForHtml.create
        (Term.fromMarkdown "Interest rate updated" False)
        [ Term.fromMarkdown "IR" True ]
        Nothing
        [ financeTag ]
        (Just interestRateDefinition)
        [ Term.fromMarkdown "Loan" False ]
        False
        (Just "2023-10-30T08:25:30.335Z")


loanDefinition : Definition
loanDefinition =
    Definition.fromMarkdown "The transfer of money by one party to another with an agreement to pay it back. The recipient, or borrower, incurs a debt and is usually required to pay interest for the use of the money."


loanItem : GlossaryItemForHtml
loanItem =
    GlossaryItemForHtml.create
        (Term.fromMarkdown "Loan" False)
        []
        Nothing
        [ financeTag ]
        (Just loanDefinition)
        [ Term.fromMarkdown "Interest rate" False ]
        False
        (Just "2023-10-30T08:26:18.523Z")


updatedLoanItem : GlossaryItemForHtml
updatedLoanItem =
    GlossaryItemForHtml.create
        (Term.fromMarkdown "Loan" False)
        []
        Nothing
        [ financeTag ]
        (Just loanDefinition)
        [ Term.fromMarkdown "Interest rate updated" False ]
        False
        (Just "2023-10-30T08:26:18.523Z")


glossaryItems : GlossaryItems
glossaryItems =
    GlossaryItems.fromList
        [ ( computerScienceTag, computerScienceTagDescription )
        , ( financeTag, financeTagDescription )
        , ( gardeningTag, gardeningTagDescription )
        ]
        [ defaultComputerScienceItem
        , defaultFinanceItem
        , informationRetrievalItem
        , interestRateItem
        , loanItem
        ]


suite : Test
suite =
    describe "The GlossaryItems module"
        [ test "inserts tags" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.insertTag houseworkTag houseworkTagDescription
                    |> GlossaryItems.tagsWithDescriptions
                    |> Expect.equal
                        [ ( computerScienceTag, computerScienceTagDescription )
                        , ( financeTag, financeTagDescription )
                        , ( gardeningTag, gardeningTagDescription )
                        , ( houseworkTag, houseworkTagDescription )
                        ]
        , test "ignores attempts to insert existing tags" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.insertTag gardeningTag gardeningTagDescription
                    |> Expect.equal glossaryItems
        , test "updates tags" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.updateTag (TagId.create 0) houseworkTag houseworkTagDescription
                    |> GlossaryItems.tagsWithDescriptions
                    |> Expect.equal
                        [ ( financeTag, financeTagDescription )
                        , ( gardeningTag, gardeningTagDescription )
                        , ( houseworkTag, houseworkTagDescription )
                        ]
        , test "updates tags in items" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.updateTag (TagId.create 0) houseworkTag houseworkTagDescription
                    |> GlossaryItems.get (GlossaryItemId.create 0)
                    |> Expect.equal
                        (Just <|
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
                        )
        , test "ignores attempts to update tags for non-existing tag IDs" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.updateTag (TagId.create 99) houseworkTag houseworkTagDescription
                    |> Expect.equal glossaryItems
        , test "removes tags" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.removeTag (TagId.create 1)
                    |> GlossaryItems.tagsWithDescriptions
                    |> Expect.equal
                        [ ( computerScienceTag, computerScienceTagDescription )
                        , ( gardeningTag, gardeningTagDescription )
                        ]
        , test "removes tags from items" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.removeTag (TagId.create 1)
                    |> GlossaryItems.get (GlossaryItemId.create 1)
                    |> Expect.equal
                        (Just <|
                            GlossaryItemForHtml.create
                                (Term.fromMarkdown "Default" False)
                                []
                                Nothing
                                []
                                (Just defaultFinanceDefinition)
                                [ Term.fromMarkdown "Loan" False ]
                                False
                                (Just "2023-10-30T08:25:24.765Z")
                        )
        , test "removes and inserts items" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.remove (GlossaryItemId.create 0)
                    |> GlossaryItems.insert defaultComputerScienceItem
                    |> GlossaryItems.orderedAlphabetically Nothing
                    |> Expect.equal
                        [ ( GlossaryItemId.create 0, defaultComputerScienceItem )
                        , ( GlossaryItemId.create 1, defaultFinanceItem )
                        , ( GlossaryItemId.create 2, informationRetrievalItem )
                        , ( GlossaryItemId.create 3, interestRateItem )
                        , ( GlossaryItemId.create 4, loanItem )
                        ]
        , test "updates items" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.update (GlossaryItemId.create 3) updatedInterestRateItem
                    |> GlossaryItems.orderedAlphabetically Nothing
                    |> Expect.equal
                        [ ( GlossaryItemId.create 0, defaultComputerScienceItem )
                        , ( GlossaryItemId.create 1, defaultFinanceItem )
                        , ( GlossaryItemId.create 2, informationRetrievalItem )
                        , ( GlossaryItemId.create 3, updatedInterestRateItem )
                        , ( GlossaryItemId.create 4, updatedLoanItem )
                        ]
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
                    |> Expect.equal (Just <| Term.fromMarkdown "Default (Computer Science)" False)
        , test "returns all disambiguated preferred terms" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.disambiguatedPreferredTerms Nothing
                    |> Expect.equal
                        [ Term.fromMarkdown "Default (Computer Science)" False
                        , Term.fromMarkdown "Default (Finance)" False
                        , Term.fromMarkdown "Information retrieval" False
                        , Term.fromMarkdown "Interest rate" False
                        , Term.fromMarkdown "Loan" False
                        ]
        , test "returns all disambiguated preferred terms with tag filter applied" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.disambiguatedPreferredTerms (Just <| TagId.create 1)
                    |> Expect.equal
                        [ Term.fromMarkdown "Default (Finance)" False
                        , Term.fromMarkdown "Interest rate" False
                        , Term.fromMarkdown "Loan" False
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
                    |> Expect.equal (Just <| Term.fromMarkdown "Default (Finance)" False)
        , test "returns all of the disambiguated preferred terms which have a definition" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.disambiguatedPreferredTermsWhichHaveDefinitions Nothing
                    |> Expect.equal
                        [ Term.fromMarkdown "Default (Computer Science)" False
                        , Term.fromMarkdown "Default (Finance)" False
                        , Term.fromMarkdown "Information retrieval" False
                        , Term.fromMarkdown "Interest rate" False
                        , Term.fromMarkdown "Loan" False
                        ]
        , test "returns all of the disambiguated preferred terms which have a definition with tag filter applied" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.disambiguatedPreferredTermsWhichHaveDefinitions (Just <| TagId.create 1)
                    |> Expect.equal
                        [ Term.fromMarkdown "Default (Finance)" False
                        , Term.fromMarkdown "Interest rate" False
                        , Term.fromMarkdown "Loan" False
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
                    |> Expect.equal (Just <| Term.fromMarkdown "Default (Finance)" False)
        , test "returns a list of pairs associating each alternative term with the disambiguated preferred terms that it appears together with" <|
            \_ ->
                glossaryItems
                    |> GlossaryItems.disambiguatedPreferredTermsByAlternativeTerm Nothing
                    |> Expect.equal
                        [ ( Term.fromMarkdown "Preset" False
                          , [ Term.fromMarkdown "Default (Computer Science)" False ]
                          )
                        , ( Term.fromMarkdown "IR" True
                          , [ Term.fromMarkdown "Interest rate" False
                            , Term.fromMarkdown "Information retrieval" False
                            ]
                          )
                        , ( Term.fromMarkdown "Factory preset" False
                          , [ Term.fromMarkdown "Default (Computer Science)" False ]
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
                    ]
                    |> GlossaryItems.get (GlossaryItemId.create 0)
                    |> Expect.equal
                        (Just <|
                            GlossaryItemForHtml.create
                                (Term.fromMarkdown "Foo" False)
                                []
                                (Just gardeningTag)
                                [ computerScienceTag, financeTag, houseworkTag ]
                                Nothing
                                []
                                False
                                (Just "2023-10-30T08:25:30.335Z")
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
                    |> GlossaryItems.tags
                    |> Expect.equal [ computerScienceTag, financeTag, gardeningTag, houseworkTag ]
        , test "sorts tags with descriptions alphabetically" <|
            \_ ->
                GlossaryItems.fromList
                    [ ( financeTag, financeTagDescription )
                    , ( houseworkTag, houseworkTagDescription )
                    , ( gardeningTag, gardeningTagDescription )
                    , ( computerScienceTag, computerScienceTagDescription )
                    ]
                    []
                    |> GlossaryItems.tagsWithDescriptions
                    |> Expect.equal
                        [ ( computerScienceTag, computerScienceTagDescription )
                        , ( financeTag, financeTagDescription )
                        , ( gardeningTag, gardeningTagDescription )
                        , ( houseworkTag, houseworkTagDescription )
                        ]
        ]
