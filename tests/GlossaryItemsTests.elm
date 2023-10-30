module GlossaryItemsTests exposing (suite)

import Data.GlossaryItem.Definition as Definition exposing (Definition)
import Data.GlossaryItem.Tag as Tag exposing (Tag)
import Data.GlossaryItem.Term as Term
import Data.GlossaryItemForHtml as GlossaryItemForHtml exposing (GlossaryItemForHtml)
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Data.TagDescription as TagDescription exposing (TagDescription)
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


glossaryItems : GlossaryItems
glossaryItems =
    GlossaryItems.fromList
        [ ( computerScienceTag, computerScienceTagDescription )
        , ( financeTag, financeTagDescription )
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
        [ test "does stuff" <|
            \_ ->
                1
                    |> Expect.equal
                        1
        ]
