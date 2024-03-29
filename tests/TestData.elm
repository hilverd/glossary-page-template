module TestData exposing (..)

import Data.AboutParagraph as AboutParagraph
import Data.CardWidth as CardWidth
import Data.DescribedTag as DescribedTag exposing (DescribedTag)
import Data.Glossary as Glossary exposing (Glossary)
import Data.GlossaryItem.Definition as Definition exposing (Definition)
import Data.GlossaryItem.DisambiguatedTerm as DisambiguatedTerm
import Data.GlossaryItem.Tag as Tag exposing (Tag)
import Data.GlossaryItem.Term as Term
import Data.GlossaryItemForUi as GlossaryItemForUi exposing (GlossaryItemForUi)
import Data.GlossaryItemId as GlossaryItemId
import Data.GlossaryItems exposing (GlossaryItems)
import Data.GlossaryTitle as GlossaryTitle
import Data.GlossaryVersionNumber as GlossaryVersionNumber
import Data.TagDescription as TagDescription exposing (TagDescription)
import Data.TagId as TagId exposing (TagId)


computerScienceTagId : TagId
computerScienceTagId =
    TagId.create "Computer Science"


computerScienceTag : Tag
computerScienceTag =
    Tag.fromMarkdown "Computer Science"


computerScienceTagDescription : TagDescription
computerScienceTagDescription =
    TagDescription.fromMarkdown "These are items about computer science — the study of computation, information, and automation."


financeTagId : TagId
financeTagId =
    TagId.create "Finance"


financeTag : Tag
financeTag =
    Tag.fromMarkdown "Finance"


financeTagDescription : TagDescription
financeTagDescription =
    TagDescription.fromMarkdown "These are items about finance — the study and discipline of money, currency and capital assets."


gardeningTagId : TagId
gardeningTagId =
    TagId.create "Gardening"


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


defaultComputerScienceItem : GlossaryItemForUi
defaultComputerScienceItem =
    GlossaryItemForUi.create
        (GlossaryItemId.create "Default (Computer Science)")
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
        Nothing
        Nothing


defaultFinanceDefinition : Definition
defaultFinanceDefinition =
    Definition.fromMarkdown "In finance, default is failure to meet the legal obligations (or conditions) of a loan, for example when a home buyer fails to make a mortgage payment, or when a corporation or government fails to pay a bond which has reached maturity. A national or sovereign default is the failure or refusal of a government to repay its national debt."


defaultFinanceItem : GlossaryItemForUi
defaultFinanceItem =
    GlossaryItemForUi.create
        (GlossaryItemId.create "Default (Finance)")
        (Term.fromMarkdown "Default" False)
        []
        (Just financeTag)
        []
        (Just defaultFinanceDefinition)
        [ DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Loan" False ]
        False
        (Just "2023-10-30T08:25:24.765Z")
        Nothing
        Nothing


informationRetrievalDefinition : Definition
informationRetrievalDefinition =
    Definition.fromMarkdown "Information retrieval (IR) in computing and information science is the process of obtaining information system resources that are relevant to an information need from a collection of those resources. Searches can be based on full-text or other content-based indexing."


informationRetrievalItem : GlossaryItemForUi
informationRetrievalItem =
    GlossaryItemForUi.create
        (GlossaryItemId.create "Information retrieval")
        (Term.fromMarkdown "Information retrieval" False)
        [ Term.fromMarkdown "IR" True ]
        Nothing
        [ computerScienceTag ]
        (Just informationRetrievalDefinition)
        []
        False
        (Just "2023-09-16T07:09:19.630Z")
        Nothing
        Nothing


interestRateDefinition : Definition
interestRateDefinition =
    Definition.fromMarkdown "An interest rate is the amount of interest due per period, as a proportion of the amount lent, deposited, or borrowed (called the principal sum). The total interest on an amount lent or borrowed depends on the principal sum, the interest rate, the compounding frequency, and the length of time over which it is lent, deposited, or borrowed."


interestRateItem : GlossaryItemForUi
interestRateItem =
    GlossaryItemForUi.create
        (GlossaryItemId.create "Interest rate")
        (Term.fromMarkdown "Interest rate" False)
        [ Term.fromMarkdown "IR" True ]
        Nothing
        [ financeTag ]
        (Just interestRateDefinition)
        [ DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Loan" False ]
        False
        (Just "2023-10-30T08:25:30.335Z")
        Nothing
        Nothing


updatedInterestRateItem : GlossaryItemForUi
updatedInterestRateItem =
    GlossaryItemForUi.create
        (GlossaryItemId.create "Interest rate updated")
        (Term.fromMarkdown "Interest rate updated" False)
        [ Term.fromMarkdown "IR" True ]
        Nothing
        [ financeTag ]
        (Just interestRateDefinition)
        [ DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Loan" False ]
        False
        (Just "2023-10-30T08:25:30.335Z")
        Nothing
        Nothing


loanDefinition : Definition
loanDefinition =
    Definition.fromMarkdown "The transfer of money by one party to another with an agreement to pay it back. The recipient, or borrower, incurs a debt and is usually required to pay interest for the use of the money."


loanItem : GlossaryItemForUi
loanItem =
    GlossaryItemForUi.create
        (GlossaryItemId.create "Loan")
        (Term.fromMarkdown "Loan" False)
        []
        Nothing
        [ financeTag ]
        (Just loanDefinition)
        [ DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Interest rate" False ]
        False
        (Just "2023-10-30T08:26:18.523Z")
        Nothing
        Nothing


updatedLoanItem : GlossaryItemForUi
updatedLoanItem =
    GlossaryItemForUi.create
        (GlossaryItemId.create "Loan")
        (Term.fromMarkdown "Loan" False)
        []
        Nothing
        [ financeTag ]
        (Just loanDefinition)
        [ DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Interest rate updated" False ]
        False
        (Just "2023-10-30T08:26:18.523Z")
        Nothing
        Nothing


computerScienceDescribedTag : DescribedTag
computerScienceDescribedTag =
    DescribedTag.create
        computerScienceTagId
        computerScienceTag
        computerScienceTagDescription


financeDescribedTag : DescribedTag
financeDescribedTag =
    DescribedTag.create
        financeTagId
        financeTag
        financeTagDescription


gardeningDescribedTag : DescribedTag
gardeningDescribedTag =
    DescribedTag.create
        gardeningTagId
        gardeningTag
        gardeningTagDescription


glossary : Glossary
glossary =
    Glossary.create
        True
        True
        True
        False
        CardWidth.Intermediate
        (GlossaryTitle.fromMarkdown "Example Glossary")
        (AboutParagraph.fromMarkdown "An example glossary.")
        []
        [ computerScienceDescribedTag
        , financeDescribedTag
        , gardeningDescribedTag
        ]
        [ defaultComputerScienceItem
        , defaultFinanceItem
        , informationRetrievalItem
        , interestRateItem
        , loanItem
        ]
        GlossaryVersionNumber.initial


glossaryItems : GlossaryItems
glossaryItems =
    Glossary.items glossary
