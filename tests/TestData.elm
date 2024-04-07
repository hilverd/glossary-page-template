module TestData exposing (..)

import Data.AboutParagraph as AboutParagraph
import Data.CardWidth as CardWidth
import Data.DescribedTag as DescribedTag exposing (DescribedTag)
import Data.DescribedTagFromDom exposing (DescribedTagFromDom)
import Data.GlossaryForUi as GlossaryForUi exposing (GlossaryForUi)
import Data.GlossaryFromDom exposing (GlossaryFromDom)
import Data.GlossaryItem.Definition as Definition exposing (Definition)
import Data.GlossaryItem.DisambiguatedTerm as DisambiguatedTerm
import Data.GlossaryItem.Tag as Tag exposing (Tag)
import Data.GlossaryItem.Term as Term
import Data.GlossaryItemForUi as GlossaryItemForUi exposing (GlossaryItemForUi)
import Data.GlossaryItemFromDom exposing (GlossaryItemFromDom)
import Data.GlossaryItemId as GlossaryItemId
import Data.GlossaryItems exposing (GlossaryItems)
import Data.GlossaryTitle as GlossaryTitle
import Data.GlossaryVersionNumber as GlossaryVersionNumber
import Data.TagDescription as TagDescription exposing (TagDescription)
import Data.TagId as TagId exposing (TagId)


computerScienceTagRawId : String
computerScienceTagRawId =
    "Computer Science"


computerScienceTagId : TagId
computerScienceTagId =
    TagId.create computerScienceTagRawId


computerScienceRawTag : String
computerScienceRawTag =
    "Computer Science"


computerScienceTag : Tag
computerScienceTag =
    Tag.fromMarkdown computerScienceRawTag


computerScienceTagRawDescription : String
computerScienceTagRawDescription =
    "These are items about computer science — the study of computation, information, and automation."


computerScienceTagDescription : TagDescription
computerScienceTagDescription =
    TagDescription.fromMarkdown computerScienceTagRawDescription


financeTagRawId : String
financeTagRawId =
    "Finance"


financeTagId : TagId
financeTagId =
    TagId.create financeTagRawId


financeRawTag : String
financeRawTag =
    "Finance"


financeTag : Tag
financeTag =
    Tag.fromMarkdown financeRawTag


financeTagRawDescription : String
financeTagRawDescription =
    "These are items about finance — the study and discipline of money, currency and capital assets."


financeTagDescription : TagDescription
financeTagDescription =
    TagDescription.fromMarkdown financeTagRawDescription


gardeningTagRawId : String
gardeningTagRawId =
    "Gardening"


gardeningTagId : TagId
gardeningTagId =
    TagId.create gardeningTagRawId


gardeningRawTag : String
gardeningRawTag =
    "Gardening"


gardeningTag : Tag
gardeningTag =
    Tag.fromMarkdown gardeningRawTag


gardeningTagRawDescription : String
gardeningTagRawDescription =
    "These are items about gardening — the practice of growing and cultivating plants as part of horticulture."


gardeningTagDescription : TagDescription
gardeningTagDescription =
    TagDescription.fromMarkdown gardeningTagRawDescription


houseworkRawTag : String
houseworkRawTag =
    "Housework"


houseworkTag : Tag
houseworkTag =
    Tag.fromMarkdown houseworkRawTag


houseworkTagRawDescription : String
houseworkTagRawDescription =
    "These are items about housework — the act of overseeing the organisational, day-to-day operations of a house or estate."


houseworkTagDescription : TagDescription
houseworkTagDescription =
    TagDescription.fromMarkdown houseworkTagRawDescription


defaultComputerScienceRawDefinition : String
defaultComputerScienceRawDefinition =
    "The preexisting value of a user-configurable setting that is assigned to a software application, computer program or device. Such settings are also called presets or factory presets, especially for electronic devices."


defaultComputerScienceDefinition : Definition
defaultComputerScienceDefinition =
    Definition.fromMarkdown defaultComputerScienceRawDefinition


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


defaultComputerScienceItemFromDom : GlossaryItemFromDom
defaultComputerScienceItemFromDom =
    { id = "Default (Computer Science)"
    , preferredTerm = { isAbbreviation = False, body = "Default" }
    , alternativeTerms =
        [ { isAbbreviation = False, body = "Preset" }
        , { isAbbreviation = False, body = "Factory preset" }
        ]
    , disambiguationTag = Just computerScienceRawTag
    , normalTags = []
    , definition = Just defaultComputerScienceRawDefinition
    , relatedPreferredTerms = []
    , needsUpdating = False
    , lastUpdatedDateAsIso8601 = Just "2023-09-15T19:58:59.573Z"
    , lastUpdatedByName = Nothing
    , lastUpdatedByEmailAddress = Nothing
    }


defaultFinanceRawDefinition : String
defaultFinanceRawDefinition =
    "In finance, default is failure to meet the legal obligations (or conditions) of a loan, for example when a home buyer fails to make a mortgage payment, or when a corporation or government fails to pay a bond which has reached maturity. A national or sovereign default is the failure or refusal of a government to repay its national debt."


defaultFinanceDefinition : Definition
defaultFinanceDefinition =
    Definition.fromMarkdown defaultFinanceRawDefinition


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


defaultFinanceItemFromDom : GlossaryItemFromDom
defaultFinanceItemFromDom =
    { id = "Default (Finance)"
    , preferredTerm = { isAbbreviation = False, body = "Default" }
    , alternativeTerms = []
    , disambiguationTag = Just financeRawTag
    , normalTags = []
    , definition = Just defaultFinanceRawDefinition
    , relatedPreferredTerms = [ { isAbbreviation = False, body = "Loan" } ]
    , needsUpdating = False
    , lastUpdatedDateAsIso8601 = Just "2023-10-30T08:25:24.765Z"
    , lastUpdatedByName = Nothing
    , lastUpdatedByEmailAddress = Nothing
    }


informationRetrievalRawDefinition : String
informationRetrievalRawDefinition =
    "Information retrieval (IR) in computing and information science is the process of obtaining information system resources that are relevant to an information need from a collection of those resources. Searches can be based on full-text or other content-based indexing."


informationRetrievalDefinition : Definition
informationRetrievalDefinition =
    Definition.fromMarkdown informationRetrievalRawDefinition


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


informationRetrievalItemFromDom : GlossaryItemFromDom
informationRetrievalItemFromDom =
    { id = "Information retrieval"
    , preferredTerm = { isAbbreviation = False, body = "Information retrieval" }
    , alternativeTerms = [ { isAbbreviation = False, body = "IR" } ]
    , disambiguationTag = Nothing
    , normalTags = [ computerScienceRawTag ]
    , definition = Just informationRetrievalRawDefinition
    , relatedPreferredTerms = []
    , needsUpdating = False
    , lastUpdatedDateAsIso8601 = Just "2023-09-16T07:09:19.630Z"
    , lastUpdatedByName = Nothing
    , lastUpdatedByEmailAddress = Nothing
    }


interestRateRawDefinition : String
interestRateRawDefinition =
    "An interest rate is the amount of interest due per period, as a proportion of the amount lent, deposited, or borrowed (called the principal sum). The total interest on an amount lent or borrowed depends on the principal sum, the interest rate, the compounding frequency, and the length of time over which it is lent, deposited, or borrowed."


interestRateDefinition : Definition
interestRateDefinition =
    Definition.fromMarkdown interestRateRawDefinition


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


interestRateItemFromDom : GlossaryItemFromDom
interestRateItemFromDom =
    { id = "Interest rate"
    , preferredTerm = { isAbbreviation = False, body = "Interest rate" }
    , alternativeTerms = [ { isAbbreviation = True, body = "IR" } ]
    , disambiguationTag = Nothing
    , normalTags = [ financeRawTag ]
    , definition = Just interestRateRawDefinition
    , relatedPreferredTerms = [ { isAbbreviation = False, body = "Loan" } ]
    , needsUpdating = False
    , lastUpdatedDateAsIso8601 = Just "2023-10-30T08:25:30.335Z"
    , lastUpdatedByName = Nothing
    , lastUpdatedByEmailAddress = Nothing
    }


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


loanRawDefinition : String
loanRawDefinition =
    "The transfer of money by one party to another with an agreement to pay it back. The recipient, or borrower, incurs a debt and is usually required to pay interest for the use of the money."


loanDefinition : Definition
loanDefinition =
    Definition.fromMarkdown loanRawDefinition


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


loanItemFromDom : GlossaryItemFromDom
loanItemFromDom =
    { id = "Loan"
    , preferredTerm = { isAbbreviation = False, body = "Loan" }
    , alternativeTerms = []
    , disambiguationTag = Nothing
    , normalTags = [ financeRawTag ]
    , definition = Just loanRawDefinition
    , relatedPreferredTerms = [ { isAbbreviation = False, body = "Interest rate" } ]
    , needsUpdating = False
    , lastUpdatedDateAsIso8601 = Just "2023-10-30T08:26:18.523Z"
    , lastUpdatedByName = Nothing
    , lastUpdatedByEmailAddress = Nothing
    }


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


computerScienceDescribedTagFromDom : DescribedTagFromDom
computerScienceDescribedTagFromDom =
    { id = computerScienceTagRawId
    , tag = computerScienceRawTag
    , description = computerScienceTagRawDescription
    }


computerScienceDescribedTag : DescribedTag
computerScienceDescribedTag =
    DescribedTag.create
        computerScienceTagId
        computerScienceTag
        computerScienceTagDescription


financeDescribedTagFromDom : DescribedTagFromDom
financeDescribedTagFromDom =
    { id = financeTagRawId
    , tag = financeRawTag
    , description = financeTagRawDescription
    }


financeDescribedTag : DescribedTag
financeDescribedTag =
    DescribedTag.create
        financeTagId
        financeTag
        financeTagDescription


gardeningDescribedTagFromDom : DescribedTagFromDom
gardeningDescribedTagFromDom =
    { id = gardeningTagRawId
    , tag = gardeningRawTag
    , description = gardeningTagRawDescription
    }


gardeningDescribedTag : DescribedTag
gardeningDescribedTag =
    DescribedTag.create
        gardeningTagId
        gardeningTag
        gardeningTagDescription


glossaryFromDom : GlossaryFromDom
glossaryFromDom =
    { enableLastUpdatedDates = True
    , enableExportMenu = True
    , enableOrderItemsButtons = True
    , enableHelpForMakingChanges = False
    , cardWidth = "intermediate"
    , title = "Example Glossary"
    , aboutParagraph = "An example glossary."
    , aboutLinks = []
    , tags =
        [ computerScienceDescribedTagFromDom
        , financeDescribedTagFromDom
        , gardeningDescribedTagFromDom
        ]
    , items =
        [ defaultComputerScienceItemFromDom
        , defaultFinanceItemFromDom
        , informationRetrievalItemFromDom
        , interestRateItemFromDom
        , loanItemFromDom
        ]
    , versionNumber = Just 0
    }


glossary : GlossaryForUi
glossary =
    GlossaryForUi.create
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
    GlossaryForUi.items glossary
