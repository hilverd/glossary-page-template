module ExportMarkdownTests exposing (suite)

import Data.AboutParagraph as AboutParagraph
import Data.CardWidth as CardWidth
import Data.Glossary as Glossary exposing (Glossary)
import Data.GlossaryItem.Definition as Definition exposing (Definition)
import Data.GlossaryItem.Tag as Tag exposing (Tag)
import Data.GlossaryItem.Term as Term
import Data.GlossaryItemForHtml as GlossaryItemForHtml exposing (GlossaryItemForHtml)
import Data.GlossaryTitle as GlossaryTitle
import Data.GlossaryVersionNumber as GlossaryVersionNumber
import Data.TagDescription as TagDescription exposing (TagDescription)
import Expect
import Export.Markdown
import Test exposing (Test, describe, test)


computerScienceTag : Tag
computerScienceTag =
    Tag.fromMarkdown "Computer Science"


computerScienceTagDescription : TagDescription
computerScienceTagDescription =
    TagDescription.fromMarkdown "These are items about computer science."


financeTag : Tag
financeTag =
    Tag.fromMarkdown "Finance"


financeTagDescription : TagDescription
financeTagDescription =
    TagDescription.fromMarkdown "These are items about finance."


defaultComputerScienceDefinition : Definition
defaultComputerScienceDefinition =
    Definition.fromMarkdown "The preexisting value of a user-configurable setting."


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
        Nothing
        Nothing


interestRateDefinition : Definition
interestRateDefinition =
    Definition.fromMarkdown "The amount of interest due per period."


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
        Nothing
        Nothing


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
        [ ( computerScienceTag, computerScienceTagDescription )
        , ( financeTag, financeTagDescription )
        ]
        [ defaultComputerScienceItem
        , interestRateItem
        ]
        GlossaryVersionNumber.initial


crlf : String
crlf =
    "\u{000D}\n"


lines : List String -> String
lines =
    String.join crlf


suite : Test
suite =
    describe "The Export.Markdown module"
        [ test "can format a glossary as Markdown" <|
            \_ ->
                let
                    expected =
                        [ "# Example Glossary"
                        , ""
                        , "An example glossary."
                        , ""
                        , "---------"
                        , ""
                        , "**Default (Computer Science)**\\"
                        , "**Preset**\\"
                        , "**Factory preset**"
                        , ""
                        , "[Tags: Computer Science]"
                        , ""
                        , "The preexisting value of a user-configurable setting."
                        , ""
                        , "**Interest rate**\\"
                        , "**IR**"
                        , ""
                        , "[Tags: Finance]"
                        , ""
                        , "The amount of interest due per period."
                        ]
                            |> lines
                in
                glossary
                    |> Export.Markdown.toString
                    |> Expect.equal expected
        ]
