module GlossaryItemsTests exposing (suite)

import Data.GlossaryItem.Definition as Definition exposing (Definition)
import Data.GlossaryItem.Tag as Tag exposing (Tag)
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItemForHtml as GlossaryItemForHtml exposing (GlossaryItemForHtml)
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Data.IndexOfTerms as IndexOfTerms
import Data.TagDescription as TagDescription exposing (TagDescription)
import Expect
import Test exposing (Test, describe, test)


computerScienceTag : Tag
computerScienceTag =
    Tag.fromMarkdown "Computer Science"


computerScienceTagDescription : TagDescription
computerScienceTagDescription =
    TagDescription.fromMarkdown "These are items about computer science — the study of computation, information, and automation."


defaultDefinition : Definition
defaultDefinition =
    Definition.fromMarkdown "The preexisting value of a user-configurable setting that is assigned to a software application, computer program or device. Such settings are also called presets or factory presets, especially for electronic devices."


financeTag : Tag
financeTag =
    Tag.fromMarkdown "Finance"


financeTagDescription : TagDescription
financeTagDescription =
    TagDescription.fromMarkdown "These are items about finance — the study and discipline of money, currency and capital assets."


defaultItem : GlossaryItemForHtml
defaultItem =
    GlossaryItemForHtml.create
        (Term.fromMarkdown "Default" False)
        [ Term.fromMarkdown "Preset" False
        , Term.fromMarkdown "Factory preset" False
        ]
        (Just computerScienceTag)
        []
        (Just defaultDefinition)
        []
        False
        (Just "2023-09-15T19:58:59.573Z")


glossaryItems : GlossaryItems
glossaryItems =
    GlossaryItems.fromList
        [ ( computerScienceTag, computerScienceTagDescription )
        , ( financeTag, financeTagDescription )
        ]
        [ defaultItem
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
