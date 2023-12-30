module GlossaryItemForHtmlTests exposing (suite)

import Codec
import Data.GlossaryItem.Definition as Definition
import Data.GlossaryItem.Tag as Tag
import Data.GlossaryItem.Term as Term
import Data.GlossaryItemForHtml as GlossaryItemForHtml exposing (GlossaryItemForHtml)
import Expect
import Test exposing (Test, describe, test)


defaultComputerScienceItem : GlossaryItemForHtml
defaultComputerScienceItem =
    GlossaryItemForHtml.create
        (Term.fromMarkdown "Default" False)
        [ Term.fromMarkdown "Preset" False
        , Term.fromMarkdown "Factory preset" False
        ]
        (Just <| Tag.fromMarkdown "Computer Science")
        []
        (Just <| Definition.fromMarkdown "The preexisting value of a user-configurable setting that is assigned to a software application, computer program or device. Such settings are also called presets or factory presets, especially for electronic devices.")
        []
        False
        (Just "2023-09-15T19:58:59.573Z")
        Nothing
        Nothing


suite : Test
suite =
    describe "The GlossaryItemForHtml module"
        [ test "can encode and decode to and from JSON values" <|
            \_ ->
                defaultComputerScienceItem
                    |> Codec.encodeToValue GlossaryItemForHtml.codec
                    |> Codec.decodeValue GlossaryItemForHtml.codec
                    |> Expect.equal (Ok defaultComputerScienceItem)
        , test "can encode and decode to and from JSON strings" <|
            \_ ->
                defaultComputerScienceItem
                    |> Codec.encodeToString 2 GlossaryItemForHtml.codec
                    |> Codec.decodeString GlossaryItemForHtml.codec
                    |> Expect.equal (Ok defaultComputerScienceItem)
        ]
