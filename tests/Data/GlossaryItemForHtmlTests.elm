module Data.GlossaryItemForHtmlTests exposing (suite)

import Codec
import Data.GlossaryItemForHtml as GlossaryItemForHtml
import Expect
import Test exposing (Test, describe, test)
import TestData


suite : Test
suite =
    describe "The Data.GlossaryItemForHtml module"
        [ test "can encode and decode to and from JSON values" <|
            \_ ->
                TestData.defaultComputerScienceItem
                    |> Codec.encodeToValue GlossaryItemForHtml.codec
                    |> Codec.decodeValue GlossaryItemForHtml.codec
                    |> Expect.equal (Ok TestData.defaultComputerScienceItem)
        , test "can encode and decode to and from JSON strings" <|
            \_ ->
                TestData.defaultComputerScienceItem
                    |> Codec.encodeToString 2 GlossaryItemForHtml.codec
                    |> Codec.decodeString GlossaryItemForHtml.codec
                    |> Expect.equal (Ok TestData.defaultComputerScienceItem)
        ]
