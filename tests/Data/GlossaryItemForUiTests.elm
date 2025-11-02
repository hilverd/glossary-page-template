module Data.GlossaryItemForUiTests exposing (suite)

import Codec
import Data.GlossaryItemForUi as GlossaryItemForUi
import Expect
import Test exposing (Test, describe, test)
import TestData


suite : Test
suite =
    describe "The Data.GlossaryItemForUi module"
        [ test "can encode and decode to and from JSON values" <|
            \_ ->
                TestData.defaultComputerScienceItem
                    |> Codec.encodeToValue GlossaryItemForUi.codec
                    |> Codec.decodeValue GlossaryItemForUi.codec
                    |> Expect.equal (Ok TestData.defaultComputerScienceItem)
        , test "can encode and decode to and from JSON strings" <|
            \_ ->
                TestData.defaultComputerScienceItem
                    |> Codec.encodeToString 2 GlossaryItemForUi.codec
                    |> Codec.decodeString GlossaryItemForUi.codec
                    |> Expect.equal (Ok TestData.defaultComputerScienceItem)
        ]
