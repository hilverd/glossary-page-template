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
        , describe "isItemForTag"
            [ test "returns True when disambiguated preferred term matches a normal tag" <|
                \_ ->
                    TestData.financeItemForTag
                        |> GlossaryItemForUi.isItemForTag
                        |> Expect.equal True
            , test "returns False when item has disambiguation tag (disambiguated term won't match)" <|
                \_ ->
                    TestData.financeItemWithDisambiguationTag
                        |> GlossaryItemForUi.isItemForTag
                        |> Expect.equal False
            , test "returns False when disambiguated preferred term does not match any tag" <|
                \_ ->
                    TestData.defaultComputerScienceItem
                        |> GlossaryItemForUi.isItemForTag
                        |> Expect.equal False
            , test "returns False when item has no tags" <|
                \_ ->
                    TestData.itemWithNoTags
                        |> GlossaryItemForUi.isItemForTag
                        |> Expect.equal False
            ]
        ]
