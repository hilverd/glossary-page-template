module Data.GlossaryFromDomTests exposing (suite)

import Codec
import Data.GlossaryFromDom as GlossaryFromDom
import Expect
import Test exposing (Test, describe, test)
import TestData


suite : Test
suite =
    describe "The Data.GlossaryFromDom module"
        [ test "can encode and decode JSON values" <|
            \_ ->
                TestData.glossaryFromDom
                    |> Codec.encodeToValue GlossaryFromDom.codec
                    |> Codec.decodeValue GlossaryFromDom.codec
                    |> Expect.equal (Ok TestData.glossaryFromDom)
        , test "can encode and decode JSON strings" <|
            \_ ->
                TestData.glossaryFromDom
                    |> Codec.encodeToString 2 GlossaryFromDom.codec
                    |> Codec.decodeString GlossaryFromDom.codec
                    |> Expect.equal (Ok TestData.glossaryFromDom)
        ]
