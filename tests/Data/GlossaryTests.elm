module Data.GlossaryTests exposing (suite)

import Codec
import Data.Glossary as Glossary
import Expect
import Test exposing (Test, describe, test)
import TestData


suite : Test
suite =
    describe "The Data.Glossary module"
        [ test "can encode and decode to and from JSON values" <|
            \_ ->
                TestData.glossary
                    |> Codec.encodeToValue Glossary.codec
                    |> Codec.decodeValue Glossary.codec
                    |> Expect.equal (Ok TestData.glossary)
        , test "can encode and decode to and from JSON strings" <|
            \_ ->
                TestData.glossary
                    |> Codec.encodeToString 2 Glossary.codec
                    |> Codec.decodeString Glossary.codec
                    |> Expect.equal (Ok TestData.glossary)
        ]
