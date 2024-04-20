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



{-

   TODO Test these constraints:

   * a term cannot start with "glossary-page-" (after changing to lowercase)
   * no two distinct items can have the same disambiguated preferred term (fragment) identifier
   * an item's disambiguated preferred term (fragment) identifier cannot be the same as the (fragment) identifier for an alternative term in any item
   * an item's preferred term (fragment) identifier cannot be the same as (fragment) identifier for an alternative term in any item
   * no two distinct items can share an alternative term (fragment) identifier

-}
