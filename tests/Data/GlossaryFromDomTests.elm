module Data.GlossaryFromDomTests exposing (suite)

import Codec
import Data.DescribedTag as DescribedTag exposing (DescribedTag)
import Data.DescribedTagFromDom exposing (DescribedTagFromDom)
import Data.GlossaryChange as GlossaryChange
import Data.GlossaryChangelist as GlossaryChangelist exposing (GlossaryChangelist)
import Data.GlossaryFromDom as GlossaryFromDom exposing (ApplyChangesResult(..))
import Data.GlossaryVersionNumber as GlossaryVersionNumber
import Data.TagId as TagId exposing (TagId)
import Data.TagsChanges as TagsChanges exposing (TagsChanges)
import Expect
import Test exposing (Test, describe, test)
import TestData exposing (..)


houseworkTagId : TagId
houseworkTagId =
    TagId.create "Housework"


houseworkDescribedTag : DescribedTag
houseworkDescribedTag =
    DescribedTag.create houseworkTagId houseworkTag houseworkTagDescription


houseworkDescribedTagFromDom : DescribedTagFromDom
houseworkDescribedTagFromDom =
    { id = houseworkTagRawId
    , tag = houseworkRawTag
    , description = houseworkTagRawDescription
    }


suite : Test
suite =
    describe "The Data.GlossaryFromDom module"
        [ describe "can encode and decode"
            [ test "JSON values" <|
                \_ ->
                    TestData.glossaryFromDom
                        |> Codec.encodeToValue GlossaryFromDom.codec
                        |> Codec.decodeValue GlossaryFromDom.codec
                        |> Expect.equal (Ok TestData.glossaryFromDom)
            , test "JSON strings" <|
                \_ ->
                    TestData.glossaryFromDom
                        |> Codec.encodeToString 2 GlossaryFromDom.codec
                        |> Codec.decodeString GlossaryFromDom.codec
                        |> Expect.equal (Ok TestData.glossaryFromDom)
            ]
        , describe "can apply changes"
            [ test "that insert tags" <|
                \_ ->
                    let
                        tagsChanges : TagsChanges
                        tagsChanges =
                            TagsChanges.empty
                                |> TagsChanges.insert houseworkDescribedTag

                        changeList =
                            GlossaryChangelist.create
                                GlossaryVersionNumber.initial
                                [ GlossaryChange.ChangeTags tagsChanges ]
                    in
                    glossaryFromDom
                        |> GlossaryFromDom.applyChanges changeList
                        |> Expect.equal
                            (ChangesApplied
                                ( Nothing
                                , { glossaryFromDom
                                    | tags =
                                        [ houseworkDescribedTagFromDom
                                        , computerScienceDescribedTagFromDom
                                        , financeDescribedTagFromDom
                                        , gardeningDescribedTagFromDom
                                        ]
                                    , versionNumber =
                                        GlossaryVersionNumber.initial
                                            |> GlossaryVersionNumber.increment
                                            |> GlossaryVersionNumber.toInt
                                  }
                                )
                            )
            , test "unless the version in the changelist does not match the one in the glossary" <|
                \_ ->
                    let
                        changelist : GlossaryChangelist
                        changelist =
                            GlossaryChangelist.create (GlossaryVersionNumber.create 99)
                                []
                    in
                    Expect.equal
                        VersionsDoNotMatch
                        (GlossaryFromDom.applyChanges changelist TestData.glossaryFromDom)
            ]
        ]



{-

   TODO Test these constraints:

   * a term cannot start with "glossary-page-" (after changing to lowercase)
   * no two distinct items can have the same disambiguated preferred term (fragment) identifier
   * an item's disambiguated preferred term (fragment) identifier cannot be the same as the (fragment) identifier for an alternative term in any item
   * an item's preferred term (fragment) identifier cannot be the same as (fragment) identifier for an alternative term in any item
   * no two distinct items can share an alternative term (fragment) identifier

-}
