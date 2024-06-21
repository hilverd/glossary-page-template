module Data.GlossaryFromDomTests exposing (suite)

import Codec
import Data.DescribedTag as DescribedTag exposing (DescribedTag)
import Data.DescribedTagFromDom exposing (DescribedTagFromDom)
import Data.GlossaryChange as GlossaryChange
import Data.GlossaryChangelist as GlossaryChangelist exposing (GlossaryChangelist)
import Data.GlossaryFromDom as GlossaryFromDom exposing (ApplyChangesResult(..))
import Data.GlossaryItem.TermFromDom as TermFromDom
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
            [ test "unless any term starts with 'glossary-page-' (after changing to lowercase)" <|
                \_ ->
                    let
                        changeList =
                            GlossaryChangelist.create
                                GlossaryVersionNumber.initial
                                [ GlossaryChange.Insert
                                    { loanItemFromDom
                                        | id = "some-id"
                                        , preferredTerm = TermFromDom.create False "Glossary-Page-Foo"
                                    }
                                ]
                    in
                    glossaryFromDom
                        |> GlossaryFromDom.applyChanges changeList
                        |> Expect.equal
                            (LogicalErrorWhenApplyingChanges "This term is reserved: Glossary-Page-Foo")
            , test "that insert tags" <|
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
                                        [ computerScienceDescribedTagFromDom
                                        , financeDescribedTagFromDom
                                        , gardeningDescribedTagFromDom
                                        , houseworkDescribedTagFromDom
                                        ]
                                    , versionNumber =
                                        GlossaryVersionNumber.initial
                                            |> GlossaryVersionNumber.increment
                                            |> GlossaryVersionNumber.toInt
                                  }
                                )
                            )
            , test "that update tags" <|
                \_ ->
                    let
                        tagsChanges : TagsChanges
                        tagsChanges =
                            TagsChanges.empty
                                |> TagsChanges.update computerScienceTagId financeDescribedTag
                                |> TagsChanges.update financeTagId computerScienceDescribedTag

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
                                        [ { computerScienceDescribedTagFromDom
                                            | tag = financeRawTag
                                            , description = financeTagRawDescription
                                          }
                                        , { financeDescribedTagFromDom
                                            | tag = computerScienceRawTag
                                            , description = computerScienceTagRawDescription
                                          }
                                        , gardeningDescribedTagFromDom
                                        ]
                                    , items =
                                        [ { defaultComputerScienceItemFromDom
                                            | disambiguationTag = Just financeRawTag
                                          }
                                        , { defaultFinanceItemFromDom
                                            | disambiguationTag = Just computerScienceRawTag
                                          }
                                        , { informationRetrievalItemFromDom
                                            | normalTags = [ financeRawTag ]
                                          }
                                        , { interestRateItemFromDom
                                            | normalTags = [ computerScienceRawTag ]
                                          }
                                        , { loanItemFromDom
                                            | normalTags = [ computerScienceRawTag ]
                                          }
                                        ]
                                    , versionNumber =
                                        GlossaryVersionNumber.initial
                                            |> GlossaryVersionNumber.increment
                                            |> GlossaryVersionNumber.toInt
                                  }
                                )
                            )
            , test "that remove tags" <|
                \_ ->
                    let
                        tagsChanges : TagsChanges
                        tagsChanges =
                            TagsChanges.empty
                                |> TagsChanges.remove computerScienceTagId

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
                                        [ financeDescribedTagFromDom
                                        , gardeningDescribedTagFromDom
                                        ]
                                    , items =
                                        [ { defaultComputerScienceItemFromDom
                                            | disambiguationTag = Nothing
                                          }
                                        , defaultFinanceItemFromDom
                                        , { informationRetrievalItemFromDom
                                            | normalTags = []
                                          }
                                        , interestRateItemFromDom
                                        , loanItemFromDom
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
            , test "unless the tag updates result in multiple items with the same (disambiguated) preferred term identifier" <|
                \_ ->
                    let
                        tagsChanges : TagsChanges
                        tagsChanges =
                            TagsChanges.empty
                                |> TagsChanges.update computerScienceTagId financeDescribedTag

                        changeList =
                            GlossaryChangelist.create
                                GlossaryVersionNumber.initial
                                [ GlossaryChange.ChangeTags tagsChanges ]
                    in
                    glossaryFromDom
                        |> GlossaryFromDom.applyChanges changeList
                        |> Expect.equal
                            (LogicalErrorWhenApplyingChanges "There are multiple items with the same (disambiguated) preferred term identifier \"Default_(Finance)\"")
            ]
        ]



{-

   TODO Test these constraints:

   * an item's preferred term cannot be the same as an alternative term in any item
   * an item cannot have two identical alternative terms
-}
