module Data.GlossaryFromDomTests exposing (suite)

import Codec
import Data.Checksum as Checksum exposing (Checksum)
import Data.DescribedTag as DescribedTag exposing (DescribedTag)
import Data.DescribedTagFromDom exposing (DescribedTagFromDom)
import Data.GlossaryChange as GlossaryChange exposing (GlossaryChange)
import Data.GlossaryChangeWithChecksum exposing (GlossaryChangeWithChecksum)
import Data.GlossaryChangelist as GlossaryChangelist exposing (GlossaryChangelist)
import Data.GlossaryFromDom as GlossaryFromDom exposing (ApplyChangesResult(..))
import Data.GlossaryItem.TermFromDom as TermFromDom
import Data.GlossaryItemForUi as GlossaryItemForUi
import Data.GlossaryItemFromDom exposing (GlossaryItemFromDom)
import Data.GlossaryVersionNumber as GlossaryVersionNumber
import Data.TagId as TagId exposing (TagId)
import Data.TagsChanges as TagsChanges exposing (TagsChanges)
import Expect
import Test exposing (Test, describe, test)
import TestData
    exposing
        ( computerScienceDescribedTag
        , computerScienceDescribedTagFromDom
        , computerScienceRawTag
        , computerScienceTagId
        , computerScienceTagRawDescription
        , defaultComputerScienceItemFromDom
        , defaultFinanceItemFromDom
        , financeDescribedTag
        , financeDescribedTagFromDom
        , financeRawTag
        , financeTagId
        , financeTagRawDescription
        , gardeningDescribedTagFromDom
        , glossaryFromDom
        , houseworkRawTag
        , houseworkTag
        , houseworkTagDescription
        , houseworkTagRawDescription
        , houseworkTagRawId
        , informationRetrievalItemFromDom
        , interestRateItemFromDom
        , loanItem
        , loanItemFromDom
        , spadeItem
        , spadeItemFromDom
        )


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


glossaryChangeWithMeaninglessChecksum : GlossaryChange -> GlossaryChangeWithChecksum
glossaryChangeWithMeaninglessChecksum glossaryChange =
    { glossaryChange = glossaryChange
    , checksum = Checksum.create "whatever"
    }


glossaryChangeWithChecksum : Checksum -> GlossaryChange -> GlossaryChangeWithChecksum
glossaryChangeWithChecksum checksum glossaryChange =
    { glossaryChange = glossaryChange
    , checksum = checksum
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
            [ test "that insert items" <|
                \_ ->
                    let
                        changeList : GlossaryChangelist
                        changeList =
                            GlossaryChangelist.create
                                GlossaryVersionNumber.initial
                                [ GlossaryChange.Insert TestData.spadeItemFromDom
                                    |> glossaryChangeWithMeaninglessChecksum
                                ]
                    in
                    glossaryFromDom
                        |> GlossaryFromDom.applyChanges changeList
                        |> Expect.equal
                            (ChangesApplied
                                ( Just <| GlossaryItemForUi.id spadeItem
                                , { glossaryFromDom
                                    | tags =
                                        [ computerScienceDescribedTagFromDom
                                        , financeDescribedTagFromDom
                                        , gardeningDescribedTagFromDom
                                        ]
                                    , items =
                                        [ defaultComputerScienceItemFromDom
                                        , defaultFinanceItemFromDom
                                        , informationRetrievalItemFromDom
                                        , interestRateItemFromDom
                                        , loanItemFromDom
                                        , spadeItemFromDom
                                        ]
                                    , versionNumber =
                                        GlossaryVersionNumber.initial
                                            |> GlossaryVersionNumber.increment
                                            |> GlossaryVersionNumber.toInt
                                  }
                                )
                            )
            , test "that update items" <|
                \_ ->
                    let
                        loanRenamedToAdvance : GlossaryItemFromDom
                        loanRenamedToAdvance =
                            { loanItemFromDom
                                | preferredTerm =
                                    { isAbbreviation = False, body = "Advance" }
                            }

                        defaultFinanceNowPointingToAdvance : GlossaryItemFromDom
                        defaultFinanceNowPointingToAdvance =
                            { defaultFinanceItemFromDom
                                | relatedPreferredTerms = [ { isAbbreviation = False, body = "Advance" } ]
                            }

                        interestRateNowPointingToAdvance : GlossaryItemFromDom
                        interestRateNowPointingToAdvance =
                            { interestRateItemFromDom
                                | relatedPreferredTerms = [ { isAbbreviation = False, body = "Advance" } ]
                            }

                        change : GlossaryChange
                        change =
                            GlossaryChange.Update loanRenamedToAdvance

                        changeList : GlossaryChangelist
                        changeList =
                            GlossaryChangelist.create (GlossaryVersionNumber.create 99)
                                [ change
                                    |> glossaryChangeWithChecksum (GlossaryFromDom.checksumForChange glossaryFromDom change)
                                ]
                    in
                    glossaryFromDom
                        |> GlossaryFromDom.applyChanges changeList
                        |> Expect.equal
                            (ChangesApplied
                                ( Just <| GlossaryItemForUi.id loanItem
                                , { glossaryFromDom
                                    | tags =
                                        [ computerScienceDescribedTagFromDom
                                        , financeDescribedTagFromDom
                                        , gardeningDescribedTagFromDom
                                        ]
                                    , items =
                                        [ loanRenamedToAdvance
                                        , defaultComputerScienceItemFromDom
                                        , defaultFinanceNowPointingToAdvance
                                        , informationRetrievalItemFromDom
                                        , interestRateNowPointingToAdvance
                                        ]
                                    , versionNumber =
                                        GlossaryVersionNumber.initial
                                            |> GlossaryVersionNumber.increment
                                            |> GlossaryVersionNumber.toInt
                                  }
                                )
                            )
            , test "that remove items" <|
                \_ ->
                    let
                        changeList : GlossaryChangelist
                        changeList =
                            GlossaryChangelist.create
                                GlossaryVersionNumber.initial
                                [ GlossaryChange.Remove (GlossaryItemForUi.id TestData.loanItem)
                                    |> glossaryChangeWithMeaninglessChecksum
                                ]
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
                                        ]
                                    , items =
                                        [ defaultComputerScienceItemFromDom
                                        , defaultFinanceItemFromDom
                                        , informationRetrievalItemFromDom
                                        , interestRateItemFromDom
                                        ]
                                    , versionNumber =
                                        GlossaryVersionNumber.initial
                                            |> GlossaryVersionNumber.increment
                                            |> GlossaryVersionNumber.toInt
                                  }
                                )
                            )
            , test "that insert tags" <|
                \_ ->
                    let
                        tagsChanges : TagsChanges
                        tagsChanges =
                            TagsChanges.empty
                                |> TagsChanges.insert houseworkDescribedTag

                        changeList : GlossaryChangelist
                        changeList =
                            GlossaryChangelist.create
                                GlossaryVersionNumber.initial
                                [ GlossaryChange.ChangeTags tagsChanges
                                    |> glossaryChangeWithMeaninglessChecksum
                                ]
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

                        changeList : GlossaryChangelist
                        changeList =
                            GlossaryChangelist.create
                                GlossaryVersionNumber.initial
                                [ GlossaryChange.ChangeTags tagsChanges
                                    |> glossaryChangeWithMeaninglessChecksum
                                ]
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
                                        [ { defaultFinanceItemFromDom
                                            | disambiguationTag = Just computerScienceRawTag
                                          }
                                        , { defaultComputerScienceItemFromDom
                                            | disambiguationTag = Just financeRawTag
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

                        changeList : GlossaryChangelist
                        changeList =
                            GlossaryChangelist.create
                                GlossaryVersionNumber.initial
                                [ GlossaryChange.ChangeTags tagsChanges
                                    |> glossaryChangeWithMeaninglessChecksum
                                ]
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
            , test "unless any term starts with 'glossary-page-' (after changing to lowercase)" <|
                \_ ->
                    let
                        changeList : GlossaryChangelist
                        changeList =
                            GlossaryChangelist.create
                                GlossaryVersionNumber.initial
                                [ GlossaryChange.Insert
                                    { loanItemFromDom
                                        | id = "some-id"
                                        , preferredTerm = TermFromDom.create False "Glossary-Page-Foo"
                                    }
                                    |> glossaryChangeWithMeaninglessChecksum
                                ]
                    in
                    glossaryFromDom
                        |> GlossaryFromDom.applyChanges changeList
                        |> Expect.equal
                            (LogicalErrorWhenApplyingChanges "This term is reserved: Glossary-Page-Foo")
            , test "unless an item's preferred term is the same as an alternative term in any item" <|
                \_ ->
                    let
                        changeList : GlossaryChangelist
                        changeList =
                            GlossaryChangelist.create
                                GlossaryVersionNumber.initial
                                [ GlossaryChange.Insert
                                    { loanItemFromDom
                                        | id = "some-id"
                                        , preferredTerm = TermFromDom.create False "Foo"
                                        , alternativeTerms = [ TermFromDom.create False "Loan" ]
                                    }
                                    |> glossaryChangeWithMeaninglessChecksum
                                ]
                    in
                    glossaryFromDom
                        |> GlossaryFromDom.applyChanges changeList
                        |> Expect.equal
                            (LogicalErrorWhenApplyingChanges "A preferred term cannot also appear as an alternative term: \"Loan\"")
            , test "unless an item has two identical alternative terms" <|
                \_ ->
                    let
                        changeList : GlossaryChangelist
                        changeList =
                            GlossaryChangelist.create
                                GlossaryVersionNumber.initial
                                [ GlossaryChange.Insert
                                    { loanItemFromDom
                                        | id = "some-id"
                                        , preferredTerm = TermFromDom.create False "Foo"
                                        , alternativeTerms =
                                            [ TermFromDom.create False "Bar"
                                            , TermFromDom.create False "Bar"
                                            ]
                                    }
                                    |> glossaryChangeWithMeaninglessChecksum
                                ]
                    in
                    glossaryFromDom
                        |> GlossaryFromDom.applyChanges changeList
                        |> Expect.equal
                            (LogicalErrorWhenApplyingChanges "The alternative term \"Bar\" occurs multiple times in the item with preferred term \"Foo\".")
            , test "unless the version in the changelist does not match the one in the glossary and the checksums also do not match" <|
                \_ ->
                    let
                        changelist : GlossaryChangelist
                        changelist =
                            GlossaryChangelist.create (GlossaryVersionNumber.create 99)
                                [ TagsChanges.empty
                                    |> TagsChanges.update computerScienceTagId financeDescribedTag
                                    |> GlossaryChange.ChangeTags
                                    |> glossaryChangeWithChecksum (Checksum.create "different")
                                ]
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

                        changeList : GlossaryChangelist
                        changeList =
                            GlossaryChangelist.create
                                GlossaryVersionNumber.initial
                                [ GlossaryChange.ChangeTags tagsChanges
                                    |> glossaryChangeWithMeaninglessChecksum
                                ]
                    in
                    glossaryFromDom
                        |> GlossaryFromDom.applyChanges changeList
                        |> Expect.equal
                            (LogicalErrorWhenApplyingChanges "There are multiple items with the same (disambiguated) preferred term identifier \"Default_(Finance)\"")
            ]
        ]
