module Data.ChecksumTests exposing (suite)

import Data.Checksum as Checksum exposing (Checksum)
import Data.GlossaryChange as GlossaryChange exposing (GlossaryChange)
import Data.GlossaryForUi as GlossaryForUi exposing (GlossaryForUi)
import Data.GlossaryFromDom exposing (ApplyChangesResult(..), GlossaryFromDom)
import Data.GlossaryTitle as GlossaryTitle
import Expect
import Test exposing (Test, concat, describe, test)
import TestData exposing (glossaryFromDom, loanItemFromDom)


suite : Test
suite =
    describe "The Data.Checksum module"
        [ describe "can compute checksums"
            [ test "where the change is ToggleEnableLastUpdatedDates" <|
                \_ ->
                    let
                        glossaryInFrontEnd : GlossaryForUi
                        glossaryInFrontEnd =
                            { glossaryFromDom | enableLastUpdatedDates = True }
                                |> GlossaryForUi.fromGlossaryFromDom

                        glossaryInBackEnd : GlossaryFromDom
                        glossaryInBackEnd =
                            { glossaryFromDom | enableLastUpdatedDates = False }

                        change : GlossaryChange
                        change =
                            GlossaryChange.ToggleEnableLastUpdatedDates

                        checksumSentAlongWithChange : Checksum
                        checksumSentAlongWithChange =
                            Checksum.againstGlossaryForUi glossaryInFrontEnd change

                        checksumForGlossaryInBackEnd : Checksum
                        checksumForGlossaryInBackEnd =
                            Checksum.againstGlossaryFromDom glossaryInBackEnd change
                    in
                    Expect.notEqual checksumSentAlongWithChange checksumForGlossaryInBackEnd
            , let
                glossaryInFrontEnd : GlossaryForUi
                glossaryInFrontEnd =
                    GlossaryForUi.fromGlossaryFromDom { glossaryFromDom | title = "One" }

                change : GlossaryChange
                change =
                    GlossaryChange.SetTitle <| GlossaryTitle.fromMarkdown "does-not-matter"

                checksumSentAlongWithChange : Checksum
                checksumSentAlongWithChange =
                    Checksum.againstGlossaryForUi glossaryInFrontEnd change
              in
              concat
                [ test "where the change is SetTitle and the checksums do not match" <|
                    \_ ->
                        Expect.notEqual checksumSentAlongWithChange
                            (Checksum.againstGlossaryFromDom { glossaryFromDom | title = "Two" } change)
                , test "where the change is SetTitle and the checksums match" <|
                    \_ ->
                        Expect.equal checksumSentAlongWithChange
                            (Checksum.againstGlossaryFromDom { glossaryFromDom | title = "One" } change)
                ]
            , test "where the change is Update but no other changes have been made to the item" <|
                \_ ->
                    let
                        glossaryInFrontEnd : GlossaryForUi
                        glossaryInFrontEnd =
                            { glossaryFromDom
                                | items =
                                    [ TestData.interestRateItemFromDom
                                    , loanItemFromDom
                                    ]
                            }
                                |> GlossaryForUi.fromGlossaryFromDom

                        glossaryInBackEnd : GlossaryFromDom
                        glossaryInBackEnd =
                            { glossaryFromDom
                                | items =
                                    [ TestData.defaultComputerScienceItemFromDom
                                    , TestData.interestRateItemFromDom
                                    , loanItemFromDom
                                    ]
                            }

                        change : GlossaryChange
                        change =
                            GlossaryChange.Update loanItemFromDom

                        checksumSentAlongWithChange : Checksum
                        checksumSentAlongWithChange =
                            Checksum.againstGlossaryForUi glossaryInFrontEnd change

                        checksumForGlossaryInBackEnd : Checksum
                        checksumForGlossaryInBackEnd =
                            Checksum.againstGlossaryFromDom glossaryInBackEnd change
                    in
                    Expect.equal checksumSentAlongWithChange checksumForGlossaryInBackEnd
            , test "where the change is Update" <|
                \_ ->
                    let
                        glossaryInFrontEnd : GlossaryForUi
                        glossaryInFrontEnd =
                            { glossaryFromDom
                                | items =
                                    [ TestData.defaultComputerScienceItemFromDom
                                    , loanItemFromDom
                                    ]
                            }
                                |> GlossaryForUi.fromGlossaryFromDom

                        glossaryInBackEnd : GlossaryFromDom
                        glossaryInBackEnd =
                            { glossaryFromDom
                                | items =
                                    [ TestData.defaultComputerScienceItemFromDom
                                    , { loanItemFromDom
                                        | preferredTerm =
                                            { isAbbreviation = False, body = "Advance" }
                                      }
                                    ]
                            }

                        change : GlossaryChange
                        change =
                            GlossaryChange.Update loanItemFromDom

                        checksumSentAlongWithChange : Checksum
                        checksumSentAlongWithChange =
                            Checksum.againstGlossaryForUi glossaryInFrontEnd change

                        checksumForGlossaryInBackEnd : Checksum
                        checksumForGlossaryInBackEnd =
                            Checksum.againstGlossaryFromDom glossaryInBackEnd change
                    in
                    Expect.notEqual checksumSentAlongWithChange checksumForGlossaryInBackEnd
            ]
        ]
