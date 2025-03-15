module Data.ChecksumTests exposing (suite)

import Data.Checksum as Checksum exposing (Checksum)
import Data.GlossaryChange as GlossaryChange exposing (GlossaryChange)
import Data.GlossaryForUi as GlossaryForUi exposing (GlossaryForUi)
import Data.GlossaryFromDom exposing (ApplyChangesResult(..), GlossaryFromDom)
import Data.GlossaryTitle as GlossaryTitle
import Expect
import Test exposing (Test, describe, test)
import TestData exposing (glossaryFromDom)


suite : Test
suite =
    describe "The Data.Checksum module"
        [ describe "can compute checksums"
            [ describe "against a GlossaryForUi"
                [ test "where the change is ToggleEnableLastUpdatedDates" <|
                    \_ ->
                        let
                            glossary1 : GlossaryForUi
                            glossary1 =
                                { glossaryFromDom | enableLastUpdatedDates = True }
                                    |> GlossaryForUi.fromGlossaryFromDom

                            glossary2 : GlossaryForUi
                            glossary2 =
                                { glossaryFromDom | enableLastUpdatedDates = False }
                                    |> GlossaryForUi.fromGlossaryFromDom

                            change : GlossaryChange
                            change =
                                GlossaryChange.ToggleEnableLastUpdatedDates

                            checksum1 : Checksum
                            checksum1 =
                                change
                                    |> Checksum.againstGlossaryForUi glossary1

                            checksum2 : Checksum
                            checksum2 =
                                change
                                    |> Checksum.againstGlossaryForUi glossary2
                        in
                        Expect.notEqual checksum1 checksum2
                , test "where the change is SetTitle" <|
                    \_ ->
                        let
                            glossary1 : GlossaryForUi
                            glossary1 =
                                { glossaryFromDom | title = "One" }
                                    |> GlossaryForUi.fromGlossaryFromDom

                            glossary2 : GlossaryForUi
                            glossary2 =
                                { glossaryFromDom | title = "Two" }
                                    |> GlossaryForUi.fromGlossaryFromDom

                            change : GlossaryChange
                            change =
                                GlossaryChange.SetTitle (GlossaryTitle.fromMarkdown "does-not-matter")

                            checksum1 : Checksum
                            checksum1 =
                                change
                                    |> Checksum.againstGlossaryForUi glossary1

                            checksum2 : Checksum
                            checksum2 =
                                change
                                    |> Checksum.againstGlossaryForUi glossary2
                        in
                        Expect.notEqual checksum1 checksum2
                ]
            , describe "against a GlossaryFromDom"
                [ test "where the change is ToggleEnableLastUpdatedDates" <|
                    \_ ->
                        let
                            glossary1 : GlossaryFromDom
                            glossary1 =
                                { glossaryFromDom | enableLastUpdatedDates = True }

                            glossary2 : GlossaryFromDom
                            glossary2 =
                                { glossaryFromDom | enableLastUpdatedDates = False }

                            change : GlossaryChange
                            change =
                                GlossaryChange.ToggleEnableLastUpdatedDates

                            checksum1 : Checksum
                            checksum1 =
                                change
                                    |> Checksum.againstGlossaryFromDom glossary1

                            checksum2 : Checksum
                            checksum2 =
                                change
                                    |> Checksum.againstGlossaryFromDom glossary2
                        in
                        Expect.notEqual checksum1 checksum2
                ]
            ]
        ]
