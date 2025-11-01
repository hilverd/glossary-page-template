module Data.GlossaryItemForUiTests exposing (suite)

import Codec
import Data.AboutParagraph as AboutParagraph
import Data.CardWidth as CardWidth
import Data.GlossaryForUi as GlossaryForUi
import Data.GlossaryItem.DisambiguatedTerm as DisambiguatedTerm
import Data.GlossaryItem.Term as Term
import Data.GlossaryItemForUi as GlossaryItemForUi
import Data.GlossaryTitle as GlossaryTitle
import Data.GlossaryVersionNumber as GlossaryVersionNumber
import Data.Theme as Theme
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
        , describe "GlossaryForUi.title"
            [ test "returns the glossary title when three-column layout is not enabled" <|
                \_ ->
                    let
                        glossaryForUi =
                            GlossaryForUi.create
                                True
                                True
                                True
                                Nothing
                                False
                                CardWidth.Intermediate
                                Theme.System
                                (GlossaryTitle.fromMarkdown "My Glossary Title")
                                (AboutParagraph.fromMarkdown "About this glossary")
                                []
                                []
                                []
                                GlossaryVersionNumber.initial
                    in
                    glossaryForUi
                        |> GlossaryForUi.title
                        |> GlossaryTitle.raw
                        |> Expect.equal "My Glossary Title"
            , test "returns the starting item's disambiguated preferred term when three-column layout is enabled" <|
                \_ ->
                    let
                        -- The starting item term needs to match one of the actual items
                        -- defaultComputerScienceItem has term "Default" with disambiguation tag "Computer Science"
                        startingItemTerm =
                            DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Default (Computer Science)" False

                        glossaryForUi =
                            GlossaryForUi.create
                                True
                                True
                                True
                                (Just startingItemTerm)
                                False
                                CardWidth.Intermediate
                                Theme.System
                                (GlossaryTitle.fromMarkdown "My Glossary Title")
                                (AboutParagraph.fromMarkdown "About this glossary")
                                []
                                [ TestData.computerScienceDescribedTag ]
                                [ TestData.defaultComputerScienceItem ]
                                GlossaryVersionNumber.initial
                    in
                    glossaryForUi
                        |> GlossaryForUi.title
                        |> GlossaryTitle.raw
                        |> Expect.equal "Default (Computer Science)"
            ]
        ]
