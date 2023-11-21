module IndexOfTermsTests exposing (suite)

import Data.GlossaryItem.Definition as Definition
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItemForHtml as GlossaryItemForHtml exposing (GlossaryItemForHtml)
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Data.IndexOfTerms as IndexOfTerms
import Expect
import Test exposing (Test, describe, test)


termFromBody : String -> Term
termFromBody body =
    Term.fromMarkdown body False


glossaryItems : GlossaryItems
glossaryItems =
    let
        one : GlossaryItemForHtml
        one =
            GlossaryItemForHtml.create
                (termFromBody "Óne")
                []
                Nothing
                []
                (Just (Definition.fromMarkdown "Óne"))
                []
                False
                Nothing

        two : GlossaryItemForHtml
        two =
            GlossaryItemForHtml.create
                (termFromBody "Two")
                []
                Nothing
                []
                (Just (Definition.fromMarkdown "Two"))
                []
                False
                Nothing

        thirtyFourty : GlossaryItemForHtml
        thirtyFourty =
            GlossaryItemForHtml.create
                (termFromBody "3040")
                []
                Nothing
                []
                (Just (Definition.fromMarkdown "3040"))
                []
                False
                Nothing

        three : GlossaryItemForHtml
        three =
            GlossaryItemForHtml.create
                (termFromBody "3Three")
                []
                Nothing
                []
                (Just (Definition.fromMarkdown "3Three"))
                []
                False
                Nothing

        doubleOhSeven : GlossaryItemForHtml
        doubleOhSeven =
            GlossaryItemForHtml.create
                (termFromBody "007")
                []
                Nothing
                []
                (Just (Definition.fromMarkdown "007"))
                []
                False
                Nothing

        omega : GlossaryItemForHtml
        omega =
            GlossaryItemForHtml.create
                (termFromBody "Ω")
                []
                Nothing
                []
                (Just (Definition.fromMarkdown "Ω"))
                []
                False
                Nothing

        future : GlossaryItemForHtml
        future =
            GlossaryItemForHtml.create
                (termFromBody "_future_")
                []
                Nothing
                []
                (Just (Definition.fromMarkdown "_future_"))
                []
                False
                Nothing
    in
    [ doubleOhSeven, one, two, thirtyFourty, three, omega, future ]
        |> GlossaryItems.fromList []
        |> Result.withDefault GlossaryItems.empty


suite : Test
suite =
    describe "The IndexOfTerms module"
        [ test "sorts terms alphabetically by their first alphabetic character (stripped of any diacritical marks)" <|
            \_ ->
                glossaryItems
                    |> IndexOfTerms.fromGlossaryItems Nothing
                    |> IndexOfTerms.termGroups
                    |> Expect.equal
                        [ { label = "0–9"
                          , entries =
                                [ IndexOfTerms.PreferredTerm <| termFromBody "007"
                                , IndexOfTerms.PreferredTerm <| termFromBody "3040"
                                , IndexOfTerms.PreferredTerm <| termFromBody "3Three"
                                ]
                          }
                        , { label = "A", entries = [] }
                        , { label = "B", entries = [] }
                        , { label = "C", entries = [] }
                        , { label = "D", entries = [] }
                        , { label = "E", entries = [] }
                        , { label = "F", entries = [ IndexOfTerms.PreferredTerm <| termFromBody "_future_" ] }
                        , { label = "G", entries = [] }
                        , { label = "H", entries = [] }
                        , { label = "I", entries = [] }
                        , { label = "J", entries = [] }
                        , { label = "K", entries = [] }
                        , { label = "L", entries = [] }
                        , { label = "M", entries = [] }
                        , { label = "N", entries = [] }
                        , { label = "O", entries = [ IndexOfTerms.PreferredTerm <| termFromBody "Óne" ] }
                        , { label = "P", entries = [] }
                        , { label = "Q", entries = [] }
                        , { label = "R", entries = [] }
                        , { label = "S", entries = [] }
                        , { label = "T", entries = [ IndexOfTerms.PreferredTerm <| termFromBody "Two" ] }
                        , { label = "U", entries = [] }
                        , { label = "V", entries = [] }
                        , { label = "W", entries = [] }
                        , { label = "X", entries = [] }
                        , { label = "Y", entries = [] }
                        , { label = "Z", entries = [] }
                        , { label = "…", entries = [ IndexOfTerms.PreferredTerm <| termFromBody "Ω" ] }
                        ]
        , test "doesn't include 0–9 and ellipsis if not needed" <|
            \_ ->
                []
                    |> GlossaryItems.fromList []
                    |> Result.map (IndexOfTerms.fromGlossaryItems Nothing)
                    |> Result.map IndexOfTerms.termGroups
                    |> Expect.equal
                        (Ok
                            [ { label = "A", entries = [] }
                            , { label = "B", entries = [] }
                            , { label = "C", entries = [] }
                            , { label = "D", entries = [] }
                            , { label = "E", entries = [] }
                            , { label = "F", entries = [] }
                            , { label = "G", entries = [] }
                            , { label = "H", entries = [] }
                            , { label = "I", entries = [] }
                            , { label = "J", entries = [] }
                            , { label = "K", entries = [] }
                            , { label = "L", entries = [] }
                            , { label = "M", entries = [] }
                            , { label = "N", entries = [] }
                            , { label = "O", entries = [] }
                            , { label = "P", entries = [] }
                            , { label = "Q", entries = [] }
                            , { label = "R", entries = [] }
                            , { label = "S", entries = [] }
                            , { label = "T", entries = [] }
                            , { label = "U", entries = [] }
                            , { label = "V", entries = [] }
                            , { label = "W", entries = [] }
                            , { label = "X", entries = [] }
                            , { label = "Y", entries = [] }
                            , { label = "Z", entries = [] }
                            ]
                        )
        ]
