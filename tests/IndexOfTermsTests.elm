module IndexOfTermsTests exposing (suite)

import Data.GlossaryItem as GlossaryItem exposing (GlossaryItem)
import Data.GlossaryItem.Definition as Definition
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Data.IndexOfTerms as IndexOfTerms
import Expect
import Test exposing (Test, describe, test)


termFromBody : String -> Term
termFromBody body =
    Term.fromPlaintext body False


glossaryItems : GlossaryItems
glossaryItems =
    let
        one : GlossaryItem
        one =
            GlossaryItem.init
                (termFromBody "Óne")
                []
                (Just (Definition.fromPlaintext "Óne"))
                []
                False
                Nothing

        two : GlossaryItem
        two =
            GlossaryItem.init
                (termFromBody "Two")
                []
                (Just (Definition.fromPlaintext "Two"))
                []
                False
                Nothing

        three : GlossaryItem
        three =
            GlossaryItem.init
                (termFromBody "3Three")
                []
                (Just (Definition.fromPlaintext "3Three"))
                []
                False
                Nothing

        doubleOhSeven : GlossaryItem
        doubleOhSeven =
            GlossaryItem.init
                (termFromBody "007")
                []
                (Just (Definition.fromPlaintext "007"))
                []
                False
                Nothing

        future : GlossaryItem
        future =
            GlossaryItem.init
                (termFromBody "_future_")
                []
                (Just (Definition.fromPlaintext "_future_"))
                []
                False
                Nothing
    in
    GlossaryItems.fromList [ doubleOhSeven, one, two, three, future ]


suite : Test
suite =
    describe "The IndexOfTerms module"
        [ test "sorts terms alphabetically by their first alphabetic character (stripped of any diacritical marks)" <|
            \_ ->
                glossaryItems
                    |> IndexOfTerms.fromGlossaryItems
                    |> IndexOfTerms.termGroups
                    |> Expect.equal
                        [ { label = "A", entries = [] }
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
                        , { label = "T"
                          , entries =
                                [ IndexOfTerms.PreferredTerm <| termFromBody "3Three"
                                , IndexOfTerms.PreferredTerm <| termFromBody "Two"
                                ]
                          }
                        , { label = "U", entries = [] }
                        , { label = "V", entries = [] }
                        , { label = "W", entries = [] }
                        , { label = "X", entries = [] }
                        , { label = "Y", entries = [] }
                        , { label = "Z", entries = [] }
                        , { label = "…", entries = [ IndexOfTerms.PreferredTerm <| termFromBody "007" ] }
                        ]
        , test "doesn't include ellipsis if not needed" <|
            \_ ->
                []
                    |> GlossaryItems.fromList
                    |> IndexOfTerms.fromGlossaryItems
                    |> IndexOfTerms.termGroups
                    |> Expect.equal
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
        ]
