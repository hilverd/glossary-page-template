module IndexOfTermsTests exposing (suite)

import Data.GlossaryItem exposing (GlossaryItem)
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
            { terms = [ termFromBody "Óne" ]
            , definitions = [ Definition.fromPlaintext "Óne" ]
            , relatedTerms = []
            , needsUpdating = False
            , lastUpdatedDate = Nothing
            }

        two : GlossaryItem
        two =
            { terms = [ termFromBody "Two" ]
            , definitions = [ Definition.fromPlaintext "Two" ]
            , relatedTerms = []
            , needsUpdating = False
            , lastUpdatedDate = Nothing
            }

        thirtyFourty : GlossaryItem
        thirtyFourty =
            { terms = [ termFromBody "3040" ]
            , definitions = [ Definition.fromPlaintext "3040" ]
            , relatedTerms = []
            , needsUpdating = False
            , lastUpdatedDate = Nothing
            }

        three : GlossaryItem
        three =
            { terms = [ termFromBody "3Three" ]
            , definitions = [ Definition.fromPlaintext "3Three" ]
            , relatedTerms = []
            , needsUpdating = False
            , lastUpdatedDate = Nothing
            }

        doubleOhSeven : GlossaryItem
        doubleOhSeven =
            { terms = [ termFromBody "007" ]
            , definitions = [ Definition.fromPlaintext "007" ]
            , relatedTerms = []
            , needsUpdating = False
            , lastUpdatedDate = Nothing
            }

        omega : GlossaryItem
        omega =
            { terms = [ termFromBody "Ω" ]
            , definitions = [ Definition.fromPlaintext "Ω" ]
            , relatedTerms = []
            , needsUpdating = False
            , lastUpdatedDate = Nothing
            }

        future : GlossaryItem
        future =
            { terms = [ termFromBody "_future_" ]
            , definitions = [ Definition.fromPlaintext "_future_" ]
            , relatedTerms = []
            , needsUpdating = False
            , lastUpdatedDate = Nothing
            }
    in
    GlossaryItems.fromList [ doubleOhSeven, one, two, thirtyFourty, three, omega, future ]


suite : Test
suite =
    describe "The IndexOfTerms module"
        [ test "sorts terms alphabetically by their first alphabetic character (stripped of any diacritical marks)" <|
            \_ ->
                glossaryItems
                    |> IndexOfTerms.fromGlossaryItems
                    |> IndexOfTerms.termGroups
                    |> Expect.equal
                        [ { label = "0–9", terms = [ termFromBody "007", termFromBody "3040", termFromBody "3Three" ] }
                        , { label = "A", terms = [] }
                        , { label = "B", terms = [] }
                        , { label = "C", terms = [] }
                        , { label = "D", terms = [] }
                        , { label = "E", terms = [] }
                        , { label = "F", terms = [ termFromBody "_future_" ] }
                        , { label = "G", terms = [] }
                        , { label = "H", terms = [] }
                        , { label = "I", terms = [] }
                        , { label = "J", terms = [] }
                        , { label = "K", terms = [] }
                        , { label = "L", terms = [] }
                        , { label = "M", terms = [] }
                        , { label = "N", terms = [] }
                        , { label = "O", terms = [ termFromBody "Óne" ] }
                        , { label = "P", terms = [] }
                        , { label = "Q", terms = [] }
                        , { label = "R", terms = [] }
                        , { label = "S", terms = [] }
                        , { label = "T", terms = [ termFromBody "Two" ] }
                        , { label = "U", terms = [] }
                        , { label = "V", terms = [] }
                        , { label = "W", terms = [] }
                        , { label = "X", terms = [] }
                        , { label = "Y", terms = [] }
                        , { label = "Z", terms = [] }
                        , { label = "…", terms = [ termFromBody "Ω" ] }
                        ]
        , test "doesn't include ellipsis if not needed" <|
            \_ ->
                []
                    |> GlossaryItems.fromList
                    |> IndexOfTerms.fromGlossaryItems
                    |> IndexOfTerms.termGroups
                    |> Expect.equal
                        [ { label = "A", terms = [] }
                        , { label = "B", terms = [] }
                        , { label = "C", terms = [] }
                        , { label = "D", terms = [] }
                        , { label = "E", terms = [] }
                        , { label = "F", terms = [] }
                        , { label = "G", terms = [] }
                        , { label = "H", terms = [] }
                        , { label = "I", terms = [] }
                        , { label = "J", terms = [] }
                        , { label = "K", terms = [] }
                        , { label = "L", terms = [] }
                        , { label = "M", terms = [] }
                        , { label = "N", terms = [] }
                        , { label = "O", terms = [] }
                        , { label = "P", terms = [] }
                        , { label = "Q", terms = [] }
                        , { label = "R", terms = [] }
                        , { label = "S", terms = [] }
                        , { label = "T", terms = [] }
                        , { label = "U", terms = [] }
                        , { label = "V", terms = [] }
                        , { label = "W", terms = [] }
                        , { label = "X", terms = [] }
                        , { label = "Y", terms = [] }
                        , { label = "Z", terms = [] }
                        ]
        ]
