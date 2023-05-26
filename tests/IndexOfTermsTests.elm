module IndexOfTermsTests exposing (suite)

import Data.GlossaryItem exposing (GlossaryItem)
import Data.GlossaryItem.Details as Details
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
            , details = [ Details.fromPlaintext "Óne" ]
            , relatedTerms = []
            , needsUpdating = False
            , lastUpdatedDate = Nothing
            }

        two : GlossaryItem
        two =
            { terms = [ termFromBody "Two" ]
            , details = [ Details.fromPlaintext "Two" ]
            , relatedTerms = []
            , needsUpdating = False
            , lastUpdatedDate = Nothing
            }

        three : GlossaryItem
        three =
            { terms = [ termFromBody "3Three" ]
            , details = [ Details.fromPlaintext "3Three" ]
            , relatedTerms = []
            , needsUpdating = False
            , lastUpdatedDate = Nothing
            }

        doubleOhSeven : GlossaryItem
        doubleOhSeven =
            { terms = [ termFromBody "007" ]
            , details = [ Details.fromPlaintext "007" ]
            , relatedTerms = []
            , needsUpdating = False
            , lastUpdatedDate = Nothing
            }

        future : GlossaryItem
        future =
            { terms = [ termFromBody "_future_" ]
            , details = [ Details.fromPlaintext "_future_" ]
            , relatedTerms = []
            , needsUpdating = False
            , lastUpdatedDate = Nothing
            }
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
                        [ { label = "A", terms = [] }
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
                        , { label = "T", terms = [ termFromBody "3Three", termFromBody "Two" ] }
                        , { label = "U", terms = [] }
                        , { label = "V", terms = [] }
                        , { label = "W", terms = [] }
                        , { label = "X", terms = [] }
                        , { label = "Y", terms = [] }
                        , { label = "Z", terms = [] }
                        , { label = "…", terms = [ termFromBody "007" ] }
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
