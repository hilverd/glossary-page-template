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
            { terms = [ termFromBody "One" ]
            , details = [ Details.fromPlaintext "One" ]
            , relatedTerms = []
            }

        two : GlossaryItem
        two =
            { terms = [ termFromBody "Two" ]
            , details = [ Details.fromPlaintext "Two" ]
            , relatedTerms = []
            }

        three : GlossaryItem
        three =
            { terms = [ termFromBody "Three" ]
            , details = [ Details.fromPlaintext "Three" ]
            , relatedTerms = []
            }

        future : GlossaryItem
        future =
            { terms = [ termFromBody "_future_" ]
            , details = [ Details.fromPlaintext "_future_" ]
            , relatedTerms = []
            }
    in
    GlossaryItems.fromList [ one, two, three, future ]


suite : Test
suite =
    describe "The IndexOfTerms module"
        [ test "sorts terms alphabetically by their first character" <|
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
                        , { label = "F", terms = [] }
                        , { label = "G", terms = [] }
                        , { label = "H", terms = [] }
                        , { label = "I", terms = [] }
                        , { label = "J", terms = [] }
                        , { label = "K", terms = [] }
                        , { label = "L", terms = [] }
                        , { label = "M", terms = [] }
                        , { label = "N", terms = [] }
                        , { label = "O", terms = [ termFromBody "One" ] }
                        , { label = "P", terms = [] }
                        , { label = "Q", terms = [] }
                        , { label = "R", terms = [] }
                        , { label = "S", terms = [] }
                        , { label = "T", terms = [ termFromBody "Three", termFromBody "Two" ] }
                        , { label = "U", terms = [] }
                        , { label = "V", terms = [] }
                        , { label = "W", terms = [] }
                        , { label = "X", terms = [] }
                        , { label = "Y", terms = [] }
                        , { label = "Z", terms = [] }
                        , { label = "_", terms = [ termFromBody "_future_" ] }
                        ]
        ]
