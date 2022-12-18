module SearchTests exposing (..)

import Components.SearchDialog exposing (searchResult)
import Data.GlossaryItem.Details as Details exposing (Details)
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Expect
import Search
import Test exposing (..)


termFromBody : String -> Term
termFromBody body =
    Term.fromPlaintext body False


loadedGlossaryItems : GlossaryItems
loadedGlossaryItems =
    let
        one =
            { terms = [ termFromBody "One" ]
            , details = [ Details.fromPlaintext "One" ]
            , relatedTerms = [ { idReference = "#Two", body = "Two" } ]
            }

        two =
            { terms = [ termFromBody "Two" ]
            , details = [ Details.fromPlaintext "Two" ]
            , relatedTerms = []
            }

        three =
            { terms = [ termFromBody "Three" ]
            , details = [ Details.fromPlaintext "Three" ]
            , relatedTerms = [ { idReference = "#Two", body = "Two" } ]
            }
    in
    GlossaryItems.fromList [ one, two, three ]


suite : Test
suite =
    describe "The Search module"
        [ describe "Search.search"
            [ test "returns search results sorted by relevance for a given search term" <|
                \_ ->
                    loadedGlossaryItems
                        |> Search.search "Two"
                        |> Expect.equal
                            [ searchResult "#Two" "Two" ]
            ]
        ]
