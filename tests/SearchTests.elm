module SearchTests exposing (suite)

import Components.SearchDialog exposing (searchResult)
import Data.GlossaryItem exposing (GlossaryItem)
import Data.GlossaryItem.Details as Details
import Data.GlossaryItem.RelatedTerm as RelatedTerm
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Expect
import Html
import Search
import Test exposing (Test, describe, test)


termFromBody : String -> Term
termFromBody body =
    Term.fromPlaintext body False


loadedGlossaryItems : GlossaryItems
loadedGlossaryItems =
    let
        one : GlossaryItem
        one =
            { terms = [ termFromBody "One" ]
            , details = [ Details.fromPlaintext "One" ]
            , relatedTerms = [ RelatedTerm.fromPlaintext "#Two" "Two" ]
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
            , relatedTerms = [ RelatedTerm.fromPlaintext "#Two" "Two" ]
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
                        |> Search.search False "Two"
                        |> Expect.equal
                            [ searchResult "#Two" [ Html.text "Two" ] ]
            ]
        ]
