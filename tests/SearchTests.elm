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
            { terms = [ termFromBody "First term" ]
            , details = [ Details.fromPlaintext "First term" ]
            , relatedTerms = [ RelatedTerm.fromPlaintext "Second_term" "Second term" ]
            }

        two : GlossaryItem
        two =
            { terms = [ termFromBody "Second term" ]
            , details = [ Details.fromPlaintext "Second term" ]
            , relatedTerms = []
            }

        three : GlossaryItem
        three =
            { terms = [ termFromBody "Third term" ]
            , details = [ Details.fromPlaintext "Third term" ]
            , relatedTerms = [ RelatedTerm.fromPlaintext "Second_term" "Second term" ]
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
                        |> Search.search False "term"
                        |> Expect.equal
                            [ searchResult "#Second_term" [ Html.text "Second term" ]
                            , searchResult "#First_term" [ Html.text "First term" ]
                            , searchResult "#Third_term" [ Html.text "Third term" ]
                            ]
            ]
        ]
