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
            { terms = [ termFromBody "The term one" ]
            , details = [ Details.fromPlaintext "The term one" ]
            , relatedTerms = [ RelatedTerm.fromPlaintext "Second_the_term" "Second the term" ]
            }

        two : GlossaryItem
        two =
            { terms = [ termFromBody "Second the term" ]
            , details = [ Details.fromPlaintext "Second the term" ]
            , relatedTerms = []
            }

        three : GlossaryItem
        three =
            { terms = [ termFromBody "The term three" ]
            , details = [ Details.fromPlaintext "The term three" ]
            , relatedTerms = [ RelatedTerm.fromPlaintext "Second_the_term" "Second the term" ]
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
                            [ searchResult "#Second_the_term" [ Html.text "Second the term" ]
                            , searchResult "#The_term_one" [ Html.text "The term one" ]
                            , searchResult "#The_term_three" [ Html.text "The term three" ]
                            ]

            -- , test "terms for which the search term is a prefix are ranked higher than other terms" <|
            --     \_ ->
            --         loadedGlossaryItems
            --             |> Search.search False "the"
            --             |> Expect.equal
            --                 [ searchResult "#The_term_one" [ Html.text "The term one" ]
            --                 , searchResult "#The_term_three" [ Html.text "The term three" ]
            --                 , searchResult "#Second_the_term" [ Html.text "Second the term" ]
            --                 ]
            ]
        ]
