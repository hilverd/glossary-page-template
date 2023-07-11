module SearchTests exposing (suite)

import Components.SearchDialog as SearchDialog exposing (SearchResult, searchResult)
import Data.GlossaryItem exposing (GlossaryItem)
import Data.GlossaryItem.Definition as Definition
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
            , definitions = [ Definition.fromPlaintext "The term one" ]
            , relatedTerms = [ RelatedTerm.fromPlaintext "Second_the_term" "Second the term" ]
            , needsUpdating = False
            , lastUpdatedDate = Nothing
            }

        two : GlossaryItem
        two =
            { terms = [ termFromBody "Second the term" ]
            , definitions = [ Definition.fromPlaintext "Second the term" ]
            , relatedTerms = []
            , needsUpdating = False
            , lastUpdatedDate = Nothing
            }

        three : GlossaryItem
        three =
            { terms = [ termFromBody "The term three" ]
            , definitions = [ Definition.fromPlaintext "The term three" ]
            , relatedTerms = [ RelatedTerm.fromPlaintext "Second_the_term" "Second the term" ]
            , needsUpdating = False
            , lastUpdatedDate = Nothing
            }
    in
    GlossaryItems.fromList [ one, two, three ]


suite : Test
suite =
    describe "The Search module"
        [ describe "Search.search"
            [ test "returns search results sorted by relevance for a given search string" <|
                \_ ->
                    loadedGlossaryItems
                        |> Search.search False "term"
                        |> List.map SearchDialog.searchResultHref
                        |> Expect.equal
                            [ "#Second_the_term"
                            , "#The_term_one"
                            , "#The_term_three"
                            ]
            , test "terms for which the search string is a prefix are ranked higher than other terms" <|
                \_ ->
                    loadedGlossaryItems
                        |> Search.search False "the"
                        |> List.map SearchDialog.searchResultHref
                        |> Expect.equal
                            [ "#The_term_one"
                            , "#The_term_three"
                            , "#Second_the_term"
                            ]
            ]
        ]
