module SearchTests exposing (suite)

import Components.SearchDialog as SearchDialog exposing (SearchResult, searchResult)
import Data.GlossaryItem as GlossaryItem exposing (GlossaryItem)
import Data.GlossaryItem.Definition as Definition
import Data.GlossaryItem.RelatedTerm as RelatedTerm
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItem.TermId as TermId
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Expect
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
            GlossaryItem.init
                (Just (termFromBody "The term one"))
                []
                [ Definition.fromPlaintext "The term one" ]
                [ RelatedTerm.fromPlaintext (TermId.fromString "Second_the_term") "Second the term" ]
                False
                Nothing

        two : GlossaryItem
        two =
            GlossaryItem.init
                (Just (termFromBody "Second the term"))
                []
                [ Definition.fromPlaintext "Second the term" ]
                []
                False
                Nothing

        three : GlossaryItem
        three =
            GlossaryItem.init
                (Just (termFromBody "The term three"))
                []
                [ Definition.fromPlaintext "The term three" ]
                [ RelatedTerm.fromPlaintext (TermId.fromString "Second_the_term") "Second the term" ]
                False
                Nothing
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
