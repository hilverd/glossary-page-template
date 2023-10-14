module SearchTests exposing (suite)

import Components.SearchDialog as SearchDialog exposing (SearchResult, searchResult)
import Data.GlossaryItem as GlossaryItem exposing (GlossaryItem)
import Data.GlossaryItem.Definition as Definition
import Data.GlossaryItem.RelatedTerm as RelatedTerm
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItem.TermId as TermId
import Data.GlossaryItemForHtml as GlossaryItemForHtml exposing (GlossaryItemForHtml)
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Data.IncubatingGlossaryItems as IncubatingGlossaryItems exposing (IncubatingGlossaryItems)
import Expect
import Search
import Test exposing (Test, describe, test)


termFromBody : String -> Term
termFromBody body =
    Term.fromPlaintext body False


loadedGlossaryItems : IncubatingGlossaryItems
loadedGlossaryItems =
    let
        one : GlossaryItemForHtml
        one =
            GlossaryItemForHtml.create
                (termFromBody "The term one")
                []
                Nothing
                []
                (Just <| Definition.fromPlaintext "The term one")
                [ Term.fromPlaintext "Second the term" False ]
                False
                Nothing

        two : GlossaryItemForHtml
        two =
            GlossaryItemForHtml.create
                (termFromBody "Second the term")
                []
                Nothing
                []
                (Just <| Definition.fromPlaintext "Second the term")
                []
                False
                Nothing

        three : GlossaryItemForHtml
        three =
            GlossaryItemForHtml.create
                (termFromBody "The term three")
                []
                Nothing
                []
                (Just (Definition.fromPlaintext "The term three"))
                [ Term.fromPlaintext "Second the term" False ]
                False
                Nothing
    in
    IncubatingGlossaryItems.fromList [] [ one, two, three ]


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
