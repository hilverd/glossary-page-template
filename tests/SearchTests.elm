module SearchTests exposing (suite)

import Data.GlossaryItem.Definition as Definition
import Data.GlossaryItem.DisambiguatedTerm as DisambiguatedTerm
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItemForUi as GlossaryItemForUi exposing (GlossaryItemForUi)
import Data.GlossaryItemId as GlossaryItemId
import Data.GlossaryItemsForUi as GlossaryItemsForUi exposing (GlossaryItemsForUi)
import Expect
import Search
import Test exposing (Test, describe, test)


termFromBody : String -> Term
termFromBody body =
    Term.fromMarkdown body False


loadedGlossaryItemsForUi : GlossaryItemsForUi
loadedGlossaryItemsForUi =
    let
        one : GlossaryItemForUi
        one =
            GlossaryItemForUi.create
                (GlossaryItemId.create "The term one")
                (termFromBody "The term one")
                []
                Nothing
                []
                (Just <| Definition.fromMarkdown "Apples")
                [ DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Second the term" False ]
                False
                Nothing
                Nothing
                Nothing

        two : GlossaryItemForUi
        two =
            GlossaryItemForUi.create
                (GlossaryItemId.create "Second the term")
                (termFromBody "Second the term")
                []
                Nothing
                []
                (Just <| Definition.fromMarkdown "Three pears")
                []
                False
                Nothing
                Nothing
                Nothing

        three : GlossaryItemForUi
        three =
            GlossaryItemForUi.create
                (GlossaryItemId.create "The term three")
                (termFromBody "The term three")
                []
                Nothing
                []
                (Just (Definition.fromMarkdown "Oranges"))
                [ DisambiguatedTerm.fromTerm <| Term.fromMarkdown "Second the term" False ]
                False
                Nothing
                Nothing
                Nothing
    in
    [ one, two, three ]
        |> GlossaryItemsForUi.fromList []
        |> Result.withDefault GlossaryItemsForUi.empty


suite : Test
suite =
    describe "The Search module"
        [ describe "Search.resultsForItems"
            [ test "returns search results sorted by relevance for a given search string" <|
                \_ ->
                    loadedGlossaryItemsForUi
                        |> Search.resultsForItems Nothing (always True) 10 "term"
                        |> (\{ totalNumberOfResults, results } ->
                                { totalNumberOfResults = totalNumberOfResults
                                , results = List.map (\{ preferredTerm } -> Term.id <| DisambiguatedTerm.toTerm preferredTerm) results
                                }
                           )
                        |> Expect.equal
                            { totalNumberOfResults = 3
                            , results =
                                [ "Second_the_term"
                                , "The_term_one"
                                , "The_term_three"
                                ]
                            }
            , test "terms for which the search string is a prefix are ranked higher than other terms" <|
                \_ ->
                    loadedGlossaryItemsForUi
                        |> Search.resultsForItems Nothing (always True) 10 "the"
                        |> (\{ totalNumberOfResults, results } ->
                                { totalNumberOfResults = totalNumberOfResults
                                , results = List.map (\{ preferredTerm } -> Term.id <| DisambiguatedTerm.toTerm preferredTerm) results
                                }
                           )
                        |> Expect.equal
                            { totalNumberOfResults = 3
                            , results =
                                [ "The_term_one"
                                , "The_term_three"
                                , "Second_the_term"
                                ]
                            }
            , test "also searches definitions, but terms take priority" <|
                \_ ->
                    loadedGlossaryItemsForUi
                        |> Search.resultsForItems Nothing (always True) 10 "three"
                        |> (\{ totalNumberOfResults, results } ->
                                { totalNumberOfResults = totalNumberOfResults
                                , results = List.map (\{ preferredTerm } -> Term.id <| DisambiguatedTerm.toTerm preferredTerm) results
                                }
                           )
                        |> Expect.equal
                            { totalNumberOfResults = 2
                            , results =
                                [ "The_term_three"
                                , "Second_the_term"
                                ]
                            }
            , test "can restrict the number of results" <|
                \_ ->
                    loadedGlossaryItemsForUi
                        |> Search.resultsForItems Nothing (always True) 1 "three"
                        |> (\{ totalNumberOfResults, results } ->
                                { totalNumberOfResults = totalNumberOfResults
                                , results = List.map (\{ preferredTerm } -> Term.id <| DisambiguatedTerm.toTerm preferredTerm) results
                                }
                           )
                        |> Expect.equal
                            { totalNumberOfResults = 2
                            , results =
                                [ "The_term_three"
                                ]
                            }
            ]
        ]
