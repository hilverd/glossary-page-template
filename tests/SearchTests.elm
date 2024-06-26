module SearchTests exposing (suite)

import Components.SearchDialog as SearchDialog
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
        [ describe "Search.search"
            [ test "returns search results sorted by relevance for a given search string" <|
                \_ ->
                    loadedGlossaryItemsForUi
                        |> Search.search False Nothing "term"
                        |> List.map SearchDialog.searchResultHref
                        |> Expect.equal
                            [ "#Second_the_term"
                            , "#The_term_one"
                            , "#The_term_three"
                            ]
            , test "terms for which the search string is a prefix are ranked higher than other terms" <|
                \_ ->
                    loadedGlossaryItemsForUi
                        |> Search.search False Nothing "the"
                        |> List.map SearchDialog.searchResultHref
                        |> Expect.equal
                            [ "#The_term_one"
                            , "#The_term_three"
                            , "#Second_the_term"
                            ]
            , test "also searches definitions, but terms take priority" <|
                \_ ->
                    loadedGlossaryItemsForUi
                        |> Search.search False Nothing "three"
                        |> List.map SearchDialog.searchResultHref
                        |> Expect.equal
                            [ "#The_term_three"
                            , "#Second_the_term"
                            ]
            ]
        ]
