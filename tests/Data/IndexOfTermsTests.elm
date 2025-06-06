module Data.IndexOfTermsTests exposing (suite)

import Data.GlossaryItem.Definition as Definition
import Data.GlossaryItem.DisambiguatedTerm as DisambiguatedTerm
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItemForUi as GlossaryItemForUi exposing (GlossaryItemForUi)
import Data.GlossaryItemId as GlossaryItemId
import Data.GlossaryItemsForUi as GlossaryItemsForUi exposing (GlossaryItemsForUi)
import Data.IndexOfTerms as IndexOfTerms
import Expect
import Test exposing (Test, describe, test)


termFromBody : String -> Term
termFromBody body =
    Term.fromMarkdown body False


glossaryItemForUi : String -> GlossaryItemForUi
glossaryItemForUi body =
    GlossaryItemForUi.create
        (GlossaryItemId.create body)
        (termFromBody body)
        []
        Nothing
        []
        (Just (Definition.fromMarkdown body))
        []
        False
        Nothing
        Nothing
        Nothing


glossaryItems : GlossaryItemsForUi
glossaryItems =
    [ glossaryItemForUi "007"
    , glossaryItemForUi "Óne"
    , glossaryItemForUi "Two"
    , glossaryItemForUi "3040"
    , glossaryItemForUi "3Three"
    , glossaryItemForUi "Ω"
    , glossaryItemForUi "_future_"
    ]
        |> GlossaryItemsForUi.fromList [] Nothing
        |> Result.withDefault GlossaryItemsForUi.empty


suite : Test
suite =
    describe "The Data.IndexOfTerms module"
        [ test "sorts terms alphabetically by their first alphabetic character (stripped of any diacritical marks)" <|
            \_ ->
                let
                    preferredTerm : String -> IndexOfTerms.Entry
                    preferredTerm body =
                        IndexOfTerms.PreferredTerm (GlossaryItemId.create body) <| DisambiguatedTerm.fromTerm <| termFromBody body
                in
                glossaryItems
                    |> IndexOfTerms.fromGlossaryItems Nothing
                    |> IndexOfTerms.termGroups
                    |> Expect.equal
                        [ { label = "0–9"
                          , entries =
                                [ preferredTerm "007"
                                , preferredTerm "3040"
                                , preferredTerm "3Three"
                                ]
                          }
                        , { label = "A", entries = [] }
                        , { label = "B", entries = [] }
                        , { label = "C", entries = [] }
                        , { label = "D", entries = [] }
                        , { label = "E", entries = [] }
                        , { label = "F", entries = [ preferredTerm "_future_" ] }
                        , { label = "G", entries = [] }
                        , { label = "H", entries = [] }
                        , { label = "I", entries = [] }
                        , { label = "J", entries = [] }
                        , { label = "K", entries = [] }
                        , { label = "L", entries = [] }
                        , { label = "M", entries = [] }
                        , { label = "N", entries = [] }
                        , { label = "O", entries = [ preferredTerm "Óne" ] }
                        , { label = "P", entries = [] }
                        , { label = "Q", entries = [] }
                        , { label = "R", entries = [] }
                        , { label = "S", entries = [] }
                        , { label = "T", entries = [ preferredTerm "Two" ] }
                        , { label = "U", entries = [] }
                        , { label = "V", entries = [] }
                        , { label = "W", entries = [] }
                        , { label = "X", entries = [] }
                        , { label = "Y", entries = [] }
                        , { label = "Z", entries = [] }
                        , { label = "…", entries = [ preferredTerm "Ω" ] }
                        ]
        , test "doesn't include 0–9 and ellipsis if not needed" <|
            \_ ->
                []
                    |> GlossaryItemsForUi.fromList [] Nothing
                    |> Result.map (IndexOfTerms.fromGlossaryItems Nothing)
                    |> Result.map IndexOfTerms.termGroups
                    |> Expect.equal
                        (Ok
                            [ { label = "A", entries = [] }
                            , { label = "B", entries = [] }
                            , { label = "C", entries = [] }
                            , { label = "D", entries = [] }
                            , { label = "E", entries = [] }
                            , { label = "F", entries = [] }
                            , { label = "G", entries = [] }
                            , { label = "H", entries = [] }
                            , { label = "I", entries = [] }
                            , { label = "J", entries = [] }
                            , { label = "K", entries = [] }
                            , { label = "L", entries = [] }
                            , { label = "M", entries = [] }
                            , { label = "N", entries = [] }
                            , { label = "O", entries = [] }
                            , { label = "P", entries = [] }
                            , { label = "Q", entries = [] }
                            , { label = "R", entries = [] }
                            , { label = "S", entries = [] }
                            , { label = "T", entries = [] }
                            , { label = "U", entries = [] }
                            , { label = "V", entries = [] }
                            , { label = "W", entries = [] }
                            , { label = "X", entries = [] }
                            , { label = "Y", entries = [] }
                            , { label = "Z", entries = [] }
                            ]
                        )
        ]
