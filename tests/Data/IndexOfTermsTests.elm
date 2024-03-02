module Data.IndexOfTermsTests exposing (suite)

import Data.GlossaryItem.Definition as Definition
import Data.GlossaryItem.DisambiguatedTerm as DisambiguatedTerm
import Data.GlossaryItem.Term as Term exposing (Term)
import Data.GlossaryItemForHtml as GlossaryItemForHtml exposing (GlossaryItemForHtml)
import Data.GlossaryItemId as GlossaryItemId
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Data.IndexOfTerms as IndexOfTerms
import Expect
import Test exposing (Test, describe, test)


termFromBody : String -> Term
termFromBody body =
    Term.fromMarkdown body False


glossaryItemForHtml : String -> GlossaryItemForHtml
glossaryItemForHtml body =
    GlossaryItemForHtml.create
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


glossaryItems : GlossaryItems
glossaryItems =
    [ glossaryItemForHtml "007"
    , glossaryItemForHtml "Óne"
    , glossaryItemForHtml "Two"
    , glossaryItemForHtml "3040"
    , glossaryItemForHtml "3Three"
    , glossaryItemForHtml "Ω"
    , glossaryItemForHtml "_future_"
    ]
        |> GlossaryItems.fromList []
        |> Result.withDefault GlossaryItems.empty


suite : Test
suite =
    describe "The Data.IndexOfTerms module"
        [ test "sorts terms alphabetically by their first alphabetic character (stripped of any diacritical marks)" <|
            \_ ->
                let
                    preferredTerm body =
                        IndexOfTerms.PreferredTerm (GlossaryItemId.create -1) <| DisambiguatedTerm.fromTerm <| termFromBody body
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
                    |> GlossaryItems.fromList []
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
