module SearchTests exposing (..)

import Components.SearchDialog exposing (searchResult)
import Data.GlossaryItem as GlossaryItem
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Expect
import Search
import Test exposing (..)


termFromBody : String -> GlossaryItem.Term
termFromBody body =
    { id = String.replace " " "_" body
    , isAbbreviation = False
    , body = body
    }


loadedGlossaryItems : GlossaryItems
loadedGlossaryItems =
    let
        one =
            { terms = [ termFromBody "One" ]
            , details = [ "One" ]
            , relatedTerms = [ { idReference = "#Two", body = "Two" } ]
            }

        two =
            { terms = [ termFromBody "Two" ]
            , details = [ "Two" ]
            , relatedTerms = []
            }

        three =
            { terms = [ termFromBody "Three" ]
            , details = [ "Three" ]
            , relatedTerms = [ { idReference = "#Two", body = "Two" } ]
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
                        |> Search.search "Two"
                        |> Expect.equal
                            [ searchResult "#Two" "Two" ]
            ]
        ]
