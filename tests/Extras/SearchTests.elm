module Extras.SearchTests exposing (..)

import Components.SearchDialog exposing (searchResult)
import Data.GlossaryItem as GlossaryItem
import Data.GlossaryItems as GlossaryItems
import Data.LoadedGlossaryItems exposing (LoadedGlossaryItems)
import Expect
import Search
import Test exposing (..)


termFromBody : String -> GlossaryItem.Term
termFromBody body =
    { id = String.replace " " "_" body
    , isAbbreviation = False
    , body = body
    }


loadedGlossaryItems : LoadedGlossaryItems
loadedGlossaryItems =
    let
        actOfParliament =
            { terms = [ termFromBody "Acts of Parliament" ]
            , details = [ "An Act of Parliament (also called a statute) is a law made by the UK Parliament. All Acts start as bills introduced in either the Commons or the Lords. When a bill has been agreed by both Houses of Parliament and has been given Royal Assent by the Monarch, it becomes an Act. Acts are known as ‘primary legislation’ because they do not depend on other legislative authority." ]
            , relatedTerms = []
            }
    in
    GlossaryItems.fromList [ actOfParliament ]
        |> Result.Ok


suite : Test
suite =
    describe "The Search module"
        [ describe "Search.search"
            [ test "returns search results sorted by relevance for a given search term" <|
                \_ ->
                    loadedGlossaryItems
                        |> Search.search "par"
                        |> Expect.equal [ searchResult "#Acts_of_Parliament" "Acts of Parliament" ]
            ]
        ]
