module MarkdownFragmentTests exposing (suite)

import Data.MarkdownFragment as MarkdownFragment
import Expect
import Markdown.Block as Block
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "The MarkdownFragment module"
        [ describe "MarkdownFragment.inlineFoldl"
            [ test "folds over the inline elements of a Markdown fragment" <|
                \_ ->
                    "*Hello* _there_"
                        |> MarkdownFragment.fromString
                        |> MarkdownFragment.inlineFoldl
                            (\inline result ->
                                case inline of
                                    Block.CodeSpan str ->
                                        str :: result

                                    Block.Text str ->
                                        str :: result

                                    _ ->
                                        result
                            )
                            []
                        |> Expect.equal (Ok [ "there", " ", "Hello" ])
            ]
        ]
