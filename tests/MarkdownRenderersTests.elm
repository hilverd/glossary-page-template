module MarkdownRenderersTests exposing (suite)

import Data.MarkdownFragment as MarkdownFragment
import Expect
import Html
import Html.Attributes as Attr exposing (class)
import Markdown.Renderer as Renderer
import MarkdownRenderers
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "The MarkdownRenderers module"
        [ describe "MarkdownRenderers.htmlMsgRenderer without math support"
            [ test "renders a Markdown fragment to HTML" <|
                \_ ->
                    """```math
e = mc^2
```"""
                        |> MarkdownFragment.fromString
                        |> MarkdownFragment.parsed
                        |> Result.andThen
                            (Renderer.render
                                (MarkdownRenderers.htmlMsgRenderer
                                    { enableMathSupport = False
                                    , makeLinksTabbable = True
                                    }
                                )
                            )
                        |> Expect.equal
                            (Ok
                                [ Html.pre
                                    []
                                    [ Html.code
                                        [ class "language-math" ]
                                        [ Html.text "e = mc^2\n" ]
                                    ]
                                ]
                            )
            ]
        , describe "MarkdownRenderers.htmlMsgRenderer with math support"
            [ test "renders a Markdown fragment with display math to HTML" <|
                \_ ->
                    """```math
e = mc^2
```"""
                        |> MarkdownFragment.fromString
                        |> MarkdownFragment.parsed
                        |> Result.andThen
                            (Renderer.render
                                (MarkdownRenderers.htmlMsgRenderer
                                    { enableMathSupport = True
                                    , makeLinksTabbable = True
                                    }
                                )
                            )
                        |> Expect.equal
                            (Ok
                                [ Html.node "katex-display"
                                    [ Attr.attribute "data-expr" "e = mc^2\n" ]
                                    []
                                ]
                            )
            , test "renders a Markdown fragment with inline math to HTML" <|
                \_ ->
                    "`$e = mc^2$`"
                        |> MarkdownFragment.fromString
                        |> MarkdownFragment.parsed
                        |> Result.andThen
                            (Renderer.render
                                (MarkdownRenderers.htmlMsgRenderer
                                    { enableMathSupport = True
                                    , makeLinksTabbable = True
                                    }
                                )
                            )
                        |> Expect.equal
                            (Ok
                                [ Html.p
                                    [ class "max-w-prose print:max-w-full" ]
                                    [ Html.node "katex-inline"
                                        [ Attr.attribute "data-expr" "e = mc^2" ]
                                        []
                                    ]
                                ]
                            )
            ]
        , describe "MarkdownRenderers.inlineHtmlMsgRenderer with math support"
            [ test "renders a Markdown fragment with inline math to HTML" <|
                \_ ->
                    "`$e = mc^2$`"
                        |> MarkdownFragment.fromString
                        |> MarkdownFragment.parsed
                        |> Result.andThen (Renderer.render (MarkdownRenderers.inlineHtmlMsgRenderer True))
                        |> Expect.equal
                            (Ok
                                [ Html.span
                                    [ class "mr-1" ]
                                    [ Html.node "katex-inline"
                                        [ Attr.attribute "data-expr" "e = mc^2" ]
                                        []
                                    ]
                                ]
                            )
            ]
        ]
