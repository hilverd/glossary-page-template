module Data.GlossaryItem.RelatedTerm exposing
    ( RelatedTerm, fromPlaintext, fromMarkdown, decode, raw, idReference, markdown, view
    , htmlTreeForAnki
    )

{-| A related term.


# Related Terms

@docs RelatedTerm, fromPlaintext, fromMarkdown, decode, raw, idReference, markdown, view, htmlTree

-}

import Data.MarkdownFragment as MarkdownFragment exposing (MarkdownFragment)
import Extras.HtmlTree exposing (HtmlTree)
import Extras.String
import Html exposing (Html, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode exposing (Decoder)
import Markdown.Block as Block exposing (Block)
import Markdown.Html
import Markdown.Renderer as Renderer exposing (Renderer)
import MarkdownRenderers
import Regex
import String.Normalize
import Svg.Attributes exposing (from)


{-| A related term.
-}
type RelatedTerm
    = PlaintextRelatedTerm
        { idReference : String
        , body : String
        }
    | MarkdownRelatedTerm
        { idReference : String
        , body : MarkdownFragment
        }


{-| Construct a related term from plain text.

    fromPlaintext "Device_Edge" "Device Edge"
    |> raw --> "Device Edge"

    fromPlaintext "Device_Edge" "Device Edge"
    |> idReference --> "Device_Edge"

-}
fromPlaintext : String -> String -> RelatedTerm
fromPlaintext idReference0 body =
    PlaintextRelatedTerm
        { idReference = idReference0
        , body = body
        }


{-| Construct a related term from a Markdown string.

    fromMarkdown "Device_Edge" "Device _Edge_"
    |> raw --> "Device _Edge_"

    fromMarkdown "Device_Edge" "Device _Edge_"
    |> idReference --> "Device_Edge"

-}
fromMarkdown : String -> String -> RelatedTerm
fromMarkdown idReference0 body =
    let
        fragment =
            MarkdownFragment.fromString body
    in
    MarkdownRelatedTerm
        { idReference = idReference0
        , body = fragment
        }


{-| Decode a related term from its JSON representation.

    import Json.Decode as Decode exposing (Decoder)
    import Json.Encode as Encode

    deviceEdge : Encode.Value
    deviceEdge =
        Encode.object
            [ ( "idReference", Encode.string "Device_Edge" )
            , ( "body", Encode.string "Device Edge" )
            ]

    decoded : Result Decode.Error RelatedTerm
    decoded = Decode.decodeValue (decode False) deviceEdge

    Result.map idReference decoded --> Ok "Device_Edge"

    Result.map raw decoded --> Ok "Device Edge"

-}
decode : Bool -> Decoder RelatedTerm
decode enableMarkdownBasedSyntax =
    (if enableMarkdownBasedSyntax then
        Decode.map2 fromMarkdown

     else
        Decode.map2 fromPlaintext
    )
        (Decode.field "idReference" Decode.string)
        (Decode.field "body" Decode.string)


{-| Retrieve the ID reference of a related term.
-}
idReference : RelatedTerm -> String
idReference relatedTerm =
    case relatedTerm of
        PlaintextRelatedTerm t ->
            t.idReference

        MarkdownRelatedTerm t ->
            t.idReference


{-| Retrieve the raw body of a related term.
-}
raw : RelatedTerm -> String
raw relatedTerm =
    case relatedTerm of
        PlaintextRelatedTerm t ->
            t.body

        MarkdownRelatedTerm t ->
            MarkdownFragment.raw t.body


{-| Convert a related term to a string suitable for a Markdown document.
-}
markdown : RelatedTerm -> String
markdown relatedTerm =
    case relatedTerm of
        PlaintextRelatedTerm t ->
            Extras.String.escapeForMarkdown t.body

        MarkdownRelatedTerm t ->
            MarkdownFragment.raw t.body


{-| View a related term as HTML.
-}
view : Bool -> RelatedTerm -> Html msg
view enableMathSupport relatedTerm =
    case relatedTerm of
        PlaintextRelatedTerm t ->
            text t.body

        MarkdownRelatedTerm t ->
            let
                parsed : Result String (List Block)
                parsed =
                    MarkdownFragment.parsed t.body
            in
            case parsed of
                Ok blocks ->
                    case Renderer.render (MarkdownRenderers.inlineHtmlMsgRenderer { enableMathSupport = enableMathSupport }) blocks of
                        Ok rendered ->
                            Html.span
                                [ class "prose print:prose-neutral dark:prose-invert dark:prose-pre:text-gray-200 prose-code:before:hidden prose-code:after:hidden leading-normal" ]
                                rendered

                        Err renderingError ->
                            text <| "Failed to render Markdown: " ++ renderingError

                Err parsingError ->
                    text <| "Failed to parse Markdown: " ++ parsingError


{-| Convert a related term to an HtmlTree for Anki.
-}
htmlTreeForAnki : Bool -> RelatedTerm -> HtmlTree
htmlTreeForAnki enableMathSupport relatedTerm =
    case relatedTerm of
        PlaintextRelatedTerm t ->
            Extras.HtmlTree.Leaf t.body

        MarkdownRelatedTerm t ->
            case MarkdownFragment.parsed t.body of
                Ok blocks ->
                    case blocks |> Renderer.render (MarkdownRenderers.inlineHtmlTreeRendererForAnki enableMathSupport) of
                        Ok rendered ->
                            Extras.HtmlTree.Node "span" False [] rendered

                        Err renderingError ->
                            Extras.HtmlTree.Leaf <| "Failed to render Markdown: " ++ renderingError

                Err parsingError ->
                    Extras.HtmlTree.Leaf <| "Failed to parse Markdown: " ++ parsingError
