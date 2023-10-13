module Data.GlossaryItem.RelatedTerm exposing (RelatedTerm, fromPlaintext, fromMarkdown, decode, raw, idReference, markdown, view, htmlTreeForAnki)

{-| A related term.


# Related Terms

@docs RelatedTerm, fromPlaintext, fromMarkdown, decode, raw, idReference, markdown, view, htmlTreeForAnki

-}

import Data.GlossaryItem.TermId as TermId exposing (TermId)
import Data.MarkdownFragment as MarkdownFragment exposing (MarkdownFragment)
import Extras.HtmlTree exposing (HtmlTree)
import Extras.String
import Html exposing (Html, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode exposing (Decoder)
import Markdown.Block exposing (Block)
import Markdown.Renderer as Renderer
import MarkdownRenderers


{-| A related term.
-}
type RelatedTerm
    = PlaintextRelatedTerm
        { idReference : TermId
        , body : String
        }
    | MarkdownRelatedTerm
        { idReference : TermId
        , body : MarkdownFragment
        }


{-| Construct a related term from plain text.

    import Data.GlossaryItem.TermId as TermId

    fromPlaintext (TermId.fromString "Device_Edge") "Device Edge"
    |> raw --> "Device Edge"

    fromPlaintext (TermId.fromString "Device_Edge") "Device Edge"
    |> idReference
    |> TermId.toString
    --> "Device_Edge"

-}
fromPlaintext : TermId -> String -> RelatedTerm
fromPlaintext idReference0 body =
    PlaintextRelatedTerm
        { idReference = idReference0
        , body = body
        }


{-| Construct a related term from a Markdown string.

    import Data.GlossaryItem.TermId as TermId

    fromMarkdown (TermId.fromString "Device_Edge") "Device _Edge_"
    |> raw --> "Device _Edge_"

    fromMarkdown (TermId.fromString "Device_Edge") "Device _Edge_"
    |> idReference
    |> TermId.toString
    --> "Device_Edge"

-}
fromMarkdown : TermId -> String -> RelatedTerm
fromMarkdown idReference0 body =
    let
        fragment : MarkdownFragment
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
    import Data.GlossaryItem.TermId as TermId

    deviceEdge : Encode.Value
    deviceEdge =
        Encode.object
            [ ( "idReference", Encode.string "Device_Edge" )
            , ( "body", Encode.string "Device Edge" )
            ]

    decoded : Result Decode.Error RelatedTerm
    decoded = Decode.decodeValue (decode False) deviceEdge

    Result.map idReference decoded
    --> Ok <| TermId.fromString "Device_Edge"

    Result.map raw decoded --> Ok "Device Edge"

-}
decode : Bool -> Decoder RelatedTerm
decode enableMarkdownBasedSyntax =
    Decode.map2
        (if enableMarkdownBasedSyntax then
            fromMarkdown

         else
            fromPlaintext
        )
        (Decode.field "idReference" <| Decode.map TermId.fromString <| Decode.string)
        (Decode.field "body" Decode.string)


{-| Retrieve the ID reference of a related term.
-}
idReference : RelatedTerm -> TermId
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
                    case Renderer.render (MarkdownRenderers.inlineHtmlMsgRenderer enableMathSupport) blocks of
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
