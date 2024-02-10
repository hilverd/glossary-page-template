module Data.GlossaryTitle exposing (GlossaryTitle, fromMarkdown, raw, codec, toFilename, inlineText, markdown, view)

{-| The title of a glossary.


# Glossary Titles

@docs GlossaryTitle, fromMarkdown, raw, codec, toFilename, inlineText, markdown, view

-}

import Codec exposing (Codec)
import Data.MarkdownFragment as MarkdownFragment exposing (MarkdownFragment)
import Html exposing (Attribute, Html, text)
import Html.Attributes exposing (class)
import Internationalisation as I18n
import Markdown.Block exposing (Block)
import Markdown.Renderer as Renderer
import MarkdownRenderers
import Regex


{-| An opaque type for a glossary title which is just a wrapper around a string.
-}
type GlossaryTitle
    = MarkdownGlossaryTitle MarkdownFragment


{-| Construct a glossary title from a Markdown string.

    title : GlossaryTitle
    title = fromMarkdown "The _ideal_ case"

    raw title --> "The _ideal_ case"

-}
fromMarkdown : String -> GlossaryTitle
fromMarkdown =
    MarkdownFragment.fromString >> MarkdownGlossaryTitle


{-| Retrieve the raw body of a glossary title.
-}
raw : GlossaryTitle -> String
raw (MarkdownGlossaryTitle fragment) =
    MarkdownFragment.raw fragment


{-| An encoder/decoder for a glossary title.
-}
codec : Codec GlossaryTitle
codec =
    Codec.map fromMarkdown raw Codec.string


{-| Convert a glossary title to a filename with the given extension.
Spaces are replaced by underscores.
Any other characters which are not generally safe for filenames are omitted.

    "Weather Terms: A Complete Guide"
    |> fromMarkdown
    |> toFilename ".html"
    --> "Weather_Terms_A_Complete_Guide.html"

-}
toFilename : String -> GlossaryTitle -> String
toFilename extension glossaryTitle =
    let
        notSoNiceCharacters : Regex.Regex
        notSoNiceCharacters =
            "[^A-Za-zÀ-ÖØ-öø-įĴ-őŔ-žǍ-ǰǴ-ǵǸ-țȞ-ȟȤ-ȳɃɆ-ɏḀ-ẞƀ-ƓƗ-ƚƝ-ơƤ-ƥƫ-ưƲ-ƶẠ-ỿ0-9_-]"
                |> Regex.fromString
                |> Maybe.withDefault Regex.never
    in
    glossaryTitle
        |> raw
        |> Regex.replace notSoNiceCharacters
            (\m ->
                if m.match == " " then
                    "_"

                else
                    ""
            )
        |> (\result ->
                if String.startsWith "-" result then
                    String.dropLeft 1 result

                else
                    result
           )
        |> (\result -> result ++ extension)


{-| Retrieve the concatenated inline text of a glossary title.

    fromMarkdown "*Hello* _there_"
    |> inlineText
    --> "Hello there"

-}
inlineText : GlossaryTitle -> String
inlineText (MarkdownGlossaryTitle fragment) =
    fragment
        |> MarkdownFragment.concatenateInlineText
        |> Result.withDefault (MarkdownFragment.raw fragment)


{-| Convert a glossary title to a string suitable for a Markdown document.
-}
markdown : GlossaryTitle -> String
markdown (MarkdownGlossaryTitle fragment) =
    MarkdownFragment.raw fragment


{-| View a glossary title as HTML.

    import Html exposing (Html)

    expected : Html msg
    expected =
        Html.span []
            [ Html.span []
                [ Html.text "The "
                , Html.em [] [ Html.text "ideal" ]
                , Html.text " case"
                ]
            ]

    fromMarkdown "The _ideal_ case" |> view False
    --> expected

-}
view : Bool -> List (Attribute msg) -> GlossaryTitle -> Html msg
view enableMathSupport additionalAttributes (MarkdownGlossaryTitle fragment) =
    let
        parsed : Result String (List Block)
        parsed =
            MarkdownFragment.parsed fragment
    in
    case parsed of
        Ok blocks ->
            case Renderer.render (MarkdownRenderers.inlineHtmlMsgRenderer enableMathSupport) blocks of
                Ok rendered ->
                    Html.span
                        (class "prose print:prose-neutral dark:prose-invert dark:prose-pre:text-gray-200 prose-code:before:hidden prose-code:after:hidden" :: additionalAttributes)
                        rendered

                Err renderingError ->
                    text <| I18n.failedToRenderMarkdown ++ ": " ++ renderingError

        Err parsingError ->
            text <| I18n.failedToParseMarkdown ++ ": " ++ parsingError
