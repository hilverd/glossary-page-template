module Export.Markdown exposing (download)

{-| Functionality for exporting as Markdown.

@docs download

-}

import Data.AboutLink as AboutLink exposing (AboutLink)
import Data.AboutParagraph as AboutParagraph
import Data.AboutSection exposing (AboutSection(..))
import Data.GlossaryItem exposing (GlossaryItem)
import Data.GlossaryItem.Details as Details
import Data.GlossaryItem.RelatedTerm as RelatedTerm exposing (RelatedTerm)
import Data.GlossaryItem.Term as Term
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Data.GlossaryTitle as GlossaryTitle exposing (GlossaryTitle)
import Extras.HtmlTree
import File.Download as Download
import Regex


escape : String -> String
escape string =
    let
        charactersToEscape =
            -- It might be necessary to escape this set too: [().!-]
            -- but these are left out for now.
            "[\\`*_{}\\[\\]#+]"
                |> Regex.fromString
                |> Maybe.withDefault Regex.never
    in
    Regex.replace charactersToEscape (.match >> (++) "\\") string


bold : String -> String
bold string =
    "**" ++ escape string ++ "**"


link : AboutLink -> String
link aboutLink =
    "["
        ++ (aboutLink |> AboutLink.body |> escape)
        ++ "]("
        ++ (aboutLink |> AboutLink.href |> Extras.HtmlTree.escape)
        ++ ")"


horizontalRule : String
horizontalRule =
    "---------"


crlf : String
crlf =
    "\u{000D}\n"


lines : List String -> String
lines =
    String.join crlf


paragraphs : List String -> String
paragraphs =
    List.filter (not << String.isEmpty)
        >> String.join (crlf ++ crlf)


itemToMarkdown : GlossaryItem -> String
itemToMarkdown { terms, details, relatedTerms } =
    let
        termsString =
            terms
                |> List.map (Term.raw >> bold)
                |> lines

        detailsString =
            details
                |> List.map (Details.raw >> escape)
                |> paragraphs

        relatedTermsPrefix =
            if List.isEmpty relatedTerms then
                ""

            else if List.isEmpty details then
                "See: "

            else
                "See also: "

        relatedTermsString =
            relatedTerms
                |> List.map (RelatedTerm.raw >> escape)
                |> String.join ", "
                |> (++) relatedTermsPrefix
    in
    [ termsString, detailsString, relatedTermsString ]
        |> paragraphs


{-| Export a glossary with the given title, "about" paragraph, and "about" links to a Markdown file.
This is achieved by producing a [command for downloading](https://package.elm-lang.org/packages/elm/file/latest/File.Download) this file.
-}
download : GlossaryTitle -> AboutSection -> GlossaryItems -> Cmd msg
download glossaryTitle aboutSection glossaryItems =
    let
        filename =
            GlossaryTitle.toFilename ".md" glossaryTitle

        titleHeaderString =
            glossaryTitle |> GlossaryTitle.toString |> escape |> (++) "# "

        ( aboutParagraph, aboutLinks ) =
            case aboutSection of
                PlaintextAboutSection { paragraph, links } ->
                    ( paragraph, links )

        aboutParagraphString =
            aboutParagraph |> AboutParagraph.toString |> escape

        aboutLinksString =
            aboutLinks
                |> List.map (link >> (++) "* ")
                |> lines

        itemsString =
            glossaryItems
                |> GlossaryItems.orderedAlphabetically
                |> List.map (Tuple.second >> itemToMarkdown)
                |> paragraphs

        content =
            [ titleHeaderString
            , aboutParagraphString
            , aboutLinksString
            , horizontalRule
            , itemsString
            ]
                |> paragraphs
    in
    Download.string filename "text/markdown" content
