module Export.Markdown exposing (download)

import Data.AboutLink as AboutLink exposing (AboutLink)
import Data.GlossaryItem exposing (GlossaryItem, RelatedTerm)
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Data.GlossaryTitle as GlossaryTitle exposing (GlossaryTitle)
import File.Download as Download
import Regex


bold : String -> String
bold string =
    "**" ++ string ++ "**"


link : AboutLink -> String
link aboutLink =
    "[" ++ AboutLink.body aboutLink ++ "](" ++ AboutLink.href aboutLink ++ ")"


horizontalRule : String
horizontalRule =
    "---------"


itemToMarkdown : GlossaryItem -> String
itemToMarkdown { terms, details, relatedTerms } =
    let
        termsString =
            terms
                |> List.map (\term -> escape term.body |> bold)
                |> String.join "\n"

        detailsString =
            details
                |> List.map (\detailsSingle -> escape detailsSingle)
                |> String.join "\n\n"

        relatedTermsPrefix =
            if List.isEmpty relatedTerms then
                ""

            else if List.isEmpty details then
                "See: "

            else
                "See also: "

        relatedTermsString =
            relatedTerms
                |> List.map (\relatedTerm -> relatedTerm.body)
                |> String.join ", "
                |> (++) relatedTermsPrefix
    in
    termsString ++ "\n\n" ++ detailsString ++ "\n\n" ++ relatedTermsString


escape : String -> String
escape string =
    let
        charactersToEscape =
            -- Here I'm leaving out [().!-]
            "[\\`*_{}\\[\\]#+]"
                |> Regex.fromString
                |> Maybe.withDefault Regex.never
    in
    Regex.replace charactersToEscape (.match >> (++) "\\") string


download : GlossaryTitle -> String -> List AboutLink -> GlossaryItems -> Cmd msg
download glossaryTitle aboutParagraph aboutLinks glossaryItems =
    let
        filename =
            GlossaryTitle.toFilename "md" glossaryTitle

        titleHeaderString =
            glossaryTitle |> GlossaryTitle.toString |> escape |> (++) "# "

        aboutParagraphString =
            escape aboutParagraph

        aboutLinksString =
            aboutLinks
                |> List.map (link >> (++) "* ")
                |> String.join "\n"

        itemsString =
            glossaryItems
                |> GlossaryItems.orderedAlphabetically
                |> List.map (Tuple.second >> itemToMarkdown)
                |> String.join "\n\n"

        content =
            titleHeaderString
                ++ "\n\n"
                ++ aboutParagraphString
                ++ "\n\n"
                ++ aboutLinksString
                ++ "\n\n"
                ++ horizontalRule
                ++ "\n\n"
                ++ itemsString
    in
    Download.string filename "text/markdown" content
