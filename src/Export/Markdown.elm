module Export.Markdown exposing (download)

{-| Functionality for exporting as Markdown.

@docs download

-}

import Array
import Data.AboutLink as AboutLink exposing (AboutLink)
import Data.AboutParagraph as AboutParagraph
import Data.AboutSection exposing (AboutSection)
import Data.GlossaryItem exposing (GlossaryItem)
import Data.GlossaryItem.Details as Details
import Data.GlossaryItem.RelatedTerm as RelatedTerm
import Data.GlossaryItem.Term as Term
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Data.GlossaryTitle as GlossaryTitle exposing (GlossaryTitle)
import Extras.HtmlTree
import Extras.String
import File.Download as Download


bold : String -> String
bold string =
    "**" ++ string ++ "**"


link : AboutLink -> String
link aboutLink =
    "["
        ++ (aboutLink |> AboutLink.body |> Extras.String.escapeForMarkdown)
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
        termsString : String
        termsString =
            terms
                |> List.map (Term.markdown >> bold)
                |> lines

        detailsString : String
        detailsString =
            details
                |> List.map Details.markdown
                |> paragraphs

        relatedTermsPrefix : String
        relatedTermsPrefix =
            if List.isEmpty relatedTerms then
                ""

            else if List.isEmpty details then
                "See: "

            else
                "See also: "

        relatedTermsString : String
        relatedTermsString =
            relatedTerms
                |> List.map RelatedTerm.markdown
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
        filename : String
        filename =
            GlossaryTitle.toFilename ".md" glossaryTitle

        titleHeadingString : String
        titleHeadingString =
            glossaryTitle |> GlossaryTitle.markdown |> (++) "# "

        aboutParagraphString : String
        aboutParagraphString =
            aboutSection.paragraph |> AboutParagraph.markdown

        aboutLinksString : String
        aboutLinksString =
            aboutSection.links
                |> List.map (link >> (++) "* ")
                |> lines

        itemsString : String
        itemsString =
            glossaryItems
                |> GlossaryItems.orderedAlphabetically
                |> Array.toList
                |> List.map (Tuple.second >> itemToMarkdown)
                |> paragraphs

        content : String
        content =
            [ titleHeadingString
            , aboutParagraphString
            , aboutLinksString
            , horizontalRule
            , itemsString
            ]
                |> paragraphs
    in
    Download.string filename "text/markdown" content
