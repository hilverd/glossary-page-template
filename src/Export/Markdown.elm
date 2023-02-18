module Export.Markdown exposing (download)

{-| Functionality for exporting as Markdown.

@docs download

-}

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
    "**" ++ Extras.String.escapeForMarkdown string ++ "**"


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
        termsString =
            terms
                |> List.map (Term.raw >> bold)
                |> lines

        detailsString =
            details
                |> List.map Details.markdown
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
                |> List.map (RelatedTerm.raw >> Extras.String.escapeForMarkdown)
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

        titleHeadingString =
            glossaryTitle |> GlossaryTitle.toString |> Extras.String.escapeForMarkdown |> (++) "# "

        aboutParagraphString =
            aboutSection.paragraph |> AboutParagraph.markdown

        aboutLinksString =
            aboutSection.links
                |> List.map (link >> (++) "* ")
                |> lines

        itemsString =
            glossaryItems
                |> GlossaryItems.orderedAlphabetically
                |> List.map (Tuple.second >> itemToMarkdown)
                |> paragraphs

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
