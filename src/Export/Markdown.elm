module Export.Markdown exposing (download)

{-| Functionality for exporting as Markdown.

@docs download

-}

import Array
import Data.AboutLink as AboutLink exposing (AboutLink)
import Data.AboutParagraph as AboutParagraph
import Data.AboutSection exposing (AboutSection)
import Data.GlossaryItem as GlossaryItem exposing (GlossaryItem)
import Data.GlossaryItem.Definition as Definition
import Data.GlossaryItem.RelatedTerm as RelatedTerm
import Data.GlossaryItem.Term as Term
import Data.GlossaryItemForHtml as GlossaryItemForHtml exposing (GlossaryItemForHtml)
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Data.GlossaryTitle as GlossaryTitle exposing (GlossaryTitle)
import Data.IncubatingGlossaryItems as IncubatingGlossaryItems exposing (IncubatingGlossaryItems)
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


itemToMarkdown : GlossaryItemForHtml -> String
itemToMarkdown glossaryItem =
    let
        termsString : String
        termsString =
            glossaryItem
                |> GlossaryItemForHtml.allTerms
                |> List.map (Term.markdown >> bold)
                |> lines

        definitions =
            GlossaryItemForHtml.definition glossaryItem
                |> Maybe.map List.singleton
                |> Maybe.withDefault []

        relatedTerms =
            GlossaryItemForHtml.relatedPreferredTerms glossaryItem

        definitionsString : String
        definitionsString =
            definitions
                |> List.map Definition.markdown
                |> paragraphs

        relatedTermsPrefix : String
        relatedTermsPrefix =
            if List.isEmpty relatedTerms then
                ""

            else if List.isEmpty definitions then
                "See: "

            else
                "See also: "

        relatedTermsString : String
        relatedTermsString =
            relatedTerms
                |> List.map Term.markdown
                |> String.join ", "
                |> (++) relatedTermsPrefix
    in
    [ termsString, definitionsString, relatedTermsString ]
        |> paragraphs


{-| Export a glossary with the given title, "about" paragraph, and "about" links to a Markdown file.
This is achieved by producing a [command for downloading](https://package.elm-lang.org/packages/elm/file/latest/File.Download) this file.
-}
download : GlossaryTitle -> AboutSection -> IncubatingGlossaryItems -> Cmd msg
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
                |> IncubatingGlossaryItems.orderedAlphabetically Nothing
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
