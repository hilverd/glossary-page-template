module Export.Markdown exposing (toString, itemToMarkdown, download)

{-| Functionality for exporting as Markdown.

@docs toString, itemToMarkdown, download

-}

import Data.AboutLink as AboutLink exposing (AboutLink)
import Data.AboutParagraph as AboutParagraph
import Data.AboutSection exposing (AboutSection)
import Data.DescribedTag as DescribedTag
import Data.GlossaryForUi as GlossaryForUi exposing (GlossaryForUi)
import Data.GlossaryItem.Definition as Definition
import Data.GlossaryItem.DisambiguatedTerm as DisambiguatedTerm
import Data.GlossaryItem.Tag as Tag
import Data.GlossaryItem.Term as Term
import Data.GlossaryItemForUi as GlossaryItemForUi exposing (GlossaryItemForUi)
import Data.GlossaryItemsForUi as GlossaryItems
import Data.GlossaryTitle as GlossaryTitle exposing (GlossaryTitle)
import Data.TagDescription as TagDescription
import Extras.HtmlTree
import Extras.String
import File.Download as Download
import Internationalisation as I18n


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


itemToMarkdown : GlossaryItemForUi -> String
itemToMarkdown glossaryItem =
    let
        termsString : String
        termsString =
            glossaryItem
                |> GlossaryItemForUi.allTerms
                |> List.map (Term.markdown >> bold)
                |> String.join ("\\" ++ crlf)

        tagsString : String
        tagsString =
            glossaryItem
                |> GlossaryItemForUi.allTags
                |> List.map Tag.markdown
                |> String.join ", "
                |> (\str ->
                        if not <| String.isEmpty str then
                            "[" ++ I18n.tags ++ ": " ++ str ++ "]"

                        else
                            ""
                   )

        definitions : List Definition.Definition
        definitions =
            GlossaryItemForUi.definition glossaryItem
                |> Maybe.map List.singleton
                |> Maybe.withDefault []

        relatedTerms : List DisambiguatedTerm.DisambiguatedTerm
        relatedTerms =
            GlossaryItemForUi.relatedPreferredTerms glossaryItem

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
                I18n.see ++ ": "

            else
                I18n.seeAlso ++ ": "

        relatedTermsString : String
        relatedTermsString =
            relatedTerms
                |> List.map (DisambiguatedTerm.toTerm >> Term.markdown)
                |> String.join ", "
                |> (++) relatedTermsPrefix
    in
    [ termsString, tagsString, definitionsString, relatedTermsString ]
        |> paragraphs


{-| Export a glossary to Markdown format.
-}
toString : GlossaryForUi -> String
toString glossaryForUi =
    let
        title : GlossaryTitle
        title =
            GlossaryForUi.title glossaryForUi

        aboutSection : AboutSection
        aboutSection =
            GlossaryForUi.aboutSection glossaryForUi

        items : GlossaryItems.GlossaryItemsForUi
        items =
            GlossaryForUi.items glossaryForUi

        titleHeadingString : String
        titleHeadingString =
            title |> GlossaryTitle.markdown |> (++) "# "

        aboutParagraphString : String
        aboutParagraphString =
            aboutSection.paragraph |> AboutParagraph.markdown

        aboutLinksString : String
        aboutLinksString =
            aboutSection.links
                |> List.map (link >> (++) "* ")
                |> lines

        tagsString : String
        tagsString =
            glossaryForUi
                |> GlossaryForUi.items
                |> GlossaryItems.describedTags
                |> List.map
                    (\describedTag ->
                        "* "
                            ++ (Tag.markdown <| DescribedTag.tag describedTag)
                            ++ " "
                            ++ Extras.String.emDash
                            ++ " "
                            ++ (TagDescription.markdown <| DescribedTag.description describedTag)
                    )
                |> lines

        tagsSection : String
        tagsSection =
            if String.isEmpty tagsString then
                ""

            else
                lines [ "## Tags", "", tagsString ]

        itemsString : String
        itemsString =
            items
                |> GlossaryItems.orderedAlphabetically Nothing
                |> List.map (Tuple.second >> itemToMarkdown)
                |> paragraphs
    in
    [ titleHeadingString
    , aboutParagraphString
    , aboutLinksString
    , horizontalRule
    , tagsSection
    , "## Items"
    , itemsString
    ]
        |> paragraphs


{-| Export a glossary to a Markdown file.
This is achieved by producing a [command for downloading](https://package.elm-lang.org/packages/elm/file/latest/File.Download) this file.
-}
download : GlossaryForUi -> Cmd msg
download glossaryForUi =
    let
        title : GlossaryTitle
        title =
            GlossaryForUi.title glossaryForUi

        filename : String
        filename =
            GlossaryTitle.toFilename ".md" title
    in
    glossaryForUi
        |> toString
        |> Download.string filename "text/markdown"
