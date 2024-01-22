module Export.Markdown exposing (toString, download)

{-| Functionality for exporting as Markdown.

@docs toString, download

-}

import Data.AboutLink as AboutLink exposing (AboutLink)
import Data.AboutParagraph as AboutParagraph
import Data.Glossary as Glossary exposing (Glossary, aboutSection)
import Data.GlossaryItem.Definition as Definition
import Data.GlossaryItem.Tag as Tag
import Data.GlossaryItem.Term as Term
import Data.GlossaryItemForHtml as GlossaryItemForHtml exposing (GlossaryItemForHtml)
import Data.GlossaryItems as GlossaryItems
import Data.GlossaryTitle as GlossaryTitle exposing (GlossaryTitle)
import Data.TagDescription as TagDescription exposing (TagDescription)
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


itemToMarkdown : GlossaryItemForHtml -> String
itemToMarkdown glossaryItem =
    let
        termsString : String
        termsString =
            glossaryItem
                |> GlossaryItemForHtml.allTerms
                |> List.map (Term.markdown >> bold)
                |> String.join ("\\" ++ crlf)

        tagsString : String
        tagsString =
            glossaryItem
                |> GlossaryItemForHtml.allTags
                |> List.map Tag.markdown
                |> String.join ", "
                |> (\str ->
                        if not <| String.isEmpty str then
                            "[" ++ I18n.tags ++ ": " ++ str ++ "]"

                        else
                            ""
                   )

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
                I18n.see ++ ": "

            else
                I18n.seeAlso ++ ": "

        relatedTermsString : String
        relatedTermsString =
            relatedTerms
                |> List.map Term.markdown
                |> String.join ", "
                |> (++) relatedTermsPrefix
    in
    [ termsString, tagsString, definitionsString, relatedTermsString ]
        |> paragraphs


{-| Export a glossary to Markdown format.
-}
toString : Glossary -> String
toString glossary =
    let
        title =
            Glossary.title glossary

        aboutSection =
            Glossary.aboutSection glossary

        items =
            Glossary.items glossary

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
            glossary
                |> Glossary.items
                |> GlossaryItems.tagsWithDescriptions
                |> List.map
                    (\( tag, tagDescription ) ->
                        "* "
                            ++ Tag.markdown tag
                            ++ " "
                            ++ Extras.String.emDash
                            ++ " "
                            ++ TagDescription.markdown tagDescription
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

        content : String
        content =
            [ titleHeadingString
            , aboutParagraphString
            , aboutLinksString
            , horizontalRule
            , tagsSection
            , "## Items"
            , itemsString
            ]
                |> paragraphs
    in
    content


{-| Export a glossary to a Markdown file.
This is achieved by producing a [command for downloading](https://package.elm-lang.org/packages/elm/file/latest/File.Download) this file.
-}
download : Glossary -> Cmd msg
download glossary =
    let
        title : GlossaryTitle
        title =
            Glossary.title glossary

        filename : String
        filename =
            GlossaryTitle.toFilename ".md" title
    in
    glossary
        |> toString
        |> Download.string filename "text/markdown"
