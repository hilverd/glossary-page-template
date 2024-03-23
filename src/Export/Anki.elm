module Export.Anki exposing (download)

{-| Functionality for exporting to a format that [Anki](https://apps.ankiweb.net/index.html) can import to a flash card deck.

@docs download

-}

import Data.AboutLink as AboutLink
import Data.AboutParagraph as AboutParagraph
import Data.Glossary as Glossary exposing (Glossary)
import Data.GlossaryItem.Definition as Definition
import Data.GlossaryItem.DisambiguatedTerm as DisambiguatedTerm exposing (DisambiguatedTerm)
import Data.GlossaryItem.Tag as Tag
import Data.GlossaryItem.Term as Term
import Data.GlossaryItemForUi as GlossaryItemForUi exposing (GlossaryItemForUi)
import Data.GlossaryItems as GlossaryItems
import Data.GlossaryTitle as GlossaryTitle
import Extras.HtmlTree
import File.Download as Download
import Internationalisation as I18n
import Regex


escape : String -> String
escape string =
    string
        |> String.replace "\"" "\"\""
        |> String.replace "\t" " "


comment : String -> String
comment =
    let
        linebreaks : Regex.Regex
        linebreaks =
            "[\u{000D}\n]"
                |> Regex.fromString
                |> Maybe.withDefault Regex.never
    in
    Regex.replace linebreaks (always " ")
        >> (++) "# "


header : String -> String -> String
header key value =
    "#" ++ key ++ ":" ++ value


crlf : String
crlf =
    "\u{000D}\n"


lines : List String -> String
lines =
    String.join crlf


htmlLines : List String -> String
htmlLines =
    String.join "<br>"


paragraphs : List String -> String
paragraphs =
    List.filter (not << String.isEmpty)
        >> String.join (crlf ++ crlf)


itemToAnki : Bool -> GlossaryItemForUi -> String
itemToAnki enableMathSupport glossaryItem =
    let
        quote : String -> String
        quote string =
            "\"" ++ string ++ "\""

        definitions =
            GlossaryItemForUi.definition glossaryItem
                |> Maybe.map List.singleton
                |> Maybe.withDefault []

        front : String
        front =
            glossaryItem
                |> GlossaryItemForUi.allTerms
                |> List.map (Term.htmlTreeForAnki enableMathSupport >> Extras.HtmlTree.toHtml >> escape)
                |> htmlLines
                |> quote

        back : String
        back =
            if List.isEmpty definitions then
                let
                    relatedTerms : List DisambiguatedTerm
                    relatedTerms =
                        GlossaryItemForUi.relatedPreferredTerms glossaryItem
                in
                if List.isEmpty relatedTerms then
                    ""

                else
                    (I18n.see
                        ++ ": "
                        ++ (relatedTerms
                                |> List.map
                                    (DisambiguatedTerm.toTerm
                                        >> Term.htmlTreeForAnki enableMathSupport
                                        >> Extras.HtmlTree.toHtml
                                        >> escape
                                    )
                                |> String.join ", "
                           )
                    )
                        |> quote

            else
                definitions
                    |> List.map (Definition.htmlTreeForAnki enableMathSupport >> Extras.HtmlTree.toHtml >> escape)
                    |> paragraphs
                    |> quote

        tags : String
        tags =
            GlossaryItemForUi.allTags glossaryItem
                |> List.map (Tag.inlineText >> String.replace " " "_" >> escape)
                |> String.join " "
                |> quote
    in
    front ++ "\t" ++ back ++ "\t" ++ tags


{-| Export a glossary with to a [text file suitable for Anki](https://docs.ankiweb.net/importing.html#text-files).
This is achieved by producing a [command for downloading](https://package.elm-lang.org/packages/elm/file/latest/File.Download) this file.
-}
download : Bool -> Glossary -> Cmd msg
download enableMathSupport glossary =
    let
        title =
            Glossary.title glossary

        aboutSection =
            Glossary.aboutSection glossary

        items =
            Glossary.items glossary

        filename : String
        filename =
            GlossaryTitle.toFilename "-Anki_deck.txt" title

        separatorHeader : String
        separatorHeader =
            header "separator" "Tab"

        htmlHeader : String
        htmlHeader =
            header "html" "true"

        columnsHeader : String
        columnsHeader =
            header "columns" "Front\tBack\tTags"

        tagsColumnHeader : String
        tagsColumnHeader =
            header "tags column" "3"

        instructionsComment : String
        instructionsComment =
            comment I18n.thisFileIsMeantToBeImportedIntoAnki

        titleComment : String
        titleComment =
            title |> GlossaryTitle.raw |> comment

        aboutLinksComment : String
        aboutLinksComment =
            aboutSection.links
                |> List.map
                    (\aboutLink ->
                        "* " ++ AboutLink.body aboutLink ++ " - " ++ AboutLink.href aboutLink
                    )
                |> List.map comment
                |> lines

        itemsString : String
        itemsString =
            items
                |> GlossaryItems.orderedAlphabetically Nothing
                |> List.map (Tuple.second >> itemToAnki enableMathSupport)
                |> lines

        content : String
        content =
            [ separatorHeader
            , htmlHeader
            , columnsHeader
            , tagsColumnHeader
            , instructionsComment
            , comment ""
            , titleComment
            , comment ""
            , comment <| AboutParagraph.raw aboutSection.paragraph
            , comment ""
            , aboutLinksComment
            , itemsString
            ]
                |> lines
    in
    Download.string filename "text/plain" content
