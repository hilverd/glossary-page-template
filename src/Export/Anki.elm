module Export.Anki exposing (download)

{-| Functionality for exporting to a format that [Anki](https://apps.ankiweb.net/index.html) can import to a flash card deck.

@docs download

-}

import Data.AboutLink as AboutLink
import Data.AboutParagraph as AboutParagraph
import Data.AboutSection exposing (AboutSection)
import Data.GlossaryItem.Definition as Definition
import Data.GlossaryItem.Term as Term
import Data.GlossaryItemForHtml as GlossaryItemForHtml exposing (GlossaryItemForHtml)
import Data.GlossaryTitle as GlossaryTitle exposing (GlossaryTitle)
import Data.IncubatingGlossaryItems as IncubatingGlossaryItems exposing (IncubatingGlossaryItems)
import Extras.HtmlTree
import File.Download as Download
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


itemToAnki : Bool -> GlossaryItemForHtml -> String
itemToAnki enableMathSupport glossaryItem =
    let
        quote : String -> String
        quote string =
            "\"" ++ string ++ "\""

        definitions =
            GlossaryItemForHtml.definition glossaryItem
                |> Maybe.map List.singleton
                |> Maybe.withDefault []

        relatedTerms =
            GlossaryItemForHtml.relatedPreferredTerms glossaryItem

        front : String
        front =
            glossaryItem
                |> GlossaryItemForHtml.allTerms
                |> List.map (Term.htmlTreeForAnki enableMathSupport >> Extras.HtmlTree.toHtml >> escape)
                |> htmlLines
                |> quote

        back : String
        back =
            if List.isEmpty definitions then
                if List.isEmpty relatedTerms then
                    ""

                else
                    ("See: "
                        ++ (relatedTerms
                                |> List.map (Term.htmlTreeForAnki enableMathSupport >> Extras.HtmlTree.toHtml >> escape)
                                |> String.join ", "
                           )
                    )
                        |> quote

            else
                definitions
                    |> List.map (Definition.htmlTreeForAnki enableMathSupport >> Extras.HtmlTree.toHtml >> escape)
                    |> paragraphs
                    |> quote
    in
    front ++ "\t" ++ back


{-| Export a glossary with the given title, "about" paragraph, and "about" links to a [text file suitable for Anki](https://docs.ankiweb.net/importing.html#text-files).
This is achieved by producing a [command for downloading](https://package.elm-lang.org/packages/elm/file/latest/File.Download) this file.
-}
download : Bool -> GlossaryTitle -> AboutSection -> IncubatingGlossaryItems -> Cmd msg
download enableMathSupport glossaryTitle aboutSection glossaryItems =
    let
        filename : String
        filename =
            GlossaryTitle.toFilename "-Anki_deck.txt" glossaryTitle

        separatorHeader : String
        separatorHeader =
            header "separator" "Tab"

        htmlHeader : String
        htmlHeader =
            header "html" "true"

        instructionsComment : String
        instructionsComment =
            comment "This file is meant to be imported into Anki (https://docs.ankiweb.net/importing.html#text-files)."

        titleComment : String
        titleComment =
            glossaryTitle |> GlossaryTitle.raw |> comment

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
            glossaryItems
                |> IncubatingGlossaryItems.orderedAlphabetically Nothing
                |> List.map (Tuple.second >> itemToAnki enableMathSupport)
                |> lines

        content : String
        content =
            [ separatorHeader
            , htmlHeader
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
