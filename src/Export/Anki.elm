module Export.Anki exposing (download)

{-| Functionality for exporting to a format that [Anki](https://apps.ankiweb.net/index.html) can import to a flash card deck.

@docs download

-}

import Data.AboutLink as AboutLink
import Data.AboutParagraph as AboutParagraph
import Data.AboutSection exposing (AboutSection)
import Data.GlossaryItem exposing (GlossaryItem)
import Data.GlossaryItem.Details as Details
import Data.GlossaryItem.RelatedTerm as RelatedTerm
import Data.GlossaryItem.Term as Term
import Data.GlossaryItems as GlossaryItems exposing (GlossaryItems)
import Data.GlossaryTitle as GlossaryTitle exposing (GlossaryTitle)
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
        linebreaks =
            "[\u{000D}\n]"
                |> Regex.fromString
                |> Maybe.withDefault Regex.never
    in
    Regex.replace linebreaks (always " ")
        >> (++) "# "


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


itemToAnki : GlossaryItem -> String
itemToAnki { terms, details, relatedTerms } =
    let
        quote string =
            "\"" ++ string ++ "\""

        front =
            terms
                |> List.map (Term.raw >> escape)
                |> lines
                |> quote

        back =
            if List.isEmpty details then
                if List.isEmpty relatedTerms then
                    ""

                else
                    ("See: "
                        ++ (relatedTerms
                                |> List.map RelatedTerm.raw
                                |> String.join ", "
                           )
                    )
                        |> quote

            else
                details
                    |> List.map (Details.raw >> escape)
                    |> paragraphs
                    |> quote
    in
    front ++ "\t" ++ back


{-| Export a glossary with the given title, "about" paragraph, and "about" links to a [text file suitable for Anki](https://docs.ankiweb.net/importing.html#text-files).
This is achieved by producing a [command for downloading](https://package.elm-lang.org/packages/elm/file/latest/File.Download) this file.
-}
download : GlossaryTitle -> AboutSection -> GlossaryItems -> Cmd msg
download glossaryTitle aboutSection glossaryItems =
    let
        filename =
            GlossaryTitle.toFilename "-Anki_deck.txt" glossaryTitle

        instructionsComment =
            comment "This file is meant to be imported into Anki (https://docs.ankiweb.net/importing.html#text-files)."

        titleComment =
            glossaryTitle |> GlossaryTitle.toString |> comment

        aboutLinksComment =
            aboutSection.links
                |> List.map
                    (\aboutLink ->
                        "* " ++ AboutLink.body aboutLink ++ " - " ++ AboutLink.href aboutLink
                    )
                |> List.map comment
                |> lines

        itemsString =
            glossaryItems
                |> GlossaryItems.orderedAlphabetically
                |> List.map (Tuple.second >> itemToAnki)
                |> lines

        content =
            [ instructionsComment
            , comment ""
            , titleComment
            , comment ""
            , comment <| AboutParagraph.toString aboutSection.paragraph
            , comment ""
            , aboutLinksComment
            , itemsString
            ]
                |> lines
    in
    Download.string filename "text/plain" content
