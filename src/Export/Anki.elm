module Export.Anki exposing (download)

import Data.AboutLink as AboutLink exposing (AboutLink)
import Data.GlossaryItem exposing (GlossaryItem)
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
                |> List.map (.body >> escape)
                |> lines
                |> quote

        back =
            if List.isEmpty details then
                if List.isEmpty relatedTerms then
                    ""

                else
                    ("See: "
                        ++ (relatedTerms
                                |> List.map .body
                                |> String.join ", "
                           )
                    )
                        |> quote

            else
                details
                    |> List.map escape
                    |> paragraphs
                    |> quote
    in
    front ++ "\t" ++ back


download : GlossaryTitle -> String -> List AboutLink -> GlossaryItems -> Cmd msg
download glossaryTitle aboutParagraph aboutLinks glossaryItems =
    let
        filename =
            GlossaryTitle.toFilename "-Anki_deck.txt" glossaryTitle

        instructionsComment =
            comment "This file is meant to be imported into Anki (https://docs.ankiweb.net/importing.html#text-files)."

        titleComment =
            glossaryTitle |> GlossaryTitle.toString |> comment

        aboutLinksComment =
            aboutLinks
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
            , comment aboutParagraph
            , comment ""
            , aboutLinksComment
            , itemsString
            ]
                |> lines
    in
    Download.string filename "text/plain" content
