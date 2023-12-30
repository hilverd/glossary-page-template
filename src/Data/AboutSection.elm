module Data.AboutSection exposing (AboutSection, codec)

import Codec exposing (Codec)
import Data.AboutLink as AboutLink exposing (AboutLink)
import Data.AboutParagraph as AboutParagraph exposing (AboutParagraph)


type alias AboutSection =
    { paragraph : AboutParagraph
    , links : List AboutLink
    }


codec : Codec AboutSection
codec =
    Codec.object (\paragraph links -> { paragraph = paragraph, links = links })
        |> Codec.field "paragraph" .paragraph AboutParagraph.codec
        |> Codec.field "links" .links (Codec.list AboutLink.codec)
        |> Codec.buildObject
