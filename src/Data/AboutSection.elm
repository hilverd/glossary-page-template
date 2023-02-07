module Data.AboutSection exposing (AboutSection)

import Data.AboutLink exposing (AboutLink)
import Data.AboutParagraph exposing (AboutParagraph)


type alias AboutSection =
    { paragraph : AboutParagraph
    , links : List AboutLink
    }
