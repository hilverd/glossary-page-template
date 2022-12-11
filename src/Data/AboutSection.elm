module Data.AboutSection exposing (AboutSection(..))

import Data.AboutLink exposing (AboutLink)
import Data.AboutParagraph exposing (AboutParagraph)


type AboutSection
    = PlaintextAboutSection
        { paragraph : AboutParagraph
        , links : List AboutLink
        }
