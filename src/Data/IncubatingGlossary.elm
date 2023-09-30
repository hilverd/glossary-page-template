module Data.IncubatingGlossary exposing (IncubatingGlossary)

import Data.AboutSection exposing (AboutSection)
import Data.CardWidth exposing (CardWidth)
import Data.GlossaryTitle exposing (GlossaryTitle)
import Data.IncubatingGlossaryItems exposing (IncubatingGlossaryItems)


type alias IncubatingGlossary =
    { enableMarkdownBasedSyntax : Bool
    , enableMathSupport : Bool
    , enableLastUpdatedDates : Bool
    , cardWidth : CardWidth
    , title : GlossaryTitle
    , aboutSection : AboutSection
    , items : IncubatingGlossaryItems
    }
