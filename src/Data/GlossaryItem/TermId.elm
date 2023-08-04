module Data.GlossaryItem.TermId exposing (TermId, fromString, toString)


type TermId
    = TermId String


fromString : String -> TermId
fromString =
    TermId


toString : TermId -> String
toString termId =
    case termId of
        TermId id ->
            id
