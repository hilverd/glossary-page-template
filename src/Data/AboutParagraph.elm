module Data.AboutParagraph exposing (AboutParagraph, fromString, toString)

{-| The "about" paragraph shown at the top of a glossary.


# "About" Paragraphs

@docs AboutParagraph, fromString, toString

-}


{-| An opaque type for an "about" paragraph which is just a wrapper around a string.
-}
type AboutParagraph
    = AboutParagraph String


{-| Construct an "about" paragraph from a string.
-}
fromString : String -> AboutParagraph
fromString =
    AboutParagraph


{-| Get the string representation of an "about" paragraph.
-}
toString : AboutParagraph -> String
toString aboutParagraph =
    case aboutParagraph of
        AboutParagraph paragraph ->
            paragraph
