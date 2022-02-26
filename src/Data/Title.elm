module Data.Title exposing (Title, fromString, toString)


type Title
    = Title String


fromString : String -> Title
fromString =
    Title


toString : Title -> String
toString title =
    case title of
        Title t ->
            t
