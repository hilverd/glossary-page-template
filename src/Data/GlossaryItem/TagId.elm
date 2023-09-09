module Data.GlossaryItem.TagId exposing (TagId, fromString, toString)


type TagId
    = TagId String


fromString : String -> TagId
fromString =
    TagId


toString : TagId -> String
toString tagId =
    case tagId of
        TagId id ->
            id
