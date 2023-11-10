module TagsForm.TagField exposing (TagField, empty, fromString, raw, validationError, setBody, setValidationError)

{-| A form field that contains a tag.


# Tag Fields

@docs TagField, empty, fromString, raw, validationError, setBody, setValidationError

-}


{-| A tag field.
-}
type TagField
    = TagField
        { body : String
        , validationError : Maybe String
        }


{-| Construct an empty tag field.

    empty |> raw --> ""

    empty |> validationError --> Nothing

-}
empty : TagField
empty =
    TagField
        { body = ""
        , validationError = Nothing
        }


{-| Construct a tag field from a string.

    fromString "NA" |> raw --> "NA"

    fromString "Foo" |> validationError --> Nothing

-}
fromString : String -> TagField
fromString body0 =
    TagField
        { body = body0
        , validationError = Nothing
        }


{-| Retrieve the raw body of a tag field.
-}
raw : TagField -> String
raw tagField =
    case tagField of
        TagField field ->
            field.body


{-| Retrieve the validation error of a tag field.
-}
validationError : TagField -> Maybe String
validationError tagField =
    case tagField of
        TagField field ->
            field.validationError


{-| Set the body on a tag field.

    empty
    |> setBody "TBD"
    |> raw
    --> "TBD"

-}
setBody : String -> TagField -> TagField
setBody body0 tagField =
    case tagField of
        TagField field ->
            TagField { field | body = body0 }


{-| Set the validation error on a tag field.

    empty
    |> setValidationError (Just "Can't be empty")
    |> validationError
    --> Just "Can't be empty"

-}
setValidationError : Maybe String -> TagField -> TagField
setValidationError error tagField =
    case tagField of
        TagField field ->
            TagField { field | validationError = error }
