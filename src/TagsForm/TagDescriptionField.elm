module TagsForm.TagDescriptionField exposing (TagDescriptionField, empty, fromString, raw, validationError, setBody, setValidationError)

{-| A form field that contains a tag description.


# Tag Description Fields

@docs TagDescriptionField, empty, fromString, raw, validationError, setBody, setValidationError

-}


{-| A tag description field.
-}
type TagDescriptionField
    = TagDescriptionField
        { body : String
        , validationError : Maybe String
        }


{-| Construct an empty tag description field.

    empty |> raw --> ""

    empty |> validationError --> Nothing

-}
empty : TagDescriptionField
empty =
    TagDescriptionField
        { body = ""
        , validationError = Nothing
        }


{-| Construct a tag description field from a string.

    fromString "NA" |> raw --> "NA"

    fromString "Foo" |> validationError --> Nothing

-}
fromString : String -> TagDescriptionField
fromString body0 =
    TagDescriptionField
        { body = body0
        , validationError = Nothing
        }


{-| Retrieve the raw body of a tag description field.
-}
raw : TagDescriptionField -> String
raw tagDescriptionField =
    case tagDescriptionField of
        TagDescriptionField field ->
            field.body


{-| Retrieve the validation error of a tag description field.
-}
validationError : TagDescriptionField -> Maybe String
validationError tagDescriptionField =
    case tagDescriptionField of
        TagDescriptionField field ->
            field.validationError


{-| Set the body on a tag description field.

    empty
    |> setBody "TBD"
    |> raw
    --> "TBD"

-}
setBody : String -> TagDescriptionField -> TagDescriptionField
setBody body0 tagDescriptionField =
    case tagDescriptionField of
        TagDescriptionField field ->
            TagDescriptionField { field | body = body0 }


{-| Set the validation error on a tag description field.

    empty
    |> setValidationError (Just "Can't be empty")
    |> validationError
    --> Just "Can't be empty"

-}
setValidationError : Maybe String -> TagDescriptionField -> TagDescriptionField
setValidationError error tagDescriptionField =
    case tagDescriptionField of
        TagDescriptionField field ->
            TagDescriptionField { field | validationError = error }
