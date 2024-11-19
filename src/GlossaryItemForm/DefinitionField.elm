module GlossaryItemForm.DefinitionField exposing (DefinitionField, empty, fromString, raw, validationError)

{-| A form field that contains a definition for a glossary item.


# Definition Fields

@docs DefinitionField, empty, fromString, raw, validationError

-}


{-| A definition field.
-}
type DefinitionField
    = DefinitionField
        { body : String
        , validationError : Maybe String
        }


{-| Construct an empty definition field.

    empty |> raw --> ""

    empty |> validationError --> Nothing

-}
empty : DefinitionField
empty =
    DefinitionField
        { body = "", validationError = Nothing }


{-| Construct a details field from a string.

    "Hello" |> fromString |> raw --> "Hello"

    "Hello" |> fromString |> validationError --> Nothing

-}
fromString : String -> DefinitionField
fromString body0 =
    DefinitionField
        { body = body0, validationError = Nothing }


{-| Retrieve the raw body of a details field.
-}
raw : DefinitionField -> String
raw detailsField =
    case detailsField of
        DefinitionField field ->
            field.body


{-| Retrieve the validation error from a details field.
-}
validationError : DefinitionField -> Maybe String
validationError detailsField =
    case detailsField of
        DefinitionField field ->
            field.validationError
