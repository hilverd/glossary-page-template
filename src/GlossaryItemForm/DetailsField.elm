module GlossaryItemForm.DetailsField exposing (DetailsField, empty, fromString, raw, validationError, setValidationError)

{-| A form field that contains details (i.e. definitions) for a glossary item.


# Details Fields

@docs DetailsField, empty, fromString, raw, validationError, setValidationError

-}


{-| A details field.
-}
type DetailsField
    = DetailsField
        { body : String
        , validationError : Maybe String
        }


{-| Construct an empty details field.

    empty |> raw --> ""

    empty |> validationError --> Nothing

-}
empty : DetailsField
empty =
    DetailsField
        { body = "", validationError = Nothing }


{-| Construct a details field from a string.

    "Hello" |> fromString |> raw --> "Hello"

    "Hello" |> fromString |> validationError --> Nothing

-}
fromString : String -> DetailsField
fromString body0 =
    DetailsField
        { body = body0, validationError = Nothing }


{-| Retrieve the raw body of a details field.
-}
raw : DetailsField -> String
raw detailsField =
    case detailsField of
        DetailsField field ->
            field.body


{-| Retrieve the validation error from a details field.
-}
validationError : DetailsField -> Maybe String
validationError detailsField =
    case detailsField of
        DetailsField field ->
            field.validationError


{-| Set the validation error on a details field.

    empty
    |> setValidationError (Just "Can't be empty")
    |> validationError
    --> Just "Can't be empty"

-}
setValidationError : Maybe String -> DetailsField -> DetailsField
setValidationError error detailsField =
    case detailsField of
        DetailsField field ->
            DetailsField { field | validationError = error }
