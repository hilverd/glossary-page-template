module GlossaryItemForm.DetailsField exposing (DetailsField, emptyPlaintext, fromPlaintext, raw, validationError, setValidationError)

{-| A form field that contains details (i.e. definitions) for a glossary item.
This can be in either plain text or Markdown.


# Details Fields

@docs DetailsField, emptyPlaintext, fromPlaintext, raw, validationError, setValidationError

-}


{-| A details field.
-}
type DetailsField
    = PlaintextDetailsField
        { body : String
        , validationError : Maybe String
        }


{-| Construct an empty plain text details field.

    empty |> raw --> ""

    empty |> validationError --> Nothing

-}
emptyPlaintext : DetailsField
emptyPlaintext =
    PlaintextDetailsField
        { body = "", validationError = Nothing }


{-| Construct a details field from a plain text string.

    "Hello" |> fromPlaintext |> raw --> "Hello"

    "Hello" |> fromPlaintext |> validationError --> Nothing

-}
fromPlaintext : String -> DetailsField
fromPlaintext body0 =
    PlaintextDetailsField
        { body = String.trim body0, validationError = Nothing }


{-| Retrieve the raw body of a details field.
-}
raw : DetailsField -> String
raw detailsField =
    case detailsField of
        PlaintextDetailsField field ->
            field.body


{-| Retrieve the validation error from a details field.
-}
validationError : DetailsField -> Maybe String
validationError detailsField =
    case detailsField of
        PlaintextDetailsField field ->
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
        PlaintextDetailsField field ->
            PlaintextDetailsField { field | validationError = error }
