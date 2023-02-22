module GlossaryItemForm.TermField exposing (TermField, emptyPlaintext, fromPlaintext, raw, isAbbreviation, isAbbreviationManuallyOverridden, validationError, setBody, setIsAbbreviation, setIsAbbreviationManuallyOverridden, setValidationError)

{-| A form field that contains a term.
This can be in either plain text or Markdown.


# Term Fields

@docs TermField, emptyPlaintext, fromPlaintext, raw, isAbbreviation, isAbbreviationManuallyOverridden, validationError, setBody, setIsAbbreviation, setIsAbbreviationManuallyOverridden, setValidationError

-}


{-| A term field.
-}
type TermField
    = PlaintextTermField
        { body : String
        , isAbbreviation : Bool
        , isAbbreviationManuallyOverridden : Bool
        , validationError : Maybe String
        }


{-| Construct an empty plain text term field.

    emptyPlaintext |> raw --> ""

    emptyPlaintext |> isAbbreviation --> False

    emptyPlaintext |> isAbbreviationManuallyOverridden --> False

    emptyPlaintext |> validationError --> Nothing

-}
emptyPlaintext : TermField
emptyPlaintext =
    PlaintextTermField
        { body = ""
        , isAbbreviation = False
        , isAbbreviationManuallyOverridden = False
        , validationError = Nothing
        }


{-| Construct a term field from a plain text string and a Boolean indicating whether the term is an abbreviation.

    fromPlaintext "NA" True |> raw --> "NA"

    fromPlaintext "NA" True |> isAbbreviation --> True

    fromPlaintext "Foo" True |> isAbbreviationManuallyOverridden --> True

    fromPlaintext "Foo" True |> validationError --> Nothing

-}
fromPlaintext : String -> Bool -> TermField
fromPlaintext body0 isAbbreviation0 =
    PlaintextTermField
        { body = body0
        , isAbbreviation = isAbbreviation0
        , isAbbreviationManuallyOverridden = True
        , validationError = Nothing
        }


{-| Retrieve the raw body of a term field.
-}
raw : TermField -> String
raw termField =
    case termField of
        PlaintextTermField field ->
            field.body


{-| Retrieve whether or not the term is an abbreviation.
-}
isAbbreviation : TermField -> Bool
isAbbreviation termField =
    case termField of
        PlaintextTermField field ->
            field.isAbbreviation


{-| Retrieve whether or not the term's "is an abbreviation" Boolean was manually overridden.
-}
isAbbreviationManuallyOverridden : TermField -> Bool
isAbbreviationManuallyOverridden termField =
    case termField of
        PlaintextTermField field ->
            field.isAbbreviationManuallyOverridden


{-| Retrieve the validation error of a term field.
-}
validationError : TermField -> Maybe String
validationError termField =
    case termField of
        PlaintextTermField field ->
            field.validationError


{-| Set the body on a term field.

    emptyPlaintext
    |> setBody "TBD"
    |> raw
    --> "TBD"

    emptyPlaintext
    |> setBody "Hello"
    |> isAbbreviation
    --> False

    emptyPlaintext
    |> setBody "TBD"
    |> isAbbreviation
    --> True

    fromPlaintext "OK" True
    |> setBody "Hello"
    |> isAbbreviation
    --> True

-}
setBody : String -> TermField -> TermField
setBody body0 termField =
    case termField of
        PlaintextTermField field ->
            let
                isAbbreviation0 : Bool
                isAbbreviation0 =
                    if isAbbreviationManuallyOverridden termField then
                        isAbbreviation termField

                    else
                        body0 |> stringLooksLikeAnAbbreviation
            in
            PlaintextTermField { field | body = body0, isAbbreviation = isAbbreviation0 }


{-| Set the "is an abbreviation" Boolean on a term field.

    fromPlaintext "TBD" False
    |> setIsAbbreviation True
    |> isAbbreviation
    --> True

-}
setIsAbbreviation : Bool -> TermField -> TermField
setIsAbbreviation isAbbreviation0 termField =
    case termField of
        PlaintextTermField field ->
            PlaintextTermField { field | isAbbreviation = isAbbreviation0 }


{-| Set the "'is an abbreviation' Boolean was manually overridden" on a term field.

    emptyPlaintext
    |> setIsAbbreviationManuallyOverridden True
    |> isAbbreviationManuallyOverridden
    --> True

-}
setIsAbbreviationManuallyOverridden : Bool -> TermField -> TermField
setIsAbbreviationManuallyOverridden manuallyOverridden termField =
    case termField of
        PlaintextTermField field ->
            PlaintextTermField { field | isAbbreviationManuallyOverridden = manuallyOverridden }


{-| Set the validation error on a term field.

    emptyPlaintext
    |> setValidationError (Just "Can't be empty")
    |> validationError
    --> Just "Can't be empty"

-}
setValidationError : Maybe String -> TermField -> TermField
setValidationError error termField =
    case termField of
        PlaintextTermField field ->
            PlaintextTermField { field | validationError = error }


stringLooksLikeAnAbbreviation : String -> Bool
stringLooksLikeAnAbbreviation string =
    let
        trimmed : String
        trimmed =
            String.trim string
    in
    (not <| String.isEmpty trimmed) && trimmed == String.toUpper trimmed
