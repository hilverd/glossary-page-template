module GlossaryItemForm.TermField exposing (TermField, empty, fromString, raw, isAbbreviation, isAbbreviationManuallyOverridden, validationError, setBody, setIsAbbreviation, setIsAbbreviationManuallyOverridden, setValidationError)

{-| A form field that contains a term.


# Term Fields

@docs TermField, empty, fromString, raw, isAbbreviation, isAbbreviationManuallyOverridden, validationError, setBody, setIsAbbreviation, setIsAbbreviationManuallyOverridden, setValidationError

-}


{-| A term field.
-}
type TermField
    = TermField
        { body : String
        , isAbbreviation : Bool
        , isAbbreviationManuallyOverridden : Bool
        , validationError : Maybe String
        }


{-| Construct an empty term field.

    empty |> raw --> ""

    empty |> isAbbreviation --> False

    empty |> isAbbreviationManuallyOverridden --> False

    empty |> validationError --> Nothing

-}
empty : TermField
empty =
    TermField
        { body = ""
        , isAbbreviation = False
        , isAbbreviationManuallyOverridden = False
        , validationError = Nothing
        }


{-| Construct a term field from a string and a Boolean indicating whether the term is an abbreviation.

    fromString "NA" True |> raw --> "NA"

    fromString "NA" True |> isAbbreviation --> True

    fromString "Foo" True |> isAbbreviationManuallyOverridden --> True

    fromString "Foo" True |> validationError --> Nothing

-}
fromString : String -> Bool -> TermField
fromString body0 isAbbreviation0 =
    TermField
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
        TermField field ->
            field.body


{-| Retrieve whether or not the term is an abbreviation.
-}
isAbbreviation : TermField -> Bool
isAbbreviation termField =
    case termField of
        TermField field ->
            field.isAbbreviation


{-| Retrieve whether or not the term's "is an abbreviation" Boolean was manually overridden.
-}
isAbbreviationManuallyOverridden : TermField -> Bool
isAbbreviationManuallyOverridden termField =
    case termField of
        TermField field ->
            field.isAbbreviationManuallyOverridden


{-| Retrieve the validation error of a term field.
-}
validationError : TermField -> Maybe String
validationError termField =
    case termField of
        TermField field ->
            field.validationError


{-| Set the body on a term field.

    empty
    |> setBody "TBD"
    |> raw
    --> "TBD"

    empty
    |> setBody "Hello"
    |> isAbbreviation
    --> False

    empty
    |> setBody "TBD"
    |> isAbbreviation
    --> True

    fromString "OK" True
    |> setBody "Hello"
    |> isAbbreviation
    --> True

-}
setBody : String -> TermField -> TermField
setBody body0 termField =
    case termField of
        TermField field ->
            let
                isAbbreviation0 : Bool
                isAbbreviation0 =
                    if isAbbreviationManuallyOverridden termField then
                        isAbbreviation termField

                    else
                        body0 |> stringLooksLikeAnAbbreviation
            in
            TermField { field | body = body0, isAbbreviation = isAbbreviation0 }


{-| Set the "is an abbreviation" Boolean on a term field.

    fromString "TBD" False
    |> setIsAbbreviation True
    |> isAbbreviation
    --> True

-}
setIsAbbreviation : Bool -> TermField -> TermField
setIsAbbreviation isAbbreviation0 termField =
    case termField of
        TermField field ->
            TermField { field | isAbbreviation = isAbbreviation0 }


{-| Set the "'is an abbreviation' Boolean was manually overridden" on a term field.

    empty
    |> setIsAbbreviationManuallyOverridden True
    |> isAbbreviationManuallyOverridden
    --> True

-}
setIsAbbreviationManuallyOverridden : Bool -> TermField -> TermField
setIsAbbreviationManuallyOverridden manuallyOverridden termField =
    case termField of
        TermField field ->
            TermField { field | isAbbreviationManuallyOverridden = manuallyOverridden }


{-| Set the validation error on a term field.

    empty
    |> setValidationError (Just "Can't be empty")
    |> validationError
    --> Just "Can't be empty"

-}
setValidationError : Maybe String -> TermField -> TermField
setValidationError error termField =
    case termField of
        TermField field ->
            TermField { field | validationError = error }


stringLooksLikeAnAbbreviation : String -> Bool
stringLooksLikeAnAbbreviation string =
    let
        trimmed : String
        trimmed =
            String.trim string
    in
    (not <| String.isEmpty trimmed) && trimmed == String.toUpper trimmed
