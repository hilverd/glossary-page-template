module Data.GlossaryItem.Details exposing (Details, fromPlaintext, raw)

{-| A definition for a glossary item.
This can be in either plain text or Markdown.


# Details

@docs Details, fromPlaintext, raw

-}


{-| Details (i.e. a definition) for a glossary item.
-}
type Details
    = PlaintextDetails { body : String }


{-| Construct details from a plain text string.

    fromPlaintext "Foo" |> raw --> "Foo"

-}
fromPlaintext : String -> Details
fromPlaintext body =
    PlaintextDetails { body = body }


{-| Retrieve the raw body of details.
-}
raw : Details -> String
raw details =
    case details of
        PlaintextDetails d ->
            d.body
