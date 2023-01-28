module Data.GlossaryItem.RelatedTerm exposing (RelatedTerm, fromPlaintext, decode, raw, idReference)

{-| A related term.


# Related Terms

@docs RelatedTerm, fromPlaintext, decode, raw, idReference

-}

import Json.Decode as Decode exposing (Decoder)


{-| A related term.
-}
type RelatedTerm
    = PlaintextRelatedTerm
        { idReference : String
        , body : String
        }


{-| Construct a related term from plain text.

    fromPlaintext "Device_Edge" "Device Edge"
    |> raw --> "Device Edge"

    fromPlaintext "Device_Edge" "Device Edge"
    |> idReference --> "Device_Edge"

-}
fromPlaintext : String -> String -> RelatedTerm
fromPlaintext idReference0 body =
    PlaintextRelatedTerm
        { idReference = idReference0
        , body = body
        }


{-| Decode a related term from its JSON representation.

    import Json.Decode as Decode exposing (Decoder)
    import Json.Encode as Encode

    deviceEdge : Encode.Value
    deviceEdge =
        Encode.object
            [ ( "idReference", Encode.string "Device_Edge" )
            , ( "body", Encode.string "Device Edge" )
            ]

    decoded : Result Decode.Error RelatedTerm
    decoded = Decode.decodeValue decode deviceEdge

    Result.map idReference decoded --> Ok "Device_Edge"

    Result.map raw decoded --> Ok "Device Edge"

-}
decode : Decoder RelatedTerm
decode =
    Decode.map2 fromPlaintext
        (Decode.field "idReference" Decode.string)
        (Decode.field "body" Decode.string)


{-| Retrieve the ID reference of a related term.
-}
idReference : RelatedTerm -> String
idReference relatedTerm =
    case relatedTerm of
        PlaintextRelatedTerm t ->
            t.idReference


{-| Retrieve the raw body of a related term.
-}
raw : RelatedTerm -> String
raw relatedTerm =
    case relatedTerm of
        PlaintextRelatedTerm t ->
            t.body
