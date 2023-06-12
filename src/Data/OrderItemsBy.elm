module Data.OrderItemsBy exposing (OrderItemsBy(..), decode)

import Json.Decode as Decode exposing (Decoder)


type OrderItemsBy
    = Alphabetically
    | MostMentionedFirst


decode : Decoder OrderItemsBy
decode =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "alphabetically" ->
                        Decode.succeed Alphabetically

                    "most-mentioned-first" ->
                        Decode.succeed MostMentionedFirst

                    somethingElse ->
                        Decode.fail <| "Unknown order: " ++ somethingElse
            )
