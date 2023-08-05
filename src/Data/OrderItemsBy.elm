module Data.OrderItemsBy exposing (OrderItemsBy(..), decode, encode)

import Data.GlossaryItem.TermId as TermId exposing (TermId)
import Json.Decode as Decode exposing (Decoder)


type OrderItemsBy
    = Alphabetically
    | MostMentionedFirst
    | FocusedOn TermId


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
                        if String.startsWith "focused-on-" str then
                            let
                                termId =
                                    str
                                        |> String.slice 11 (String.length str)
                                        |> TermId.fromString
                            in
                            Decode.succeed <| FocusedOn termId

                        else
                            Decode.fail <| "Unknown order: " ++ somethingElse
            )


encode : OrderItemsBy -> String
encode orderItemsBy =
    case orderItemsBy of
        Alphabetically ->
            "alphabetically"

        MostMentionedFirst ->
            "most-mentioned-first"

        FocusedOn termId ->
            "focused-on-" ++ TermId.toString termId
