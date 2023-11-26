module Data.OrderItemsBy exposing (OrderItemsBy(..), decode, decodeQuery, encode)

import Data.GlossaryItem.TermId as TermId exposing (TermId)
import Dict
import Json.Decode as Decode exposing (Decoder)
import Url.Parser.Query


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


decodeQuery : Url.Parser.Query.Parser OrderItemsBy
decodeQuery =
    Url.Parser.Query.custom "order-items-by"
        (\strings ->
            case strings of
                [ "alphabetically" ] ->
                    Alphabetically

                [ "most-mentioned-first" ] ->
                    MostMentionedFirst

                [ somethingElse ] ->
                    if String.startsWith "focused-on-" somethingElse then
                        let
                            termId =
                                somethingElse
                                    |> String.slice 11 (String.length somethingElse)
                                    |> TermId.fromString
                        in
                        FocusedOn termId

                    else
                        Alphabetically

                _ ->
                    Alphabetically
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
