module Data.OrderItemsBy exposing (OrderItemsBy(..), fromQuery, toQueryParameter)

import Data.GlossaryItem.RawTerm as RawTerm exposing (RawTerm)
import Data.GlossaryItem.TermId as TermId exposing (TermId)
import Url.Builder
import Url.Parser.Query


type OrderItemsBy
    = Alphabetically
    | MostMentionedFirst
    | FocusedOn RawTerm


fromQuery : Url.Parser.Query.Parser OrderItemsBy
fromQuery =
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
                            rawTerm =
                                somethingElse
                                    |> String.slice 11 (String.length somethingElse)
                                    |> RawTerm.fromString
                        in
                        FocusedOn rawTerm

                    else
                        Alphabetically

                _ ->
                    Alphabetically
        )


toQueryParameter : OrderItemsBy -> Maybe Url.Builder.QueryParameter
toQueryParameter orderItemsBy =
    let
        maybeValue =
            case orderItemsBy of
                Alphabetically ->
                    Nothing

                MostMentionedFirst ->
                    Just "most-mentioned-first"

                FocusedOn rawTerm ->
                    Just <| "focused-on-" ++ RawTerm.toString rawTerm
    in
    Maybe.map (Url.Builder.string "order-items-by") maybeValue
