module Uuid exposing (uuidStringGenerator)

{-
   Adapted from https://github.com/danyx23/elm-uuid which is copyright (c) 2015, Daniel Bachler.
-}

import Array
import Bitwise
import Char
import List
import Random exposing (Generator, int, list, map)
import String


uuidStringGenerator : Generator String
uuidStringGenerator =
    map toUuidString (list 31 hexGenerator)


toUuidString : List Int -> String
toUuidString thirtyOneHexDigits =
    String.concat
        [ thirtyOneHexDigits |> List.take 8 |> List.map mapToHex |> String.fromList
        , "-"
        , thirtyOneHexDigits |> List.drop 8 |> List.take 4 |> List.map mapToHex |> String.fromList
        , "-"
        , "4"
        , thirtyOneHexDigits |> List.drop 12 |> List.take 3 |> List.map mapToHex |> String.fromList
        , "-"
        , thirtyOneHexDigits |> List.drop 15 |> List.take 1 |> List.map limitDigitRange8ToB |> List.map mapToHex |> String.fromList
        , thirtyOneHexDigits |> List.drop 16 |> List.take 3 |> List.map mapToHex |> String.fromList
        , "-"
        , thirtyOneHexDigits |> List.drop 19 |> List.take 12 |> List.map mapToHex |> String.fromList
        ]


hexDigits : Array.Array Char
hexDigits =
    let
        mapChars offset digit =
            Char.fromCode <| digit + offset
    in
    List.map (mapChars 48) (List.range 0 9)
        ++ List.map (mapChars 97) (List.range 0 5)
        |> Array.fromList


limitDigitRange8ToB : Int -> Int
limitDigitRange8ToB digit =
    Bitwise.or (Bitwise.and digit 3) 8


{-| Map an integer in the range 0-15 to a hexadecimal character
-}
mapToHex : Int -> Char
mapToHex index =
    let
        maybeResult =
            Array.get index hexDigits
    in
    case maybeResult of
        Nothing ->
            'x'

        Just result ->
            result


hexGenerator : Generator Int
hexGenerator =
    int 0 15
