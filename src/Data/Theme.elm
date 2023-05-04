module Data.Theme exposing (Theme(..), decode)

import Json.Decode as Decode exposing (Decoder)


type Theme
    = Light
    | Dark
    | System


decode : Decoder Theme
decode =
    Decode.field "theme"
        (Decode.string
            |> Decode.andThen
                (\str ->
                    case str of
                        "light" ->
                            Decode.succeed Light

                        "dark" ->
                            Decode.succeed Dark

                        "system" ->
                            Decode.succeed System

                        somethingElse ->
                            Decode.fail <| "Unknown theme: " ++ somethingElse
                )
        )
