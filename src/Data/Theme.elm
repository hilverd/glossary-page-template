module Data.Theme exposing (Theme(..), codec, decode, toHtmlTreeAttribute)

import Codec exposing (Codec)
import Extras.HtmlTree
import Internationalisation as I18n
import Json.Decode as Decode exposing (Decoder)


type Theme
    = Light
    | Dark
    | System


decode : Decoder Theme
decode =
    Decode.string
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
                        Decode.fail <| I18n.unknownTheme ++ ": " ++ somethingElse
            )


codec : Codec Theme
codec =
    Codec.enum Codec.string
        [ ( "light", Light )
        , ( "dark", Dark )
        , ( "system", System )
        ]


toHtmlTreeAttribute : Theme -> Extras.HtmlTree.Attribute
toHtmlTreeAttribute theme =
    { name = "data-default-theme"
    , value =
        case theme of
            Light ->
                "light"

            Dark ->
                "dark"

            System ->
                "system"
    }
