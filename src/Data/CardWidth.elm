module Data.CardWidth exposing (CardWidth(..), decode, toHtmlTreeAttribute)

import Extras.HtmlTree
import Json.Decode as Decode exposing (Decoder)


type CardWidth
    = Compact
    | Intermediate
    | Wide


decode : Decoder CardWidth
decode =
    Decode.field "cardWidth"
        (Decode.string
            |> Decode.andThen
                (\str ->
                    case str of
                        "compact" ->
                            Decode.succeed Compact

                        "intermediate" ->
                            Decode.succeed Intermediate

                        "wide" ->
                            Decode.succeed Wide

                        somethingElse ->
                            Decode.fail <| "Unknown card width: " ++ somethingElse
                )
        )


toHtmlTreeAttribute : CardWidth -> Extras.HtmlTree.Attribute
toHtmlTreeAttribute cardWidth =
    { name = "data-card-width"
    , value =
        case cardWidth of
            Compact ->
                "compact"

            Intermediate ->
                "intermediate"

            Wide ->
                "wide"
    }
