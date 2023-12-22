module Data.CardWidth exposing (CardWidth(..), codec, toHtmlTreeAttribute)

import Codec exposing (Codec)
import Extras.HtmlTree


type CardWidth
    = Compact
    | Intermediate
    | Wide


codec : Codec CardWidth
codec =
    Codec.enum Codec.string
        [ ( "compact", Compact )
        , ( "intermediate", Intermediate )
        , ( "wide", Wide )
        ]


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
