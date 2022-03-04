module Data.AboutLink exposing (AboutLink, body, create, decode, href)

import Json.Decode as Decode exposing (Decoder)


type AboutLink
    = AboutLink
        { href : String
        , body : String
        }


decode : Decoder AboutLink
decode =
    Decode.map2 (\href0 body0 -> AboutLink { href = href0, body = body0 })
        (Decode.field "href" <| Decode.string)
        (Decode.field "body" <| Decode.string)


href : AboutLink -> String
href aboutLink =
    case aboutLink of
        AboutLink link ->
            link.href


body : AboutLink -> String
body aboutLink =
    case aboutLink of
        AboutLink link ->
            link.body


create : String -> String -> AboutLink
create href0 body0 =
    AboutLink { href = href0, body = body0 }
