module Data.AboutLink exposing (AboutLink, create, href, body, decode)

{-| A link in the "about" section at the top of a glossary.


# Links in the "About" Section

@docs AboutLink, create, href, body, decode

-}

import Json.Decode as Decode exposing (Decoder)


{-| An opaque type representing a link in the "about" section.
-}
type AboutLink
    = AboutLink
        { href : String
        , body : String
        }


{-| Decode an `AboutLink` from a JSON representation of what's in the HTML file for the glossary.
-}
decode : Decoder AboutLink
decode =
    Decode.map2 create
        (Decode.field "href" <| Decode.string)
        (Decode.field "body" <| Decode.string)


{-| Get the `href` part representing the link URL.

    create "https://github.com" "GitHub" |> href
        --> "https://github.com"

-}
href : AboutLink -> String
href aboutLink =
    case aboutLink of
        AboutLink link ->
            link.href


{-| Get the `body` part representing the link text.

    create "https://github.com" "GitHub" |> body
    --> "GitHub"

-}
body : AboutLink -> String
body aboutLink =
    case aboutLink of
        AboutLink link ->
            link.body


{-| Create an `AboutLink` from a link URL and a link body.
-}
create : String -> String -> AboutLink
create href0 body0 =
    AboutLink { href = href0, body = body0 }
