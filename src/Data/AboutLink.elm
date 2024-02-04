module Data.AboutLink exposing (AboutLink, create, href, body, codec)

{-| A link in the "about" section at the top of a glossary.


# Links in the "About" Section

@docs AboutLink, create, href, body, codec

-}

import Codec exposing (Codec)


{-| An opaque type representing a link in the "about" section.
-}
type AboutLink
    = AboutLink
        { href : String
        , body : String
        }


{-| Encode/decode an `AboutLink` from a JSON representation of what's in the HTML file for the glossary.
-}
codec : Codec AboutLink
codec =
    Codec.object
        create
        |> Codec.field "href" href Codec.string
        |> Codec.field "body" body Codec.string
        |> Codec.buildObject


{-| Get the `href` part representing the link URL.

    create "https://github.com" "GitHub" |> href
        --> "https://github.com"

-}
href : AboutLink -> String
href (AboutLink link) =
    link.href


{-| Get the `body` part representing the link text.

    create "https://github.com" "GitHub" |> body
    --> "GitHub"

-}
body : AboutLink -> String
body (AboutLink link) =
    link.body


{-| Create an `AboutLink` from a link URL and a link body.
-}
create : String -> String -> AboutLink
create href0 body0 =
    AboutLink { href = href0, body = body0 }
