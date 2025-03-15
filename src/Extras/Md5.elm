module Extras.Md5 exposing (hexWithCrlfToLf)

import MD5


hexWithCrlfToLf : String -> String
hexWithCrlfToLf =
    String.replace "\u{000D}\n" "\n"
        >> MD5.hex
