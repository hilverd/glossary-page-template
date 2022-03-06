module Extras.HtmlEvents exposing (onEnter, onEscape)

import Html exposing (Attribute)
import Html.Events exposing (keyCode)
import Json.Decode as Decode


onEnter : msg -> Attribute msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Decode.succeed { message = msg, stopPropagation = True, preventDefault = True }

            else
                Decode.fail "not ENTER"
    in
    Html.Events.custom "keydown" <| Decode.andThen isEnter keyCode


onEscape : msg -> Attribute msg
onEscape msg =
    let
        isEscape code =
            if code == 27 then
                Decode.succeed { message = msg, stopPropagation = True, preventDefault = True }

            else
                Decode.fail "not ESCAPE"
    in
    Html.Events.custom "keydown" <| Decode.andThen isEscape keyCode
