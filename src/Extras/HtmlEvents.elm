module Extras.HtmlEvents exposing (enter, escape, onEnter, onEscape, onKeydown)

import Html exposing (Attribute)
import Html.Events exposing (keyCode)
import Json.Decode as Decode


onKeydown : (Int -> Maybe msg) -> Attribute msg
onKeydown f =
    Html.Events.custom "keydown" <|
        Decode.andThen
            (\code ->
                case f code of
                    Just msg ->
                        Decode.succeed { message = msg, stopPropagation = True, preventDefault = True }

                    Nothing ->
                        Decode.fail "no message for key"
            )
            keyCode


enter : Int
enter =
    13


escape : Int
escape =
    27


onEnter : msg -> Attribute msg
onEnter msg =
    onKeydown
        (\code ->
            if code == enter then
                Just msg

            else
                Nothing
        )


onEscape : msg -> Attribute msg
onEscape msg =
    onKeydown
        (\code ->
            if code == escape then
                Just msg

            else
                Nothing
        )
