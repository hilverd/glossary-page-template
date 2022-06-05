module Extras.HtmlEvents exposing
    ( KeyValue
    , controlKey
    , downArrowKey
    , enterKey
    , escapeKey
    , onClickPreventDefault
    , onClickPreventDefaultAndStopPropagation
    , onClickStopPropagation
    , onEnterKey
    , onEscapeKey
    , onKeydown
    , upArrowKey
    )

import Html exposing (Attribute)
import Html.Events
import Json.Decode as Decode


type KeyValue
    = Character Char
    | Control String


toKeyValue : String -> KeyValue
toKeyValue string =
    case String.uncons string of
        Just ( char, "" ) ->
            Character char

        _ ->
            Control string


onKeydown : (KeyValue -> Maybe msg) -> Attribute msg
onKeydown f =
    Html.Events.custom "keydown" <|
        (Decode.field "key" Decode.string
            |> Decode.map toKeyValue
            |> Decode.andThen
                (\code ->
                    case f code of
                        Just msg ->
                            Decode.succeed { message = msg, stopPropagation = True, preventDefault = True }

                        Nothing ->
                            Decode.fail "no message for key"
                )
        )


upArrowKey : KeyValue
upArrowKey =
    Control "ArrowUp"


downArrowKey : KeyValue
downArrowKey =
    Control "ArrowDown"


enterKey : KeyValue
enterKey =
    Control "Enter"


controlKey : KeyValue
controlKey =
    Control "Control"


escapeKey : KeyValue
escapeKey =
    Control "Escape"


onEnterKey : msg -> Attribute msg
onEnterKey msg =
    onKeydown
        (\code ->
            if code == enterKey then
                Just msg

            else
                Nothing
        )


onEscapeKey : msg -> Attribute msg
onEscapeKey msg =
    onKeydown
        (\key ->
            if key == escapeKey then
                Just msg

            else
                Nothing
        )


onClickStopPropagation : msg -> Attribute msg
onClickStopPropagation msg =
    Html.Events.custom "click" <|
        Decode.succeed { message = msg, stopPropagation = True, preventDefault = False }


onClickPreventDefault : msg -> Attribute msg
onClickPreventDefault msg =
    Html.Events.custom "click" <|
        Decode.succeed { message = msg, stopPropagation = False, preventDefault = True }


onClickPreventDefaultAndStopPropagation : msg -> Attribute msg
onClickPreventDefaultAndStopPropagation msg =
    Html.Events.custom "click" <|
        Decode.succeed { message = msg, stopPropagation = True, preventDefault = True }
