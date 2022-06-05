module Extras.HtmlEvents exposing
    ( KeyDownEvent
    , control
    , downArrow
    , enter
    , escapeKey
    , onClickPreventDefault
    , onClickPreventDefaultAndStopPropagation
    , onClickStopPropagation
    , onEnter
    , onEscape
    , onKeydown
    , upArrow
    )

import Html exposing (Attribute)
import Html.Events
import Json.Decode as Decode


type KeyValue
    = Character Char
    | Control String


type alias KeyDownEvent =
    { keyValue : KeyValue
    , controlKey : Bool
    }


toKeyValue : String -> KeyValue
toKeyValue string =
    case String.uncons string of
        Just ( char, "" ) ->
            Character char

        _ ->
            Control string


onKeydown : (KeyDownEvent -> Maybe msg) -> Attribute msg
onKeydown f =
    Html.Events.custom "keydown" <|
        (Decode.map2 KeyDownEvent
            (Decode.field "key" <| Decode.map toKeyValue <| Decode.string)
            (Decode.field "ctrlKey" <| Decode.bool)
            |> Decode.andThen
                (\code ->
                    case f code of
                        Just msg ->
                            Decode.succeed { message = msg, stopPropagation = True, preventDefault = True }

                        Nothing ->
                            Decode.fail "no message for key"
                )
        )


withoutModifiers : KeyValue -> KeyDownEvent
withoutModifiers keyValue =
    { keyValue = keyValue, controlKey = False }


upArrow : KeyDownEvent
upArrow =
    Control "ArrowUp" |> withoutModifiers


downArrow : KeyDownEvent
downArrow =
    Control "ArrowDown" |> withoutModifiers


enter : KeyDownEvent
enter =
    Control "Enter" |> withoutModifiers


control : KeyDownEvent
control =
    Control "Control" |> withoutModifiers


escapeKey : KeyDownEvent
escapeKey =
    Control "Escape" |> withoutModifiers


onEnter : msg -> Attribute msg
onEnter msg =
    onKeydown
        (\event ->
            if event == enter then
                Just msg

            else
                Nothing
        )


onEscape : msg -> Attribute msg
onEscape msg =
    onKeydown
        (\event ->
            if event == escapeKey then
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
