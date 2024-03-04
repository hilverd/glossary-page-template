module Extras.HtmlEvents exposing
    ( KeyDownEvent
    , KeyValue
    , controlK
    , downArrow
    , e
    , end
    , enter
    , escape
    , home
    , leftArrow
    , metaK
    , n
    , onClickPreventDefault
    , onClickPreventDefaultAndStopPropagation
    , onClickStopPropagation
    , onEnter
    , onEscape
    , preventDefaultOnDecoder
    , rightArrow
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
    , metaKey : Bool -- on macOS, this is the ⌘ key
    }


toKeyValue : String -> KeyValue
toKeyValue string =
    case String.uncons string of
        Just ( char, "" ) ->
            Character char

        _ ->
            Control string


preventDefaultOnDecoder : (KeyDownEvent -> Maybe ( msg, Bool )) -> Decode.Decoder ( msg, Bool )
preventDefaultOnDecoder f =
    Decode.map3 KeyDownEvent
        (Decode.field "key" <| Decode.map toKeyValue <| Decode.string)
        (Decode.field "ctrlKey" <| Decode.bool)
        (Decode.field "metaKey" <| Decode.bool)
        |> Decode.andThen
            (\code ->
                case f code of
                    Just msgAndBool ->
                        Decode.succeed msgAndBool

                    Nothing ->
                        Decode.fail "no message for key"
            )


detailedKeyDownEventDecoder :
    (KeyDownEvent -> Maybe msg)
    -> Decode.Decoder { message : msg, stopPropagation : Bool, preventDefault : Bool }
detailedKeyDownEventDecoder f =
    Decode.map3 KeyDownEvent
        (Decode.field "key" <| Decode.map toKeyValue <| Decode.string)
        (Decode.field "ctrlKey" <| Decode.bool)
        (Decode.field "metaKey" <| Decode.bool)
        |> Decode.andThen
            (\code ->
                case f code of
                    Just msg ->
                        Decode.succeed { message = msg, stopPropagation = True, preventDefault = True }

                    Nothing ->
                        Decode.fail "no message for key"
            )


onKeydown : (KeyDownEvent -> Maybe msg) -> Attribute msg
onKeydown =
    Html.Events.custom "keydown" << detailedKeyDownEventDecoder


withoutModifiers : KeyValue -> KeyDownEvent
withoutModifiers keyValue =
    { keyValue = keyValue, controlKey = False, metaKey = False }


upArrow : KeyDownEvent
upArrow =
    Control "ArrowUp" |> withoutModifiers


downArrow : KeyDownEvent
downArrow =
    Control "ArrowDown" |> withoutModifiers


leftArrow : KeyDownEvent
leftArrow =
    Control "ArrowLeft" |> withoutModifiers


rightArrow : KeyDownEvent
rightArrow =
    Control "ArrowRight" |> withoutModifiers


enter : KeyDownEvent
enter =
    Control "Enter" |> withoutModifiers


escape : KeyDownEvent
escape =
    Control "Escape" |> withoutModifiers


home : KeyDownEvent
home =
    Control "Home" |> withoutModifiers


end : KeyDownEvent
end =
    Control "End" |> withoutModifiers


controlK : KeyDownEvent
controlK =
    { keyValue = Character 'k', controlKey = True, metaKey = False }


metaK : KeyDownEvent
metaK =
    { keyValue = Character 'k', controlKey = False, metaKey = True }


e : KeyDownEvent
e =
    { keyValue = Character 'e', controlKey = False, metaKey = False }


n : KeyDownEvent
n =
    { keyValue = Character 'n', controlKey = False, metaKey = False }


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
            if event == escape then
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
