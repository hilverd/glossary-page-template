module Extras.HtmlEvents exposing
    ( KeyDownEvent
    , KeyValue
    , isControlK
    , isDownArrow
    , isE
    , isEnd
    , isEnter
    , isEscape
    , isHome
    , isLeftArrow
    , isMetaK
    , isN
    , isRightArrow
    , isUpArrow
    , onClickPreventDefault
    , onClickPreventDefaultAndStopPropagation
    , onClickStopPropagation
    , onEnter
    , onEscape
    , preventDefaultOnDecoder
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
    , isFormField : Bool
    }


toKeyValue : String -> KeyValue
toKeyValue string =
    case String.uncons string of
        Just ( char, "" ) ->
            Character char

        _ ->
            Control string


isFormFieldDecoder : Decode.Decoder Bool
isFormFieldDecoder =
    Decode.at [ "target", "tagName" ] Decode.string
        |> Decode.map
            (\tagName ->
                let
                    upperTagName : String
                    upperTagName =
                        String.toUpper tagName
                in
                upperTagName == "INPUT" || upperTagName == "TEXTAREA"
            )


preventDefaultOnDecoder : (KeyDownEvent -> Maybe ( msg, Bool )) -> Decode.Decoder ( msg, Bool )
preventDefaultOnDecoder f =
    Decode.map4 KeyDownEvent
        (Decode.field "key" <| Decode.map toKeyValue <| Decode.string)
        (Decode.field "ctrlKey" <| Decode.bool)
        (Decode.field "metaKey" <| Decode.bool)
        isFormFieldDecoder
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
    Decode.map4 KeyDownEvent
        (Decode.field "key" <| Decode.map toKeyValue <| Decode.string)
        (Decode.field "ctrlKey" <| Decode.bool)
        (Decode.field "metaKey" <| Decode.bool)
        isFormFieldDecoder
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


isUpArrow : KeyDownEvent -> Bool
isUpArrow { keyValue, controlKey, metaKey } =
    keyValue == Control "ArrowUp" && not controlKey && not metaKey


isDownArrow : KeyDownEvent -> Bool
isDownArrow { keyValue, controlKey, metaKey } =
    keyValue == Control "ArrowDown" && not controlKey && not metaKey


isLeftArrow : KeyDownEvent -> Bool
isLeftArrow { keyValue, controlKey, metaKey } =
    keyValue == Control "ArrowLeft" && not controlKey && not metaKey


isRightArrow : KeyDownEvent -> Bool
isRightArrow { keyValue, controlKey, metaKey } =
    keyValue == Control "ArrowRight" && not controlKey && not metaKey


isEnter : KeyDownEvent -> Bool
isEnter { keyValue, controlKey, metaKey } =
    keyValue == Control "Enter" && not controlKey && not metaKey


isEscape : KeyDownEvent -> Bool
isEscape { keyValue, controlKey, metaKey } =
    keyValue == Control "Escape" && not controlKey && not metaKey


isHome : KeyDownEvent -> Bool
isHome { keyValue, controlKey, metaKey } =
    keyValue == Control "Home" && not controlKey && not metaKey


isEnd : KeyDownEvent -> Bool
isEnd { keyValue, controlKey, metaKey } =
    keyValue == Control "End" && not controlKey && not metaKey


isControlK : KeyDownEvent -> Bool
isControlK { keyValue, controlKey, metaKey } =
    keyValue == Character 'k' && controlKey && not metaKey


isMetaK : KeyDownEvent -> Bool
isMetaK { keyValue, controlKey, metaKey } =
    keyValue == Character 'k' && not controlKey && metaKey


isE : KeyDownEvent -> Bool
isE { keyValue, controlKey, metaKey } =
    keyValue == Character 'e' && not controlKey && not metaKey


isN : KeyDownEvent -> Bool
isN { keyValue, controlKey, metaKey } =
    keyValue == Character 'n' && not controlKey && not metaKey


onEnter : msg -> Attribute msg
onEnter msg =
    onKeydown
        (\event ->
            if isEnter event then
                Just msg

            else
                Nothing
        )


onEscape : msg -> Attribute msg
onEscape msg =
    onKeydown
        (\event ->
            if isEscape event then
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
