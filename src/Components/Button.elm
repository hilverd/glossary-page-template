module Components.Button exposing (emptyState, primary, radio, rounded, roundedWithoutBorder, secondary, soft, softLarge, softSmall, text, textWrapNormal, toggle, white)

import Accessibility exposing (Attribute)
import Accessibility.Aria
import Accessibility.Key
import Extras.HtmlAttribute
import Html exposing (Html)
import Html.Attributes exposing (class)
import Internationalisation as I18n
import Svg exposing (path, svg)
import Svg.Attributes as SvgAttr


withAdditionalAttributes :
    List (Attribute msg)
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
withAdditionalAttributes attributes additionalAttributes children =
    Accessibility.button
        ([ Html.Attributes.type_ "button"
         , class "select-none overflow-hidden text-ellipsis"
         ]
            ++ (attributes ++ additionalAttributes)
        )
        children


primary : Bool -> List (Attribute msg) -> List (Html msg) -> Html msg
primary enabled =
    withAdditionalAttributes
        [ class "inline-flex justify-center items-center py-2 px-4 border border-transparent shadow-xs font-medium rounded-md bg-indigo-600 text-white dark:text-gray-200 focus:outline-hidden focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 dark:focus:ring-indigo-800 dark:focus:ring-offset-indigo-300"
        , if enabled then
            class "hover:bg-indigo-700"

          else
            class "opacity-50"
        , Html.Attributes.disabled <| not enabled
        , Accessibility.Key.tabbable enabled
        ]


secondary : List (Attribute msg) -> List (Html msg) -> Html msg
secondary =
    withAdditionalAttributes
        [ class "inline-flex justify-center items-center px-4 py-2 border border-transparent shadow-xs font-medium rounded-md text-indigo-700 dark:text-indigo-100 bg-indigo-100 dark:bg-indigo-900 hover:bg-indigo-200 dark:hover:bg-indigo-800 focus:outline-hidden focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 dark:focus:ring-indigo-800 dark:focus:ring-offset-indigo-300" ]


white : Bool -> List (Attribute msg) -> List (Html msg) -> Html msg
white enabled =
    withAdditionalAttributes
        [ class "inline-flex justify-center items-center rounded-md border border-gray-300 dark:border-gray-700 shadow-xs px-4 py-2 font-medium"
        , if enabled then
            class "bg-white dark:bg-gray-800 text-gray-700 dark:text-gray-300 hover:bg-gray-50 dark:hover:bg-gray-900 focus:outline-hidden focus:ring-2 focus:ring-offset-2 focus:ring-offset-gray-100 dark:focus:ring-offset-gray-900 focus:ring-indigo-500 dark:focus:ring-indigo-800 dark:focus:ring-offset-indigo-300"

          else
            class "text-gray-300 dark:text-slate-600 bg-white dark:bg-slate-900"
        , Html.Attributes.disabled <| not enabled
        , Accessibility.Key.tabbable enabled
        ]


text : List (Attribute msg) -> List (Html msg) -> Html msg
text =
    withAdditionalAttributes
        [ class "inline-flex items-center space-x-2 font-medium text-gray-600 dark:text-gray-300 hover:text-gray-900 dark:hover:text-gray-100" ]


textWrapNormal : List (Attribute msg) -> List (Html msg) -> Html msg
textWrapNormal additionalAttributes children =
    Accessibility.button
        ([ Html.Attributes.type_ "button"
         , class "select-none inline-flex items-center space-x-2 font-medium text-gray-600 dark:text-gray-300 hover:text-gray-900 dark:hover:text-gray-400"
         ]
            ++ additionalAttributes
        )
        children


roundedWithoutBorder : Bool -> List (Attribute msg) -> List (Html msg) -> Html msg
roundedWithoutBorder enabled =
    withAdditionalAttributes
        [ class "flex items-center rounded-full text-gray-500 dark:text-gray-400"
        , if enabled then
            class "hover:text-black dark:hover:text-white focus:outline-hidden focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 dark:focus:ring-indigo-800 dark:focus:ring-offset-indigo-300"

          else
            class "opacity-50"
        , Html.Attributes.disabled <| not enabled
        , Accessibility.Key.tabbable enabled
        ]


emptyState : List (Attribute msg) -> List (Html msg) -> Html msg
emptyState =
    withAdditionalAttributes
        [ class "relative block max-w-lg border-2 border-gray-300 dark:border-gray-700 border-dashed rounded-lg p-5 text-center hover:border-gray-400 dark:hover:border-gray-600 focus:outline-hidden focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 dark:focus:ring-indigo-800 dark:focus:ring-offset-indigo-300" ]


rounded : Bool -> List (Attribute msg) -> List (Html msg) -> Html msg
rounded enabled =
    withAdditionalAttributes
        [ class "inline-flex items-center p-1.5 mr-2 border border-gray-300 dark:border-gray-500 shadow-xs rounded-full text-gray-700 dark:text-gray-300 bg-white dark:bg-gray-800"
        , if enabled then
            class "hover:bg-gray-100 dark:hover:bg-gray-700 focus:outline-hidden focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 dark:focus:ring-indigo-800 dark:focus:ring-offset-indigo-300"

          else
            class "opacity-50"
        , Html.Attributes.disabled <| not enabled
        , Accessibility.Key.tabbable enabled
        ]


softSmall : Bool -> List (Attribute msg) -> List (Html msg) -> Html msg
softSmall enabled =
    withAdditionalAttributes
        [ class "inline-flex items-center rounded-full border border-gray-300 dark:border-gray-500 bg-white dark:bg-gray-800 px-2 py-1 text-sm text-gray-700 dark:text-gray-100 shadow-xs"
        , Extras.HtmlAttribute.showIf enabled <| class "hover:bg-gray-100 dark:hover:bg-gray-700"
        , Html.Attributes.disabled <| not enabled
        , Accessibility.Key.tabbable enabled
        ]


soft : Bool -> List (Attribute msg) -> List (Html msg) -> Html msg
soft enabled =
    withAdditionalAttributes
        [ class "inline-flex items-center rounded-full max-w-xs border border-gray-300 dark:border-gray-500 bg-white dark:bg-gray-900 px-2 py-1 text-gray-700 dark:text-gray-200 shadow-xs"
        , if enabled then
            class "hover:bg-gray-100 dark:hover:bg-gray-800"

          else
            class "opacity-25"
        , Html.Attributes.disabled <| not enabled
        , Accessibility.Key.tabbable enabled
        ]


softLarge : Bool -> List (Attribute msg) -> List (Html msg) -> Html msg
softLarge enabled =
    withAdditionalAttributes
        [ class "rounded-full max-w-md text-xl border border-gray-300 dark:border-gray-500 bg-white dark:bg-gray-900 px-4.5 py-4 text-gray-700 dark:text-gray-200 shadow-xs"
        , if enabled then
            class "hover:bg-gray-100 dark:hover:bg-gray-800"

          else
            class "opacity-25"
        , Html.Attributes.disabled <| not enabled
        , Accessibility.Key.tabbable enabled
        ]


toggle : Bool -> String -> List (Attribute msg) -> List (Html msg) -> Html msg
toggle on labelId additionalAttributes children =
    Html.div
        (class "flex items-center" :: additionalAttributes)
        [ Accessibility.button
            [ Html.Attributes.type_ "button"
            , class "relative inline-flex h-6 w-11 shrink-0 cursor-pointer rounded-full border-2 border-transparent transition-colors duration-200 ease-in-out focus:outline-hidden focus:ring-2 focus:ring-indigo-600 focus:ring-offset-2"
            , class <|
                if on then
                    "bg-indigo-600 dark:bg-indigo-400"

                else
                    "bg-gray-200 dark:bg-gray-600"
            , Html.Attributes.attribute "role" "switch"
            , Accessibility.Aria.checked <| Just on
            , Accessibility.Aria.labelledBy labelId
            ]
            [ Accessibility.span
                [ class "sr-only"
                ]
                [ Html.text I18n.useSetting ]
            , Accessibility.span
                [ class "pointer-events-none relative inline-block size-5 transform rounded-full bg-white shadow-xs ring-0 transition duration-200 ease-in-out"
                , class <|
                    if on then
                        "translate-x-5"

                    else
                        "translate-x-0"
                ]
                [ Accessibility.span
                    [ class "absolute inset-0 flex size-full items-center justify-center transition-opacity"
                    , class <|
                        if on then
                            "opacity-0 duration-100 ease-out"

                        else
                            "opacity-100 duration-200 ease-in"
                    , Accessibility.Aria.hidden True
                    ]
                    [ svg
                        [ SvgAttr.class "size-3 text-gray-400"
                        , SvgAttr.fill "none"
                        , SvgAttr.viewBox "0 0 12 12"
                        ]
                        [ path
                            [ SvgAttr.d "M4 8l2-2m0 0l2-2M6 6L4 4m2 2l2 2"
                            , SvgAttr.stroke "currentColor"
                            , SvgAttr.strokeWidth "2"
                            , SvgAttr.strokeLinecap "round"
                            , SvgAttr.strokeLinejoin "round"
                            ]
                            []
                        ]
                    ]
                , Accessibility.span
                    [ class "absolute inset-0 flex size-full items-center justify-center transition-opacity"
                    , class <|
                        if on then
                            "opacity-100 duration-200 ease-in"

                        else
                            "opacity-0 duration-100 ease-out"
                    , Accessibility.Aria.hidden True
                    ]
                    [ svg
                        [ SvgAttr.class "size-3 text-indigo-600 dark:text-indigo-800"
                        , SvgAttr.fill "currentColor"
                        , SvgAttr.viewBox "0 0 12 12"
                        ]
                        [ path
                            [ SvgAttr.d "M3.707 5.293a1 1 0 00-1.414 1.414l1.414-1.414zM5 8l-.707.707a1 1 0 001.414 0L5 8zm4.707-3.293a1 1 0 00-1.414-1.414l1.414 1.414zm-7.414 2l2 2 1.414-1.414-2-2-1.414 1.414zm3.414 2l4-4-1.414-1.414-4 4 1.414 1.414z"
                            ]
                            []
                        ]
                    ]
                ]
            ]
        , Accessibility.span
            [ class "ml-3"
            , Html.Attributes.id labelId
            ]
            [ Accessibility.span
                [ class "font-medium text-gray-900 dark:text-gray-300 select-none" ]
                children
            ]
        ]


radio : String -> String -> Bool -> Bool -> List (Attribute msg) -> Html msg
radio name_ value_ checked_ tabbable additionalAttributes =
    Accessibility.radio
        name_
        value_
        checked_
        ([ class "h-4 w-4 disabled:bg-gray-200 dark:bg-gray-700 dark:disabled:bg-gray-800 text-indigo-600 dark:text-amber-700 border-gray-300 dark:border-gray-500 dark:disabled:border-gray-700 focus:ring-indigo-500 dark:focus:ring-indigo-800 dark:focus:ring-offset-indigo-300"
         , Accessibility.Key.tabbable tabbable
         ]
            ++ additionalAttributes
        )
