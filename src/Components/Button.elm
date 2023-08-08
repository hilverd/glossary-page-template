module Components.Button exposing (emptyState, primary, radio, rounded, secondary, soft, softSmall, text, toggle, white)

import Accessibility exposing (Attribute)
import Accessibility.Aria
import Accessibility.Key
import Extras.HtmlAttribute
import Html exposing (Html)
import Html.Attributes exposing (class)


withAdditionalAttributes :
    List (Attribute msg)
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
withAdditionalAttributes attributes additionalAttributes children =
    Accessibility.button
        (Html.Attributes.type_ "button"
            :: (attributes ++ additionalAttributes)
        )
        children


primary : Bool -> List (Attribute msg) -> List (Html msg) -> Html msg
primary enabled =
    withAdditionalAttributes
        [ if enabled then
            class "inline-flex justify-center py-2 px-4 border border-transparent shadow-sm font-medium rounded-md text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 dark:focus:ring-indigo-800 dark:focus:ring-offset-indigo-300"

          else
            class "inline-flex justify-center py-2 px-4 border border-transparent shadow-sm font-medium rounded-md text-white dark:text-gray-400 bg-indigo-400 dark:bg-indigo-900 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 dark:focus:ring-indigo-800 dark:focus:ring-offset-indigo-300"
        , Html.Attributes.disabled <| not enabled
        , Accessibility.Key.tabbable enabled
        ]


secondary : List (Attribute msg) -> List (Html msg) -> Html msg
secondary =
    withAdditionalAttributes
        [ class "inline-flex justify-center items-center px-4 py-2 border border-transparent shadow-sm font-medium rounded-md text-indigo-700 dark:text-indigo-100 bg-indigo-100 dark:bg-indigo-900 hover:bg-indigo-200 dark:hover:bg-indigo-800 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 dark:focus:ring-indigo-800 dark:focus:ring-offset-indigo-300" ]


white : Bool -> List (Attribute msg) -> List (Html msg) -> Html msg
white enabled =
    withAdditionalAttributes
        [ class "inline-flex justify-center items-center rounded-md border border-gray-300 dark:border-gray-700 shadow-sm px-4 py-2 font-medium"
        , if enabled then
            class "bg-white dark:bg-gray-800 text-gray-700 dark:text-gray-300 hover:bg-gray-50 dark:hover:bg-gray-900 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-offset-gray-100 dark:focus:ring-offset-gray-900 focus:ring-indigo-500 dark:focus:ring-indigo-800 dark:focus:ring-offset-indigo-300"

          else
            class "text-gray-300 dark:text-slate-600 bg-white dark:bg-slate-900"
        , Html.Attributes.disabled <| not enabled
        , Accessibility.Key.tabbable enabled
        ]


text : List (Attribute msg) -> List (Html msg) -> Html msg
text =
    withAdditionalAttributes
        [ class "inline-flex items-center space-x-2 font-medium text-gray-600 dark:text-gray-300 hover:text-gray-900 dark:hover:text-gray-400" ]


emptyState : List (Attribute msg) -> List (Html msg) -> Html msg
emptyState =
    withAdditionalAttributes
        [ class "relative block max-w-lg border-2 border-gray-300 dark:border-gray-700 border-dashed rounded-lg p-5 text-center hover:border-gray-400 dark:hover:border-gray-600 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 dark:focus:ring-indigo-800 dark:focus:ring-offset-indigo-300" ]


rounded : Bool -> List (Attribute msg) -> List (Html msg) -> Html msg
rounded enabled =
    withAdditionalAttributes
        [ class "inline-flex items-center p-1.5 mr-2 border border-gray-300 dark:border-gray-500 shadow-sm rounded-full text-gray-700 dark:text-gray-300 bg-white dark:bg-gray-800"
        , if enabled then
            class "hover:bg-gray-100 dark:hover:bg-gray-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 dark:focus:ring-indigo-800 dark:focus:ring-offset-indigo-300"

          else
            class "opacity-50"
        , Html.Attributes.disabled <| not enabled
        , Accessibility.Key.tabbable enabled
        ]


softSmall : Bool -> List (Attribute msg) -> List (Html msg) -> Html msg
softSmall enabled =
    withAdditionalAttributes
        [ class "rounded-full bg-indigo-50 dark:bg-indigo-900 px-2 py-1 text-sm text-indigo-700 dark:text-indigo-100 shadow-sm"
        , Extras.HtmlAttribute.showIf enabled <| class "hover:bg-indigo-100 dark:hover:bg-indigo-700"
        , Html.Attributes.disabled <| not enabled
        , Accessibility.Key.tabbable enabled
        ]


soft : Bool -> List (Attribute msg) -> List (Html msg) -> Html msg
soft enabled =
    withAdditionalAttributes
        [ class "rounded-full bg-indigo-100 dark:bg-indigo-900 px-2 py-1 text-indigo-700 dark:text-indigo-100 shadow-sm"
        , if enabled then
            class "hover:bg-indigo-200 dark:hover:bg-indigo-700"

          else
            class "opacity-50"
        , Html.Attributes.disabled <| not enabled
        , Accessibility.Key.tabbable enabled
        ]


toggle : Bool -> String -> List (Attribute msg) -> List (Html msg) -> Html msg
toggle on labelId additionalAttributes children =
    Html.div
        (class "flex items-center" :: additionalAttributes)
        [ Accessibility.button
            [ Html.Attributes.type_ "button"
            , class "relative inline-flex shrink-0 h-6 w-11 border-2 border-transparent rounded-full cursor-pointer motion-reduce:transition-none transition-colors ease-in-out duration-200 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 dark:focus:ring-indigo-800 dark:focus:ring-offset-indigo-300"
            , class <|
                if on then
                    "bg-indigo-600 dark:bg-indigo-400"

                else
                    "bg-gray-200 dark:bg-gray-400"
            , Html.Attributes.attribute "role" "switch"
            , Accessibility.Aria.checked <| Just on
            , Accessibility.Aria.labelledBy labelId
            ]
            [ Accessibility.span
                [ Accessibility.Aria.hidden True
                , class "pointer-events-none inline-block h-5 w-5 rounded-full bg-white dark:bg-gray-700 shadow transform motion-reduce:transform-none ring-0 transition ease-in-out duration-200"
                , class <|
                    if on then
                        "translate-x-5"

                    else
                        "translate-x-0"
                ]
                []
            ]
        , Accessibility.span
            [ class "ml-3 select-none"
            , Html.Attributes.id labelId
            ]
            [ Accessibility.span
                [ class "font-medium text-gray-900 dark:text-gray-300" ]
                children
            ]
        ]


radio : String -> String -> Bool -> Bool -> List (Attribute msg) -> Html msg
radio name_ value_ checked_ tabbable additionalAttributes =
    Accessibility.radio
        name_
        value_
        checked_
        ([ class "h-4 w-4 disabled:bg-gray-200 dark:bg-gray-700 disabled:dark:bg-gray-800 text-indigo-600 dark:text-amber-700 border-gray-300 dark:border-gray-500 disabled:dark:border-gray-700 focus:ring-indigo-500 dark:focus:ring-indigo-800 dark:focus:ring-offset-indigo-300"
         , Accessibility.Key.tabbable tabbable
         ]
            ++ additionalAttributes
        )
