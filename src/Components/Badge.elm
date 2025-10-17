module Components.Badge exposing (withRemoveButton, withRemoveButtonAndWrappingText)

import Accessibility exposing (Attribute, Html, button, span, text)
import Html.Attributes exposing (class)
import Html.Events
import Icons
import Internationalisation as I18n
import Svg.Attributes


withRemoveButton : msg -> List (Attribute Never) -> List (Html msg) -> Html msg
withRemoveButton onClick additionalAttributes children =
    span
        (class "inline-flex items-center max-w-xs gap-x-0.5 rounded-full border border-gray-300 dark:border-gray-500 bg-white dark:bg-gray-900 px-3 py-1 text-gray-700 dark:text-gray-200 shadow-xs" :: additionalAttributes)
        [ span
            [ class "whitespace-nowrap overflow-hidden text-ellipsis inline-flex items-center" ]
            children
        , button
            [ Html.Attributes.type_ "button"
            , class "group relative -mr-1 size-3.5 rounded-xs hover:bg-gray-500/20 dark:hover:bg-gray-400/20"
            , Html.Events.onClick onClick
            ]
            [ span
                [ class "sr-only" ]
                [ text I18n.remove ]
            , Icons.xMark
                [ Svg.Attributes.class "size-3.5 stroke-gray-600/50 dark:stroke-gray-300/50 group-hover:stroke-gray-600/75 dark:group-hover:stroke-gray-300/75" ]
            , span
                [ class "absolute -inset-1" ]
                []
            ]
        ]


withRemoveButtonAndWrappingText : msg -> List (Attribute Never) -> List (Html msg) -> Html msg
withRemoveButtonAndWrappingText onClick additionalAttributes children =
    span
        (class "select-none inline-flex items-center max-w-xs gap-x-0.5 rounded-full border border-gray-300 dark:border-gray-500 bg-white dark:bg-gray-900 px-3 py-1 text-gray-700 dark:text-gray-200 shadow-xs" :: additionalAttributes)
        [ span
            [ class "inline-flex items-center" ]
            children
        , button
            [ Html.Attributes.type_ "button"
            , class "group relative -mr-1 size-3.5 rounded-xs hover:bg-gray-500/20 dark:hover:bg-gray-400/20"
            , Html.Events.onClick onClick
            ]
            [ span
                [ class "sr-only" ]
                [ text I18n.remove ]
            , Icons.xMark
                [ Svg.Attributes.class "size-3.5 stroke-gray-600/50 dark:stroke-gray-300/50 group-hover:stroke-gray-600/75 dark:group-hover:stroke-gray-300/75" ]
            , span
                [ class "absolute -inset-1" ]
                []
            ]
        ]
