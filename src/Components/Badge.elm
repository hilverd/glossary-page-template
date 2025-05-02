module Components.Badge exposing (indigoWithBorderAndRemoveButton, indigoWithCheckbox, withRemoveButton)

import Accessibility exposing (Attribute, Html, button, checkbox, span, text)
import Accessibility.Key
import Html.Attributes exposing (class, for, id)
import Html.Events
import Icons
import Internationalisation as I18n
import Svg.Attributes


indigoWithBorderAndRemoveButton : Bool -> List (Attribute Never) -> msg -> List (Html msg) -> Html msg
indigoWithBorderAndRemoveButton tabbable additionalAttributes onClick children =
    span
        (class "inline-flex items-center gap-x-0.5 rounded-full bg-indigo-100 dark:bg-indigo-900 px-2 py-1 text-indigo-800 dark:text-indigo-100 select-none" :: additionalAttributes)
        [ span
            [ class "mr-0.5 whitespace-nowrap" ]
            children
        , button
            [ Html.Attributes.type_ "button"
            , Accessibility.Key.tabbable tabbable
            , class "group relative -mr-1 h-3.5 w-3.5 rounded-lg hover:bg-indigo-600/20 dark:hover:bg-indigo-200/20"
            , Html.Events.onClick onClick
            ]
            [ span
                [ class "sr-only" ]
                [ text I18n.delete ]
            , Icons.xMark
                [ Svg.Attributes.class "h-3.5 w-3.5 stroke-indigo-700/50 dark:stroke-indigo-200/50 group-hover:stroke-indigo-700/75 dark:group-hover:stroke-indigo-200/75" ]
            , span
                [ class "absolute -inset-1" ]
                []
            ]
        ]


indigoWithCheckbox :
    { tabbable : Bool, checked : Bool }
    -> String
    -> msg
    -> List (Attribute Never)
    -> List (Html msg)
    -> Html msg
indigoWithCheckbox { tabbable, checked } id_ onClick additionalAttributes children =
    span
        (class "inline-flex items-center gap-x-0.5 rounded-full bg-indigo-100 dark:bg-indigo-900 px-1 text-indigo-800 dark:text-indigo-100 select-none" :: additionalAttributes)
        [ span
            [ class "inline-flex items-center px-2 py-2" ]
            [ checkbox
                "select"
                (Just checked)
                [ class "h-4 w-4 text-indigo-600 focus:ring-indigo-500"
                , id id_
                , Html.Events.onClick onClick
                , Accessibility.Key.tabbable tabbable
                ]
            ]
        , Accessibility.label
            [ class "-ml-px block w-full py-1.5 pr-3 text-indigo-600 focus:ring-indigo-600 leading-6 whitespace-nowrap"
            , for id_
            ]
            children
        ]


withRemoveButton : msg -> List (Attribute Never) -> List (Html msg) -> Html msg
withRemoveButton onClick additionalAttributes children =
    span
        (class "inline-flex items-center gap-x-0.5 rounded-full border border-gray-300 dark:border-gray-500 bg-white dark:bg-gray-900 px-2 py-1 text-gray-700 dark:text-gray-200 shadow-xs" :: additionalAttributes)
        [ span
            [ class "whitespace-nowrap" ]
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
