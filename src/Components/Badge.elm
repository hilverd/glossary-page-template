module Components.Badge exposing (withBorderAndRemoveButton, withCheckbox)

import Accessibility exposing (Attribute, Html, button, checkbox, span, text)
import Accessibility.Key
import Html exposing (label)
import Html.Attributes exposing (class, for, id)
import Icons
import Svg.Attributes


withBorderAndRemoveButton : Bool -> List (Attribute Never) -> String -> Html msg
withBorderAndRemoveButton tabbable additionalAttributes label =
    span
        (class "inline-flex items-center gap-x-0.5 rounded-full bg-indigo-100 dark:bg-indigo-900 px-2 py-1 font-medium text-indigo-800 dark:text-indigo-100" :: additionalAttributes)
        [ span
            [ class "mr-0.5" ]
            [ text label ]
        , button
            [ Html.Attributes.type_ "button"
            , Accessibility.Key.tabbable tabbable
            , class "group relative -mr-1 h-3.5 w-3.5 rounded-lg hover:bg-indigo-600/20 dark:hover:bg-indigo-200/20"
            ]
            [ span
                [ class "sr-only" ]
                [ text "Remove" ]
            , Icons.xMark
                [ Svg.Attributes.class "h-3.5 w-3.5 stroke-indigo-700/50 dark:stroke-indigo-200/50 group-hover:stroke-indigo-700/75 dark:group-hover:stroke-indigo-200/75" ]
            , span
                [ class "absolute -inset-1" ]
                []
            ]
        ]


withCheckbox : Bool -> String -> List (Attribute Never) -> Html msg
withCheckbox tabbable label additionalAttributes =
    span
        (class "inline-flex items-center gap-x-0.5 rounded-full bg-indigo-100 dark:bg-indigo-900 px-1 font-medium text-indigo-800 dark:text-indigo-100" :: additionalAttributes)
        [ span
            [ class "inline-flex items-center px-2 py-2" ]
            [ checkbox
                "select"
                (Just False)
                [ class "h-4 w-4 text-indigo-600 focus:ring-indigo-500"
                , id "TODO"
                , Accessibility.Key.tabbable tabbable
                ]
            ]
        , Accessibility.label
            [ class "-ml-px block w-full py-1.5 pr-3 text-indigo-600 focus:ring-indigo-600 leading-6"
            , for "TODO"
            ]
            [ text label ]
        ]
