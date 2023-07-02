module Components.Badge exposing (withBorderAndRemoveButton)

import Accessibility exposing (Attribute, Html, button, label, span, text)
import Accessibility.Key
import Html.Attributes exposing (class)
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
