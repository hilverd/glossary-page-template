module Components.Dividers exposing (withLabel)

import Accessibility exposing (div, span, text)
import Accessibility.Aria as Aria
import Html exposing (Html)
import Html.Attributes exposing (class)


withLabel : List (Html.Attribute Never) -> String -> Html msg
withLabel additionalAttributes label =
    div
        (class "relative" :: additionalAttributes)
        [ div
            [ class "absolute inset-0 flex items-center"
            , Aria.hidden True
            ]
            [ div
                [ class "w-full border-t border-gray-300 dark:border-gray-600" ]
                []
            ]
        , div
            [ class "relative flex justify-center" ]
            [ span
                [ class "bg-gray-100 dark:bg-gray-900 px-2 text-gray-600 dark:text-gray-400" ]
                [ text label ]
            ]
        ]
