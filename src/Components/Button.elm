module Components.Button exposing (emptyState, primary, secondary, text, white)

import Accessibility exposing (..)
import Extras.HtmlTree exposing (HtmlTree(..))
import Html.Attributes exposing (class)


primary : List (Attribute msg) -> List (Html msg) -> Html msg
primary attributes =
    button
        ([ Html.Attributes.type_ "button"
         , class "inline-flex justify-center py-2 px-4 border border-transparent shadow-sm font-medium rounded-md text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
         ]
            ++ attributes
        )


secondary : List (Attribute msg) -> List (Html msg) -> Html msg
secondary attributes =
    button
        ([ Html.Attributes.type_ "button"
         , class "inline-flex justify-center items-center px-4 py-2 border border-transparent shadow-sm font-medium rounded-md text-indigo-700 dark:text-indigo-300 bg-indigo-100 dark:bg-indigo-900 hover:bg-indigo-200 dark:hover:bg-indigo-800 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 dark:focus:ring-indigo-800 dark:focus:ring-offset-indigo-300"
         ]
            ++ attributes
        )


white : List (Attribute msg) -> List (Html msg) -> Html msg
white attributes =
    button
        ([ Html.Attributes.type_ "button"
         , class "inline-flex justify-center items-center px-4 py-2 border border-gray-300 dark:border-gray-700 shadow-sm font-medium rounded-md text-gray-700 dark:text-gray-200 bg-white dark:bg-gray-500 hover:bg-gray-50 dark:hover:bg-gray-600 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 dark:focus:ring-offset-indigo-800"
         ]
            ++ attributes
        )


text : List (Attribute msg) -> List (Html msg) -> Html msg
text attributes =
    button
        ([ Html.Attributes.type_ "button"
         , class "inline-flex space-x-2 text-gray-400 dark:text-gray-300 hover:text-gray-500 dark:hover:text-gray-400"
         ]
            ++ attributes
        )


emptyState : List (Attribute msg) -> List (Html msg) -> Html msg
emptyState attributes =
    button
        ([ Html.Attributes.type_ "button"
         , class "relative block max-w-lg border-2 border-gray-300 border-dashed rounded-lg p-5 text-center hover:border-gray-400 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
         ]
            ++ attributes
        )
