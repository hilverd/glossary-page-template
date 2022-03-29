module Components.Form exposing (input, inputText, textarea)

import Accessibility exposing (Attribute)
import Accessibility.Aria
import Extras.Html
import Extras.HtmlTree exposing (HtmlTree(..))
import Html exposing (Html)
import Html.Attributes exposing (class)
import Icons
import Svg.Attributes


input : String -> Bool -> Maybe String -> List (Attribute msg) -> Html msg
input value_ showValidationErrors validationError additionalAttributes =
    Accessibility.div []
        [ Html.input
            ([ class "block w-full border-0 p-0 focus:ring-0 dark:bg-gray-700 dark:text-white"
             , if not showValidationErrors || validationError == Nothing then
                class "text-gray-900 placeholder-gray-500 dark:placeholder-gray-400"

               else
                class "text-red-900 dark:text-red-300 placeholder-red-300 dark:placeholder-orange-700"
             , Html.Attributes.value value_
             , Accessibility.Aria.invalid <| validationError /= Nothing
             ]
                ++ additionalAttributes
            )
            []
        , Extras.Html.showIf (showValidationErrors && validationError /= Nothing) <|
            Accessibility.div [ class "absolute inset-y-0 right-0 pr-3 flex items-center pointer-events-none" ]
                [ Icons.exclamationCircle
                    [ Svg.Attributes.class "h-5 w-5 text-red-500 dark:text-red-400" ]
                ]
        ]


inputText : String -> Bool -> Maybe String -> List (Attribute msg) -> Html msg
inputText value_ showValidationErrors validationError additionalAttributes =
    Accessibility.div []
        [ Accessibility.inputText value_
            ([ if not showValidationErrors || validationError == Nothing then
                class "w-full min-w-0 rounded-md focus:ring-indigo-500 focus:border-indigo-500 border-gray-300 dark:border-gray-500 dark:bg-gray-700 dark:text-white"

               else
                class "w-full min-w-0 rounded-md border-red-300 dark:border-red-700 dark:bg-gray-700 text-red-900 dark:text-red-300 placeholder-red-300 dark:placeholder-red-700 focus:outline-none focus:ring-red-500 focus:border-red-500"
             , Accessibility.Aria.invalid <| validationError /= Nothing
             ]
                ++ additionalAttributes
            )
        , Extras.Html.showIf (showValidationErrors && validationError /= Nothing) <|
            Accessibility.div
                [ class "absolute inset-y-0 right-0 pr-3 flex items-center pointer-events-none" ]
                [ Icons.exclamationCircle
                    [ Svg.Attributes.class "h-5 w-5 text-red-500 dark:text-red-400" ]
                ]
        ]


textarea : String -> Bool -> Maybe String -> List (Attribute msg) -> Html msg
textarea body showValidationErrors validationError additionalAttributes =
    Html.div []
        [ Accessibility.div
            [ class "grow-wrap max-w-prose"
            , Html.Attributes.attribute "data-replicated-value" <| body ++ "\n"
            ]
            [ Accessibility.textarea
                ([ if not showValidationErrors || validationError == Nothing then
                    class "shadow-sm w-full rounded-md border border-gray-300 dark:border-gray-500 focus:ring-indigo-500 focus:border-indigo-500 dark:bg-gray-700 dark:text-white"

                   else
                    class "shadow-sm w-full rounded-md border-red-300 text-red-900 dark:text-red-300 placeholder-red-300 dark:placeholder-red-700 focus:outline-none focus:ring-red-500 focus:border-red-500 dark:bg-gray-700"
                 , Accessibility.Aria.invalid <| validationError /= Nothing
                 ]
                    ++ additionalAttributes
                )
                [ Html.text body ]
            ]
        , Extras.Html.showIf (showValidationErrors && validationError /= Nothing) <|
            Accessibility.div
                [ class "absolute inset-y-0 right-0 pr-3 flex items-center pointer-events-none" ]
                [ Icons.exclamationCircle
                    [ Svg.Attributes.class "h-5 w-5 text-red-500 dark:text-red-400" ]
                ]
        ]
