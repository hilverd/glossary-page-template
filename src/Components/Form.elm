module Components.Form exposing (input, inputText, textarea)

import Accessibility exposing (Attribute)
import Accessibility.Aria
import Extras.Html
import Extras.HtmlAttribute
import Html exposing (Html, span)
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
            Accessibility.div
                [ class "absolute inset-y-0 right-0 pr-3 flex items-center pointer-events-none" ]
                [ Icons.exclamationCircle
                    [ Svg.Attributes.class "h-5 w-5 text-red-500 dark:text-red-400" ]
                ]
        ]


inputText : String -> Bool -> Bool -> Bool -> Maybe String -> List (Attribute msg) -> Html msg
inputText value_ markdownBasedSyntaxEnabled mathSupportEnabled showValidationErrors validationError additionalAttributes =
    Accessibility.div []
        [ Extras.Html.showIf markdownBasedSyntaxEnabled <| markdownSupportedMessage mathSupportEnabled
        , Accessibility.div
            [ class "relative" ]
            [ Accessibility.inputText value_
                ([ if not showValidationErrors || validationError == Nothing then
                    class "w-full min-w-0 rounded-md focus:ring-indigo-500 focus:border-indigo-500 border-gray-300 dark:border-gray-500 dark:bg-gray-700 dark:text-white placeholder-gray-500 dark:placeholder-gray-400"

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
        ]


textarea : String -> Bool -> Bool -> Bool -> Maybe String -> List (Attribute msg) -> Html msg
textarea body markdownBasedSyntaxEnabled mathSupportEnabled showValidationErrors validationError additionalAttributes =
    Accessibility.div []
        [ Extras.Html.showIf markdownBasedSyntaxEnabled <| markdownSupportedMessage mathSupportEnabled
        , Accessibility.div
            [ class "grow-wrap"
            , Html.Attributes.attribute "data-replicated-value" <| body ++ "\n\n"
            ]
            [ Accessibility.textarea
                ([ if not showValidationErrors || validationError == Nothing then
                    class "shadow-sm w-full rounded-md border border-gray-300 dark:border-gray-500 focus:ring-indigo-500 focus:border-indigo-500 dark:bg-gray-700 dark:text-white placeholder-gray-500 dark:placeholder-gray-400"

                   else
                    class "shadow-sm w-full rounded-md border-red-300 text-red-900 dark:text-red-300 placeholder-red-300 dark:placeholder-red-700 focus:outline-none focus:ring-red-500 focus:border-red-500 dark:bg-gray-700"
                 , Accessibility.Aria.invalid <| validationError /= Nothing
                 , Extras.HtmlAttribute.showIf markdownBasedSyntaxEnabled <| class "font-mono text-sm"
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


markdownSupportedMessage : Bool -> Html msg
markdownSupportedMessage mathSupportEnabled =
    Accessibility.div
        []
        [ span
            [ class "inline-flex items-center text-sm text-gray-500 dark:text-gray-300" ]
            [ Icons.markdown
                [ Svg.Attributes.class "w-5 h-5 mr-2"
                , Accessibility.Aria.hidden True
                ]
            , Html.span
                []
                [ Html.a
                    [ Html.Attributes.href "https://commonmark.org/help/"
                    , Html.Attributes.target "_blank"
                    , class "text-inherit no-underline text-gray-500 dark:text-gray-400 font-normal"
                    ]
                    [ Html.text "Markdown" ]
                , Extras.Html.showIf mathSupportEnabled <|
                    Html.span []
                        [ Html.text " and "
                        , Html.a
                            [ Html.Attributes.href "https://katex.org/docs/supported.html"
                            , Html.Attributes.target "_blank"
                            , class "text-inherit no-underline text-gray-500 dark:text-gray-400 font-normal"
                            ]
                            [ Html.node "katex-inline"
                                [ Html.Attributes.attribute "data-expr" "\\TeX"
                                ]
                                []
                            ]
                        ]
                , Html.text " supported."
                ]
            ]
        ]
