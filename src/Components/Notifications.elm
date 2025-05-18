module Components.Notifications exposing (..)

import Accessibility
import Accessibility.Aria
import Accessibility.Key
import Accessibility.Live
import Accessibility.Role
import Array
import Browser.Dom as Dom
import Browser.Events as Events
import ElementIds
import Extras.Html
import Extras.HtmlAttribute
import Extras.HtmlEvents
import Html exposing (..)
import Html.Attributes exposing (attribute, class)
import Html.Events
import Icons
import Json.Decode as Decode
import Process
import Svg.Attributes
import Task


view : Html msg -> Html msg -> Html msg
view title body =
    {- Global notification live region, render this permanently at the end of the document -}
    div
        [ Accessibility.Live.assertive
        , class "pointer-events-none fixed inset-0 flex items-end px-4 py-6 sm:items-start sm:p-6 z-60"
        ]
        [ div
            [ class "flex w-full flex-col items-center space-y-4 sm:items-end" ]
            [ {-
                 Notification panel, dynamically insert this into the live region when it needs to be displayed

                 Entering: "transform ease-out duration-300 transition"
                   From: "translate-y-2 opacity-0 sm:translate-y-0 sm:translate-x-2"
                   To: "translate-y-0 opacity-100 sm:translate-x-0"
                 Leaving: "transition ease-in duration-100"
                   From: "opacity-100"
                   To: "opacity-0"
              -}
              div
                [ class "pointer-events-auto w-full max-w-sm overflow-hidden rounded-lg bg-white dark:bg-gray-800 shadow-lg ring-1 ring-black/5 dark:ring-white/5" ]
                [ div
                    [ class "p-4" ]
                    [ div
                        [ class "flex items-start" ]
                        [ div
                            [ class "shrink-0" ]
                            [ Icons.checkCircle
                                [ Svg.Attributes.class "size-6 text-green-400"
                                , Accessibility.Aria.hidden True
                                , attribute "data-slot" "icon"
                                ]
                            ]
                        , div
                            [ class "ml-3 w-0 flex-1 pt-0.5" ]
                            [ p
                                [ class "font-medium text-gray-900 dark:text-white" ]
                                [ title ]
                            , p
                                [ class "mt-1 text-gray-500 dark:text-gray-400" ]
                                [ body ]
                            ]
                        , div
                            [ class "ml-4 flex shrink-0" ]
                            [ button
                                [ Html.Attributes.type_ "button"
                                , class "inline-flex rounded-md bg-white dark:bg-gray-800 text-gray-400 hover:text-gray-500 dark:hover:text-gray-300 focus:ring-2 focus:ring-indigo-500 focus:ring-offset-2 dark:focus:ring-offset-gray-800 focus:outline-hidden"
                                ]
                                [ span
                                    [ class "sr-only" ]
                                    [ text "Close" ]
                                , Icons.xMark
                                    [ Svg.Attributes.class "size-5"
                                    , Accessibility.Aria.hidden True
                                    , attribute "data-slot" "icon"
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
