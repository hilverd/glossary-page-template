module Components.ModalDialog exposing (view)

import Accessibility exposing (Html, div, span, text)
import Accessibility.Aria
import Accessibility.Role
import Extras.HtmlAttribute
import Extras.HtmlEvents
import Html
import Html.Attributes exposing (class)
import Html.Events


view : msg -> String -> Html msg -> Bool -> Html msg
view onEscape labelledBy body visible =
    Html.div
        [ class "fixed z-20 inset-0 overflow-y-auto print:hidden"
        , Extras.HtmlAttribute.showIf (not visible) <| class "invisible"
        , Extras.HtmlEvents.onEscape onEscape
        , Accessibility.Aria.labelledBy labelledBy
        , Accessibility.Role.dialog
        , Accessibility.Aria.modal True
        ]
        [ div
            [ class "flex items-end justify-center min-h-screen pt-4 px-4 pb-20 text-center sm:block sm:p-0" ]
            [ Html.div
                [ class "fixed inset-0 bg-gray-500 dark:bg-gray-800 bg-opacity-75 dark:bg-opacity-75 transition-opacity motion-reduce:transition-none"
                , if visible then
                    class "ease-out duration-300 opacity-100"

                  else
                    class "ease-in duration-200 opacity-0"
                , Accessibility.Aria.hidden True
                , Html.Events.onClick onEscape
                ]
                []
            , span
                [ class "hidden sm:inline-block sm:align-middle sm:h-screen"
                , Accessibility.Aria.hidden True
                ]
                [ text "\u{200B}" ]
            , div
                [ class "inline-block align-bottom bg-white dark:bg-gray-700 rounded-lg px-4 pt-5 pb-4 text-left overflow-hidden shadow-xl transform motion-reduce:transform-none transition-all motion-reduce:transition-none sm:my-8 sm:align-middle sm:max-w-lg sm:w-full sm:p-6"
                , if visible then
                    class "ease-out duration-300 opacity-100 translate-y-0 sm:scale-100"

                  else
                    class "ease-in duration-200 opacity-0 translate-y-4 sm:translate-y-0 sm:scale-95"
                ]
                [ body ]
            ]
        ]
