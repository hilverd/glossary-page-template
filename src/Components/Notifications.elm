module Components.Notifications exposing (Model, Msg, addNotification, init, subscriptions, update, view)

import Accessibility.Aria
import Accessibility.Live
import Browser.Events
import Data.GradualVisibility exposing (GradualVisibility(..))
import Extras.Html
import Html exposing (..)
import Html.Attributes exposing (attribute, class)
import Html.Events
import Icons
import Process
import Svg.Attributes
import Task
import Time



-- MODEL


type alias Notification =
    { title : Html Never
    , body : Html Never
    }


type Model
    = NotPresent { counter : Int, notification : Maybe Notification }
    | EnterStart { counter : Int, notification : Notification }
    | EnterEnd { counter : Int, notification : Notification }
    | Present { counter : Int, notification : Notification }
    | LeaveStart { counter : Int, notification : Notification }
    | LeaveEnd { counter : Int, notification : Notification }


init : Model
init =
    NotPresent { counter = 0, notification = Nothing }


counter_ : Model -> Int
counter_ model =
    case model of
        NotPresent { counter } ->
            counter

        EnterStart { counter } ->
            counter

        EnterEnd { counter } ->
            counter

        Present { counter } ->
            counter

        LeaveStart { counter } ->
            counter

        LeaveEnd { counter } ->
            counter


notification_ : Model -> Maybe Notification
notification_ model =
    case model of
        NotPresent { notification } ->
            notification

        EnterStart { notification } ->
            Just notification

        EnterEnd { notification } ->
            Just notification

        Present { notification } ->
            Just notification

        LeaveStart { notification } ->
            Just notification

        LeaveEnd { notification } ->
            Just notification


addNotification : Html Never -> Html Never -> Model -> ( Model, Cmd Msg )
addNotification title body model =
    let
        newCounter : Int
        newCounter =
            counter_ model + 1
    in
    ( EnterStart
        { counter = newCounter
        , notification = { title = title, body = body }
        }
    , Task.perform
        (always <| StartLeaving newCounter)
        (Process.sleep 3000)
    )



-- UPDATE


type Msg
    = AnimationFrame Time.Posix
    | StartLeaving Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnimationFrame _ ->
            case model of
                NotPresent _ ->
                    ( model, Cmd.none )

                EnterStart data ->
                    ( EnterEnd data, Cmd.none )

                EnterEnd data ->
                    ( Present data, Cmd.none )

                Present data ->
                    ( Present data, Cmd.none )

                LeaveStart data ->
                    ( LeaveEnd data, Cmd.none )

                LeaveEnd { counter, notification } ->
                    ( NotPresent { counter = counter, notification = Just notification }, Cmd.none )

        StartLeaving expectedCounter ->
            ( case model of
                EnterStart { counter, notification } ->
                    if counter == expectedCounter then
                        LeaveStart { counter = counter, notification = notification }

                    else
                        model

                EnterEnd { counter, notification } ->
                    if counter == expectedCounter then
                        LeaveStart { counter = counter, notification = notification }

                    else
                        model

                Present { counter, notification } ->
                    if counter == expectedCounter then
                        LeaveStart { counter = counter, notification = notification }

                    else
                        model

                _ ->
                    model
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ Accessibility.Live.assertive
        , class "pointer-events-none fixed inset-0 flex items-end px-4 py-6 sm:items-start sm:p-6 z-60"
        ]
        [ div
            [ class "flex w-full flex-col items-center space-y-4 sm:items-end" ]
            [ div
                [ class "transition-discrete pointer-events-auto w-full max-w-sm overflow-hidden rounded-lg bg-white dark:bg-gray-800 shadow-lg ring-1 ring-black/5 dark:ring-white/5"
                , case model of
                    EnterStart _ ->
                        class "transform motion-reduce:transform-none ease-out duration-300 transition translate-y-2 opacity-0 sm:translate-y-0 sm:translate-x-2"

                    EnterEnd _ ->
                        class "transform motion-reduce:transform-none ease-out duration-300 transition translate-y-0 opacity-100 sm:translate-x-0"

                    Present _ ->
                        class "transform motion-reduce:transform-none ease-out duration-300 transition translate-y-0 opacity-100 sm:translate-x-0"

                    LeaveStart _ ->
                        class "transition motion-reduce:transition-none ease-in duration-100 opacity-100"

                    LeaveEnd _ ->
                        class "transition motion-reduce:transition-none ease-in duration-100 opacity-0"

                    NotPresent _ ->
                        class "hidden transition motion-reduce:transition-none ease-in duration-100 opacity-0"
                ]
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
                                [ model |> notification_ |> Extras.Html.showMaybe .title |> Html.map never ]
                            , p
                                [ class "mt-1 text-gray-500 dark:text-gray-400" ]
                                [ model |> notification_ |> Extras.Html.showMaybe .body |> Html.map never ]
                            ]
                        , div
                            [ class "ml-4 flex shrink-0" ]
                            [ button
                                [ Html.Attributes.type_ "button"
                                , class "inline-flex rounded-md bg-white dark:bg-gray-800 text-gray-400 hover:text-gray-500 dark:hover:text-gray-300 focus:ring-2 focus:ring-indigo-500 focus:ring-offset-2 dark:focus:ring-offset-gray-800 focus:outline-hidden"
                                , Html.Events.onClick <| StartLeaving <| counter_ model
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        EnterStart _ ->
            Browser.Events.onAnimationFrame AnimationFrame

        EnterEnd _ ->
            Browser.Events.onAnimationFrame AnimationFrame

        LeaveStart _ ->
            Browser.Events.onAnimationFrame AnimationFrame

        LeaveEnd _ ->
            Browser.Events.onAnimationFrame AnimationFrame

        _ ->
            Sub.none
