module Components.Combobox exposing (Model, choice, id, init, view)

import Browser.Events as Events
import Extras.HtmlAttribute
import Html exposing (..)
import Html.Attributes exposing (attribute, class)
import Icons
import Json.Decode as Decode
import Svg.Attributes



-- MODEL


type alias Config =
    { id : Maybe String
    }


type Model
    = Model
        { choicesVisible : Bool
        }


init : Model
init =
    Model
        { choicesVisible = False }



-- UPDATE


type Msg
    = NoOp
    | HideChoices



-- VIEW


type Property
    = Id String


type Choice msg
    = Choice
        { body : Html msg
        , selected : Bool
        }


choice : Html msg -> Bool -> Choice msg
choice body selected =
    Choice { body = body, selected = selected }


id : String -> Property
id =
    Id


configFromProperties : List Property -> Config
configFromProperties =
    List.foldl
        (\property config ->
            case property of
                Id string ->
                    { config | id = Just string }
        )
        { id = Nothing
        }


view : Model -> List Property -> List (Choice msg) -> Html msg
view model properties choices =
    let
        config : Config
        config =
            configFromProperties properties
    in
    div []
        [ div
            [ class "relative mt-2"
            ]
            [ input
                [ Extras.HtmlAttribute.showMaybe Html.Attributes.id config.id
                , Html.Attributes.type_ "text"
                , class "block w-full rounded-md bg-white dark:bg-gray-900 py-1.5 pr-12 pl-3 text-gray-900 dark:text-gray-200 outline-1 -outline-offset-1 outline-gray-300 dark:outline-gray-600 placeholder:text-gray-400 darkL:placeholder:text-gray-500 focus:outline-1 focus:-outline-offset-1 focus:outline-indigo-600 dark:focus:outline-indigo-300"
                , attribute "role" "combobox"
                , attribute "aria-controls" "options"
                , attribute "aria-expanded" "false"
                , Html.Attributes.autocomplete False
                , attribute "autocorrect" "off"
                , attribute "autocapitalize" "off"
                , Html.Attributes.spellcheck False
                ]
                []
            , button
                [ Html.Attributes.type_ "button"
                , class "absolute inset-y-0 right-0 flex items-center rounded-r-md px-2 focus:outline-hidden"
                ]
                [ Icons.chevronUpDown
                    [ Svg.Attributes.class "size-6 text-gray-400 dark:text-gray-500"
                    , attribute "aria-hidden" "true"
                    , attribute "data-slot" "icon"
                    ]
                ]
            , ul
                [ class "absolute z-10 mt-1 max-h-60 w-full overflow-auto rounded-md bg-white dark:bg-gray-900 py-1 ring-1 shadow-lg ring-black/5 dark:ring-gray-100/5 focus:outline-hidden"
                , Html.Attributes.id "options"
                , attribute "role" "listbox"
                ]
                (choices
                    |> List.map
                        (\choice_ ->
                            case choice_ of
                                Choice { body, selected } ->
                                    {-
                                       Combobox option, manage highlight styles based on mouseenter/mouseleave and keyboard navigation.

                                       Active: "text-white bg-indigo-600 outline-hidden", Not Active: "text-gray-900"
                                    -}
                                    li
                                        [ class "relative cursor-default py-2 pr-9 pl-3 text-gray-900 dark:text-white select-none"
                                        , Html.Attributes.id "option-0"
                                        , attribute "role" "option"
                                        , Html.Attributes.tabindex -1
                                        ]
                                        [ {- Selected: "font-semibold" -}
                                          span
                                            [ class "block truncate"
                                            ]
                                            [ body ]
                                        , {-
                                             Checkmark, only display for selected option.

                                             Active: "text-white", Not Active: "text-indigo-600"
                                          -}
                                          span
                                            [ class "absolute inset-y-0 right-0 flex items-center pr-4"
                                            , if selected then
                                                class "text-indigo-600 dark:text-indigo-300"

                                              else
                                                class "text-white dark:text-gray-900"
                                            ]
                                            [ Icons.check
                                                [ Svg.Attributes.class "size-5"
                                                , attribute "aria-hidden" "true"
                                                , attribute "data-slot" "icon"
                                                ]
                                            ]
                                        ]
                        )
                )
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions (Model { choicesVisible }) =
    if choicesVisible then
        Events.onClick <| Decode.succeed HideChoices

    else
        Sub.none
