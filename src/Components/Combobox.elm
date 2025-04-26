module Components.Combobox exposing (Model, Msg, choice, choicesVisible, hideChoices, id, init, onBlur, onInput, onSelect, subscriptions, update, view)

import Accessibility
import Accessibility.Aria
import Accessibility.Key
import Accessibility.Role
import Browser.Dom as Dom
import Browser.Events as Events
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



-- MODEL


type alias Config parentMsg =
    { id : Maybe String
    , onSelect : Maybe (String -> parentMsg)
    , onInput : Maybe (String -> parentMsg)
    , onBlur : Maybe parentMsg
    }


type Model
    = Model
        { choicesVisible : Bool
        , activeChoice : Maybe ChoiceIndex
        }


init : Model
init =
    Model
        { choicesVisible = False
        , activeChoice = Nothing
        }


choicesVisible : Model -> Bool
choicesVisible (Model model) =
    model.choicesVisible


hideChoices : Model -> Model
hideChoices (Model model) =
    Model { model | choicesVisible = False }



-- UPDATE


type Msg
    = NoOp
    | StartShowingChoices String
    | ShowChoices String
    | HideChoices
    | ActivateChoice ChoiceIndex
    | DeactivateChoice ChoiceIndex


update : (Model -> parentModel) -> (Msg -> parentMsg) -> Msg -> Model -> ( parentModel, Cmd parentMsg )
update updateParentModel toParentMsg msg (Model model) =
    let
        ( model1, cmd ) =
            case msg of
                NoOp ->
                    ( Model model, Cmd.none )

                StartShowingChoices id_ ->
                    if model.choicesVisible == False then
                        ( Model model
                        , Process.sleep 50 |> Task.perform (always <| ShowChoices id_)
                        )

                    else
                        ( Model model, Cmd.none )

                ShowChoices id_ ->
                    ( Model { model | choicesVisible = True }
                    , Dom.focus id_ |> Task.attempt (\_ -> NoOp)
                    )

                HideChoices ->
                    ( Model { model | choicesVisible = False }, Cmd.none )

                ActivateChoice choiceIndex ->
                    ( Model { model | activeChoice = Just choiceIndex }, Cmd.none )

                DeactivateChoice choiceIndex ->
                    ( Model
                        { model
                            | activeChoice =
                                if model.activeChoice == Just choiceIndex then
                                    Nothing

                                else
                                    model.activeChoice
                        }
                    , Cmd.none
                    )
    in
    ( updateParentModel model1, Cmd.map toParentMsg cmd )



-- VIEW


type Property parentMsg
    = Id String
    | OnSelect (String -> parentMsg)
    | OnInput (String -> parentMsg)
    | OnBlur parentMsg


type alias ChoiceIndex =
    Int


type Choice parentMsg
    = Choice
        { value : String
        , body : List (Attribute parentMsg) -> Html parentMsg
        }


choice : String -> (List (Attribute parentMsg) -> Html parentMsg) -> Choice parentMsg
choice value body =
    Choice { value = value, body = body }


id : String -> Property msg
id =
    Id


onSelect : (String -> parentMsg) -> Property parentMsg
onSelect =
    OnSelect


onInput : (String -> parentMsg) -> Property parentMsg
onInput =
    OnInput


onBlur : parentMsg -> Property parentMsg
onBlur =
    OnBlur


configFromProperties : List (Property parentMsg) -> Config parentMsg
configFromProperties =
    List.foldl
        (\property config ->
            case property of
                Id string ->
                    { config | id = Just string }

                OnSelect onSelect_ ->
                    { config | onSelect = Just onSelect_ }

                OnInput onInput_ ->
                    { config | onInput = Just onInput_ }

                OnBlur onBlur_ ->
                    { config | onBlur = Just onBlur_ }
        )
        { id = Nothing
        , onSelect = Nothing
        , onInput = Nothing
        , onBlur = Nothing
        }


view : (Msg -> parentMsg) -> Model -> List (Property parentMsg) -> Maybe String -> List (Choice parentMsg) -> String -> Html parentMsg
view toParentMsg (Model model) properties inputForSelectedChoice choices input =
    let
        config : Config parentMsg
        config =
            configFromProperties properties

        id_ : String
        id_ =
            config.id |> Maybe.withDefault "combobox"

        optionsId : String
        optionsId =
            id_ ++ "-options"
    in
    div []
        [ div
            [ class "relative"
            ]
            [ Accessibility.inputText
                input
                [ Extras.HtmlAttribute.showMaybe Html.Attributes.id config.id
                , Html.Attributes.type_ "text"
                , class "block w-full rounded-md bg-white dark:bg-gray-900 py-1.5 pr-12 pl-3 text-gray-900 dark:text-gray-200 outline-1 -outline-offset-1 outline-gray-300 dark:outline-gray-600 placeholder:text-gray-400 dark:placeholder:text-gray-500 focus:outline-1 focus:-outline-offset-1 focus:outline-indigo-600 dark:focus:outline-indigo-300"
                , Accessibility.Role.comboBox
                , Accessibility.Aria.controls [ optionsId ]
                , Accessibility.Aria.expanded model.choicesVisible
                , Html.Attributes.autocomplete False
                , attribute "autocorrect" "off"
                , attribute "autocapitalize" "off"
                , Html.Attributes.spellcheck False
                , Extras.HtmlAttribute.showMaybe Html.Events.onInput config.onInput
                , Html.Events.onFocus <| toParentMsg <| StartShowingChoices id_
                , Html.Events.onClick <| toParentMsg <| StartShowingChoices id_
                , Extras.HtmlAttribute.showMaybe Html.Events.onBlur config.onBlur
                ]
            , button
                [ Html.Attributes.type_ "button"
                , class "absolute inset-y-0 right-0 flex items-center rounded-r-md px-2 focus:outline-hidden"
                , Html.Events.onMouseDown <|
                    toParentMsg <|
                        if model.choicesVisible == False then
                            StartShowingChoices id_

                        else
                            HideChoices
                ]
                [ Icons.chevronUpDown
                    [ Svg.Attributes.class "size-6 text-gray-400 dark:text-gray-500"
                    , Accessibility.Aria.hidden True
                    , attribute "data-slot" "icon"
                    ]
                ]
            , ul
                [ class "absolute z-10 mt-1 max-h-60 w-full overflow-auto rounded-md bg-white dark:bg-gray-900 py-1 ring-1 shadow-lg ring-black/5 dark:ring-gray-100/5 focus:outline-hidden"
                , Html.Attributes.id optionsId
                , Accessibility.Role.listBox
                , Extras.HtmlAttribute.showUnless (model.choicesVisible && List.length choices > 0) <| class "hidden"
                ]
                (choices
                    |> List.indexedMap
                        (\choiceIndex choice_ ->
                            case choice_ of
                                Choice { body, value } ->
                                    let
                                        selected =
                                            Just value == inputForSelectedChoice

                                        active : Bool
                                        active =
                                            model.activeChoice == Just choiceIndex
                                    in
                                    li
                                        [ class "relative cursor-default py-2 pr-9 pl-3 dark:text-white select-none"
                                        , if active then
                                            class "text-white bg-indigo-600 dark:bg-indigo-300 outline-hidden"

                                          else
                                            class "text-gray-900 dark:text-gray-200"
                                        , attribute "role" "option"
                                        , Accessibility.Key.tabbable False
                                        , Html.Events.onMouseEnter <| toParentMsg <| ActivateChoice choiceIndex
                                        , Html.Events.onMouseLeave <| toParentMsg <| DeactivateChoice choiceIndex
                                        , Extras.HtmlAttribute.showMaybe
                                            (\onSelect_ ->
                                                Extras.HtmlEvents.onMouseDownStopPropagation <| onSelect_ value
                                            )
                                            config.onSelect
                                        ]
                                        [ span
                                            [ class "block truncate"
                                            , Extras.HtmlAttribute.showIf selected <| class "font-semibold"
                                            ]
                                            [ body
                                                [ if active then
                                                    class "text-white dark:text-gray-900 bg-indigo-600 dark:bg-indigo-300 outline-hidden"

                                                  else
                                                    class "text-gray-900 dark:text-gray-200"
                                                ]
                                            ]
                                        , Extras.Html.showIf selected <|
                                            span
                                                [ class "absolute inset-y-0 right-0 flex items-center pr-4"
                                                , if active then
                                                    class "text-white dark:text-gray-900"

                                                  else
                                                    class "text-indigo-600 dark:text-indigo-300"
                                                ]
                                                [ Icons.check
                                                    [ Svg.Attributes.class "size-5"
                                                    , Accessibility.Aria.hidden True
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
subscriptions (Model model) =
    if model.choicesVisible then
        Events.onMouseDown <| Decode.succeed HideChoices

    else
        Sub.none
