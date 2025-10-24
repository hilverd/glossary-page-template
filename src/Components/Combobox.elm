port module Components.Combobox exposing (Choice, Model, Msg, choice, choicesVisible, hideChoices, icon, id, init, onBlur, onInput, onSelect, placeholder, showChoices, showValidationErrors, subscriptions, update, validationError, view)

import Accessibility
import Accessibility.Aria
import Accessibility.Key
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



-- MODEL


type alias Config value parentMsg =
    { id : Maybe String
    , placeholder : Maybe String
    , showValidationErrors : Bool
    , validationError : Maybe String
    , onSelect : Maybe (value -> parentMsg)
    , onInput : Maybe (String -> parentMsg)
    , onBlur : Maybe parentMsg
    , icon : Maybe (Html parentMsg)
    }


type Model
    = Model
        { choicesVisible : Bool
        , activeChoiceIndex : Maybe ChoiceIndex
        }


init : Model
init =
    Model
        { choicesVisible = False
        , activeChoiceIndex = Just 0
        }


choicesVisible : Model -> Bool
choicesVisible (Model model) =
    model.choicesVisible


hideChoices : Model -> Model
hideChoices (Model model) =
    Model { model | choicesVisible = False }


showChoices : Model -> Model
showChoices (Model model) =
    Model { model | choicesVisible = True }



-- PORTS


port scrollChoiceIntoView : String -> Cmd msg



-- UPDATE


type Msg
    = NoOp
    | StartShowingChoices String
    | ShowChoices String
    | HideChoices (Maybe String)
    | ActivateChoice ChoiceIndex
    | DeactivateChoice ChoiceIndex
    | ActivatePreviousOrNextChoice String Int Bool


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

                HideChoices id_ ->
                    ( Model { model | choicesVisible = False }
                    , id_
                        |> Maybe.map (Dom.focus >> Task.attempt (\_ -> NoOp))
                        |> Maybe.withDefault Cmd.none
                    )

                ActivateChoice choiceIndex ->
                    ( Model { model | activeChoiceIndex = Just choiceIndex }, Cmd.none )

                DeactivateChoice choiceIndex ->
                    ( Model
                        { model
                            | activeChoiceIndex =
                                if model.activeChoiceIndex == Just choiceIndex then
                                    Nothing

                                else
                                    model.activeChoiceIndex
                        }
                    , Cmd.none
                    )

                ActivatePreviousOrNextChoice comboboxId numberOfChoices next ->
                    let
                        delta : Int
                        delta =
                            if next then
                                1

                            else
                                -1

                        initial : Int
                        initial =
                            if next then
                                0

                            else
                                numberOfChoices - 1

                        activeChoiceIndex_ : Maybe Int
                        activeChoiceIndex_ =
                            model.activeChoiceIndex
                                |> Maybe.map ((+) delta >> min (numberOfChoices - 1) >> max 0)
                                |> Maybe.withDefault initial
                                |> (\new ->
                                        if numberOfChoices == 0 then
                                            Nothing

                                        else
                                            Just new
                                   )
                    in
                    ( Model { model | activeChoiceIndex = activeChoiceIndex_ }
                    , activeChoiceIndex_
                        |> Maybe.map (\index -> scrollChoiceIntoView <| ElementIds.comboboxChoice comboboxId index)
                        |> Maybe.withDefault Cmd.none
                    )
    in
    ( updateParentModel model1, Cmd.map toParentMsg cmd )



-- VIEW


type Property value parentMsg
    = Id String
    | Placeholder String
    | OnSelect (value -> parentMsg)
    | OnInput (String -> parentMsg)
    | OnBlur parentMsg
    | ShowValidationErrors Bool
    | ValidationError (Maybe String)
    | Icon (Html parentMsg)


type alias ChoiceIndex =
    Int


type Choice value parentMsg
    = Choice
        { value : value
        , body : List (Attribute parentMsg) -> Html parentMsg
        }


choice : value -> (List (Attribute parentMsg) -> Html parentMsg) -> Choice value parentMsg
choice value body =
    Choice { value = value, body = body }


id : String -> Property value parentMsg
id =
    Id


placeholder : String -> Property value parentMsg
placeholder =
    Placeholder


onSelect : (value -> parentMsg) -> Property value parentMsg
onSelect =
    OnSelect


onInput : (String -> parentMsg) -> Property value parentMsg
onInput =
    OnInput


onBlur : parentMsg -> Property value parentMsg
onBlur =
    OnBlur


validationError : Maybe String -> Property value parentMsg
validationError =
    ValidationError


showValidationErrors : Bool -> Property value parentMsg
showValidationErrors =
    ShowValidationErrors


icon : Html parentMsg -> Property value parentMsg
icon =
    Icon


configFromProperties : List (Property value parentMsg) -> Config value parentMsg
configFromProperties =
    List.foldl
        (\property config ->
            case property of
                Id string ->
                    { config | id = Just string }

                Placeholder placeholder_ ->
                    { config | placeholder = Just placeholder_ }

                OnSelect onSelect_ ->
                    { config | onSelect = Just onSelect_ }

                OnInput onInput_ ->
                    { config | onInput = Just onInput_ }

                OnBlur onBlur_ ->
                    { config | onBlur = Just onBlur_ }

                ShowValidationErrors showValidationErrors_ ->
                    { config | showValidationErrors = showValidationErrors_ }

                ValidationError validationError_ ->
                    { config | validationError = validationError_ }

                Icon icon_ ->
                    { config | icon = Just icon_ }
        )
        { id = Nothing
        , placeholder = Nothing
        , onSelect = Nothing
        , onInput = Nothing
        , onBlur = Nothing
        , showValidationErrors = False
        , validationError = Nothing
        , icon = Nothing
        }


view :
    (Msg -> parentMsg)
    -> Model
    -> List (Property value parentMsg)
    -> List (Attribute parentMsg)
    -> Maybe value
    -> List (Choice value parentMsg)
    -> Maybe String
    -> String
    -> Html parentMsg
view toParentMsg (Model model) properties additionalAttributes valueForSelectedChoice choices messageToShowAtBottom input =
    let
        config : Config value parentMsg
        config =
            configFromProperties properties

        id_ : String
        id_ =
            config.id |> Maybe.withDefault "combobox"

        optionsId : String
        optionsId =
            id_ ++ "-options"
    in
    div
        additionalAttributes
        [ div
            [ class "relative" ]
            [ Extras.Html.showMaybe
                (\icon_ ->
                    div
                        [ class "pointer-events-none absolute inset-y-0 left-0 flex items-center pl-3" ]
                        [ icon_ ]
                )
                config.icon
            , Accessibility.inputText
                input
                [ Extras.HtmlAttribute.showMaybe Html.Attributes.id config.id
                , Html.Attributes.type_ "text"
                , class "block w-full rounded-md bg-white dark:bg-gray-900 py-1.5 pr-12 text-gray-900 dark:text-gray-200 outline-1 -outline-offset-1 outline-gray-300 dark:outline-gray-600 placeholder:text-gray-400 dark:placeholder:text-gray-500 focus:outline-1 focus:-outline-offset-1 focus:outline-indigo-600 dark:focus:outline-indigo-300"
                , if config.icon /= Nothing then
                    class "pl-10"

                  else
                    class "pl-3"
                , Accessibility.Role.comboBox
                , Accessibility.Aria.controls [ optionsId ]
                , Accessibility.Aria.expanded model.choicesVisible
                , Html.Attributes.autocomplete False
                , attribute "autocorrect" "off"
                , attribute "autocapitalize" "off"
                , Extras.HtmlAttribute.showMaybe Html.Attributes.placeholder config.placeholder
                , Html.Attributes.spellcheck False
                , Extras.HtmlAttribute.showMaybe Html.Events.onInput config.onInput
                , Extras.HtmlAttribute.showMaybe Html.Events.onBlur config.onBlur
                , Html.Events.preventDefaultOn "keydown"
                    (Extras.HtmlEvents.preventDefaultOnDecoder
                        (\event ->
                            if Extras.HtmlEvents.isDownArrow event then
                                Just
                                    ( toParentMsg <| ActivatePreviousOrNextChoice id_ (List.length choices) True
                                    , True
                                    )

                            else if Extras.HtmlEvents.isUpArrow event then
                                Just
                                    ( toParentMsg <| ActivatePreviousOrNextChoice id_ (List.length choices) False
                                    , True
                                    )

                            else if Extras.HtmlEvents.isHome event then
                                Just
                                    ( toParentMsg <| ActivatePreviousOrNextChoice id_ (List.length choices) False
                                    , True
                                    )

                            else if Extras.HtmlEvents.isEnd event then
                                Just
                                    ( toParentMsg <| ActivatePreviousOrNextChoice id_ (List.length choices) False
                                    , True
                                    )

                            else if Extras.HtmlEvents.isEscape event then
                                Just
                                    ( toParentMsg <| HideChoices (Just id_)
                                    , True
                                    )

                            else if Extras.HtmlEvents.isEnter event then
                                let
                                    maybeActiveChoice : Maybe (Choice value parentMsg)
                                    maybeActiveChoice =
                                        model.activeChoiceIndex
                                            |> Maybe.andThen (\activeChoiceIndex -> choices |> Array.fromList |> Array.get activeChoiceIndex)
                                in
                                case ( maybeActiveChoice, config.onSelect ) of
                                    ( Just (Choice { value }), Just onSelect_ ) ->
                                        Just ( onSelect_ value, True )

                                    _ ->
                                        Nothing

                            else
                                Nothing
                        )
                    )
                ]
            , button
                [ Html.Attributes.type_ "button"
                , class "absolute inset-y-0 right-0 flex items-center rounded-r-md px-2 focus:outline-hidden"
                , Html.Events.onMouseDown <|
                    toParentMsg <|
                        if model.choicesVisible == False then
                            StartShowingChoices id_

                        else
                            HideChoices (Just id_)
                ]
                [ Icons.chevronUpDown
                    [ Svg.Attributes.class "size-6 text-gray-400 dark:text-gray-500"
                    , Accessibility.Aria.hidden True
                    , attribute "data-slot" "icon"
                    ]
                ]
            , div
                [ class "absolute z-10 mt-1 w-full" ]
                [ ul
                    [ class "max-h-60 overflow-x-hidden overflow-y-auto bg-white dark:bg-gray-800 py-1 ring-1 shadow-lg ring-black/5 dark:ring-gray-100/5 focus:outline-hidden"
                    , if messageToShowAtBottom == Nothing then
                        class "rounded-md"

                      else
                        class "rounded-t-md"
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
                                            selected : Bool
                                            selected =
                                                Just value == valueForSelectedChoice

                                            active : Bool
                                            active =
                                                model.activeChoiceIndex == Just choiceIndex
                                        in
                                        li
                                            [ class "relative cursor-default py-2 pr-9 pl-3 dark:text-white select-none"
                                            , if active then
                                                class "text-white bg-indigo-600 dark:bg-indigo-300 outline-hidden"

                                              else
                                                class "text-gray-900 dark:text-gray-200"
                                            , attribute "role" "option"
                                            , Accessibility.Key.tabbable False
                                            , Html.Attributes.id <| ElementIds.comboboxChoice id_ choiceIndex
                                            , Html.Events.onMouseEnter <| toParentMsg <| ActivateChoice choiceIndex
                                            , Html.Events.onMouseLeave <| toParentMsg <| DeactivateChoice choiceIndex
                                            , Extras.HtmlAttribute.showMaybe
                                                (\onSelect_ ->
                                                    Extras.HtmlEvents.onMouseDownStopPropagation <| onSelect_ value
                                                )
                                                config.onSelect
                                            ]
                                            [ span
                                                [ class "block truncate" ]
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
                , Extras.Html.showMaybe
                    (\message ->
                        div
                            [ class "cursor-default border border-gray-300 shadow-lg dark:border-gray-700 rounded-b-md bg-gray-100 dark:bg-gray-800 text-sm py-2 pr-9 pl-3 select-none text-gray-900 dark:text-gray-200"
                            , Extras.HtmlAttribute.showUnless model.choicesVisible <| class "hidden"
                            ]
                            [ text message ]
                    )
                    messageToShowAtBottom
                ]
            ]
        , Extras.Html.showMaybe
            (\validationError1 ->
                p
                    [ class "mt-2 text-red-600 dark:text-red-400" ]
                    [ text validationError1 ]
            )
            (if config.showValidationErrors then
                config.validationError

             else
                Nothing
            )
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions (Model model) =
    if model.choicesVisible then
        Events.onMouseDown <| Decode.succeed <| HideChoices Nothing

    else
        Sub.none
