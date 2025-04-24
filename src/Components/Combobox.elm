module Components.Combobox exposing (Model, Msg, choice, id, init, subscriptions, update, view)

import Accessibility.Aria
import Accessibility.Key
import Accessibility.Role
import Extras.Html
import Extras.HtmlAttribute
import Html exposing (..)
import Html.Attributes exposing (attribute, class)
import Html.Events
import Icons
import Svg.Attributes



-- MODEL


type alias Config =
    { id : Maybe String
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



-- UPDATE


type Msg
    = NoOp
    | ToggleChoicesVisibility
    | ActivateChoice ChoiceIndex
    | DeactivateChoice ChoiceIndex


update : (Model -> parentModel) -> (Msg -> parentMsg) -> Msg -> Model -> ( parentModel, Cmd parentMsg )
update updateParentModel toParentMsg msg (Model model) =
    let
        ( model1, cmd ) =
            case msg of
                NoOp ->
                    ( Model model, Cmd.none )

                ToggleChoicesVisibility ->
                    ( Model { model | choicesVisible = not model.choicesVisible }, Cmd.none )

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


type Property
    = Id String


type alias ChoiceIndex =
    Int


type Choice parentMsg
    = Choice
        { body : List (Attribute parentMsg) -> Html parentMsg
        , selected : Bool
        }


choice : (List (Attribute parentMsg) -> Html parentMsg) -> Bool -> Choice parentMsg
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


view : (Msg -> parentMsg) -> Model -> List Property -> List (Choice parentMsg) -> Html parentMsg
view toParentMsg (Model model) properties choices =
    let
        config : Config
        config =
            configFromProperties properties

        optionsId : String
        optionsId =
            config.id |> Maybe.withDefault "combobox" |> (++) "-options"
    in
    div []
        [ div
            [ class "relative mt-2"
            ]
            [ input
                [ Extras.HtmlAttribute.showMaybe Html.Attributes.id config.id
                , Html.Attributes.type_ "text"
                , class "block w-full rounded-md bg-white dark:bg-gray-900 py-1.5 pr-12 pl-3 text-gray-900 dark:text-gray-200 outline-1 -outline-offset-1 outline-gray-300 dark:outline-gray-600 placeholder:text-gray-400 darkL:placeholder:text-gray-500 focus:outline-1 focus:-outline-offset-1 focus:outline-indigo-600 dark:focus:outline-indigo-300"
                , Accessibility.Role.comboBox
                , Accessibility.Aria.controls [ optionsId ]
                , Accessibility.Aria.expanded model.choicesVisible
                , Html.Attributes.autocomplete False
                , attribute "autocorrect" "off"
                , attribute "autocapitalize" "off"
                , Html.Attributes.spellcheck False
                ]
                []
            , button
                [ Html.Attributes.type_ "button"
                , class "absolute inset-y-0 right-0 flex items-center rounded-r-md px-2 focus:outline-hidden"
                , Html.Events.onClick <| toParentMsg ToggleChoicesVisibility
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
                , Extras.HtmlAttribute.showUnless model.choicesVisible <| class "hidden"
                ]
                (choices
                    |> List.indexedMap
                        (\choiceIndex choice_ ->
                            case choice_ of
                                Choice { body, selected } ->
                                    let
                                        active : Bool
                                        active =
                                            model.activeChoice == Just choiceIndex
                                    in
                                    li
                                        [ class "relative cursor-default py-2 pr-9 pl-3 dark:text-white select-none"
                                        , if active then
                                            class "text-white bg-indigo-600 outline-hidden"

                                          else
                                            class "text-gray-900"
                                        , attribute "role" "option"
                                        , Accessibility.Key.tabbable False
                                        , Html.Events.onMouseEnter <| toParentMsg <| ActivateChoice choiceIndex
                                        , Html.Events.onMouseLeave <| toParentMsg <| DeactivateChoice choiceIndex
                                        ]
                                        [ span
                                            [ class "block truncate"
                                            , Extras.HtmlAttribute.showIf selected <| class "font-semibold"
                                            ]
                                            [ body
                                                [ if active then
                                                    class "text-white bg-indigo-600 outline-hidden"

                                                  else
                                                    class "text-gray-900"
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
subscriptions (Model _) =
    Sub.none
