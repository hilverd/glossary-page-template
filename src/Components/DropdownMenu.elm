module Components.DropdownMenu exposing (Choice, Model, Msg, choice, hidden, hide, id, init, update, view, visible)

import Accessibility exposing (..)
import Accessibility.Aria
import Accessibility.Key
import Accessibility.Role
import Array
import Components.Button
import Extras.HtmlAttribute
import Extras.HtmlEvents
import Extras.HtmlTree exposing (HtmlTree(..))
import Html
import Html.Attributes exposing (attribute, class, href)
import Html.Events
import Icons
import Process
import Svg.Attributes
import Task



-- MODEL


type GradualVisibility
    = Visible
    | Disappearing
    | Invisible


type Model
    = Model
        { visibility : GradualVisibility
        , config : Config
        , activeChoice : Maybe ChoiceIndex
        }


hidden : Model -> Model
hidden model =
    case model of
        Model model_ ->
            Model { model_ | visibility = Invisible }


init : List (Property parentMsg) -> Model
init properties =
    Model
        { visibility = Invisible
        , config = configFromProperties properties
        , activeChoice = Nothing
        }


innerModel : Model -> { visibility : GradualVisibility, config : Config, activeChoice : Maybe ChoiceIndex }
innerModel model =
    case model of
        Model model_ ->
            model_



-- UPDATE


type Msg
    = NoOp
    | Show
    | StartHiding
    | CompleteHiding
    | MakeChoiceActive ChoiceIndex
    | MakeChoiceInactive ChoiceIndex


visible : Model -> Bool
visible =
    innerModel
        >> .visibility
        >> (/=) Invisible


hide : Msg
hide =
    StartHiding


update : (Model -> parentModel) -> (Msg -> parentMsg) -> Msg -> Model -> ( parentModel, Cmd parentMsg )
update updateParentModel toParentMsg msg model =
    let
        model_ =
            innerModel model

        ( model1, cmd ) =
            case msg of
                NoOp ->
                    ( model_, Cmd.none )

                Show ->
                    ( { model_ | visibility = Visible, activeChoice = Nothing }, Cmd.none )

                StartHiding ->
                    ( { model_ | visibility = Disappearing, activeChoice = Nothing }
                    , Process.sleep 100 |> Task.perform (always CompleteHiding)
                    )

                CompleteHiding ->
                    ( { model_ | visibility = Invisible, activeChoice = Nothing }, Cmd.none )

                MakeChoiceActive choiceIndex ->
                    ( { model_ | activeChoice = Just choiceIndex }, Cmd.none )

                MakeChoiceInactive choiceIndex ->
                    ( { model_
                        | activeChoice =
                            if model_.activeChoice == Just choiceIndex then
                                Nothing

                            else
                                model_.activeChoice
                      }
                    , Cmd.none
                    )
    in
    ( updateParentModel <| Model model1, Cmd.map toParentMsg cmd )



-- VIEW


type Property msg
    = Id String


type alias Config =
    { id : Maybe String
    }


type alias ChoiceIndex =
    Int


type Choice parentMsg
    = Choice
        { body : List (Html parentMsg)
        , onSelect : parentMsg
        }


choice : List (Html parentMsg) -> parentMsg -> Choice parentMsg
choice body onSelect =
    Choice { body = body, onSelect = onSelect }


id : String -> Property msg
id =
    Id


configFromProperties : List (Property msg) -> Config
configFromProperties =
    List.foldl
        (\property config ->
            case property of
                Id string ->
                    { config | id = Just string }
        )
        { id = Nothing }


view :
    (Msg -> parentMsg)
    -> Model
    -> List (Html parentMsg)
    -> List (Choice parentMsg)
    -> Html parentMsg
view toParentMsg model body_ choices =
    let
        model_ =
            innerModel model

        config =
            model_.config
    in
    div
        [ class "relative inline-block text-left" ]
        [ div
            []
            [ Components.Button.white True
                [ class "w-full"
                , Extras.HtmlAttribute.showMaybe Html.Attributes.id config.id
                , Accessibility.Aria.expanded <| model_.visibility == Visible
                , Accessibility.Aria.hasMenuPopUp
                , Extras.HtmlEvents.onClickPreventDefaultAndStopPropagation <|
                    toParentMsg <|
                        case model_.visibility of
                            Visible ->
                                StartHiding

                            Disappearing ->
                                NoOp

                            Invisible ->
                                Show
                , Extras.HtmlEvents.onKeydown
                    (\code ->
                        if code == Extras.HtmlEvents.enter then
                            Just <|
                                case model_.visibility of
                                    Visible ->
                                        case model_.activeChoice of
                                            Just active ->
                                                choices
                                                    |> Array.fromList
                                                    |> Array.get active
                                                    |> Maybe.map
                                                        (\choice_ ->
                                                            case choice_ of
                                                                Choice { onSelect } ->
                                                                    onSelect
                                                        )
                                                    |> Maybe.withDefault (toParentMsg NoOp)

                                            Nothing ->
                                                toParentMsg StartHiding

                                    Disappearing ->
                                        toParentMsg NoOp

                                    Invisible ->
                                        toParentMsg Show

                        else if code == Extras.HtmlEvents.escape then
                            Just <| toParentMsg StartHiding

                        else if code == Extras.HtmlEvents.downArrow then
                            let
                                numberOfChoices =
                                    List.length choices
                            in
                            Just <|
                                case model_.activeChoice of
                                    Just active ->
                                        toParentMsg <| MakeChoiceActive <| remainderBy numberOfChoices (active + 1)

                                    Nothing ->
                                        if numberOfChoices > 0 then
                                            toParentMsg <| MakeChoiceActive 0

                                        else
                                            toParentMsg NoOp

                        else if code == Extras.HtmlEvents.upArrow then
                            let
                                numberOfChoices =
                                    List.length choices
                            in
                            Just <|
                                case model_.activeChoice of
                                    Just active ->
                                        toParentMsg <|
                                            MakeChoiceActive <|
                                                if active == 0 then
                                                    numberOfChoices - 1

                                                else
                                                    active - 1

                                    Nothing ->
                                        if numberOfChoices > 0 then
                                            toParentMsg <| MakeChoiceActive <| numberOfChoices - 1

                                        else
                                            toParentMsg <| NoOp

                        else
                            Nothing
                    )
                ]
                (body_
                    ++ [ Icons.chevronDown
                            [ Svg.Attributes.class "-mr-1 ml-2 h-5 w-5" ]
                       ]
                )
            ]
        , div
            [ Extras.HtmlAttribute.showMaybe Accessibility.Aria.labelledBy config.id
            , Accessibility.Aria.orientationVertical
            , class "origin-top-right absolute right-0 mt-2 w-56 rounded-md shadow-lg bg-white dark:bg-gray-800 ring-1 ring-black dark:ring-gray-600 ring-opacity-5 divide-y divide-gray-100 focus:outline-none"
            , class "hidden" |> Extras.HtmlAttribute.showIf (model_.visibility == Invisible)
            , if model_.visibility == Visible then
                class "transition ease-out duration-100 transform opacity-100 scale-100"

              else
                class "transition ease-in duration-75 transform opacity-0 scale-95"
            , Accessibility.Role.menu
            , Accessibility.Key.tabbable False
            ]
            [ div
                [ class "py-1"
                , attribute "role" "none"
                ]
                (choices
                    |> List.indexedMap
                        (\choiceIndex choice_ ->
                            case choice_ of
                                Choice { body, onSelect } ->
                                    Html.a
                                        [ class "group flex items-center px-4 py-2 hover:no-underline"
                                        , if model_.activeChoice == Just choiceIndex then
                                            class "text-gray-900 dark:text-gray-100 bg-gray-100 dark:bg-gray-900"

                                          else
                                            class "text-gray-700 dark:text-gray-300"
                                        , href "#"
                                        , Accessibility.Role.menuItem
                                        , Accessibility.Key.tabbable False
                                        , Extras.HtmlEvents.onClickPreventDefault onSelect
                                        , Html.Events.onMouseEnter <| toParentMsg <| MakeChoiceActive choiceIndex
                                        , Html.Events.onMouseLeave <| toParentMsg <| MakeChoiceInactive choiceIndex
                                        ]
                                        body
                        )
                )
            ]
        ]
