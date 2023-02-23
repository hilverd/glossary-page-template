module Components.DropdownMenu exposing
    ( Choice
    , Model
    , Msg
    , Property
    , choice
    , hidden
    , id
    , init
    , subscriptions
    , update
    , view
    )

import Accessibility exposing (Html, div)
import Accessibility.Aria
import Accessibility.Key
import Accessibility.Role
import Array
import Browser.Events as Events
import Components.Button
import Extras.HtmlAttribute
import Extras.HtmlEvents
import Html
import Html.Attributes exposing (attribute, class, href)
import Html.Events
import Icons
import Json.Decode as Decode
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


update : (Model -> parentModel) -> (Msg -> parentMsg) -> Msg -> Model -> ( parentModel, Cmd parentMsg )
update updateParentModel toParentMsg msg model =
    let
        model_ : { visibility : GradualVisibility, config : Config, activeChoice : Maybe ChoiceIndex }
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
    -> Bool
    -> List (Html parentMsg)
    -> List (Choice parentMsg)
    -> Html parentMsg
view toParentMsg model enabled body_ choices =
    let
        model_ : { visibility : GradualVisibility, config : Config, activeChoice : Maybe ChoiceIndex }
        model_ =
            innerModel model

        config : Config
        config =
            model_.config
    in
    div
        [ class "relative inline-block text-left" ]
        [ div
            []
            [ Components.Button.white enabled
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
                , Html.Events.preventDefaultOn "keydown"
                    (Extras.HtmlEvents.preventDefaultOnDecoder
                        (\event ->
                            if event == Extras.HtmlEvents.enter then
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
                                                    |> Maybe.map (\x -> ( x, True ))

                                            Nothing ->
                                                Just ( toParentMsg StartHiding, True )

                                    Disappearing ->
                                        Nothing

                                    Invisible ->
                                        Just ( toParentMsg Show, True )

                            else if event == Extras.HtmlEvents.escape then
                                Just ( toParentMsg StartHiding, True )

                            else if event == Extras.HtmlEvents.downArrow then
                                let
                                    numberOfChoices : Int
                                    numberOfChoices =
                                        List.length choices
                                in
                                case model_.activeChoice of
                                    Just active ->
                                        Just ( (active + 1) |> min (numberOfChoices - 1) |> max 0 |> MakeChoiceActive |> toParentMsg, True )

                                    Nothing ->
                                        if numberOfChoices > 0 then
                                            Just ( toParentMsg <| MakeChoiceActive 0, True )

                                        else
                                            Nothing

                            else if event == Extras.HtmlEvents.upArrow then
                                let
                                    numberOfChoices : Int
                                    numberOfChoices =
                                        List.length choices
                                in
                                case model_.activeChoice of
                                    Just active ->
                                        Just ( (active - 1) |> min (numberOfChoices - 1) |> max 0 |> MakeChoiceActive |> toParentMsg, True )

                                    Nothing ->
                                        if numberOfChoices > 0 then
                                            Just ( toParentMsg <| MakeChoiceActive <| numberOfChoices - 1, True )

                                        else
                                            Nothing

                            else
                                Nothing
                        )
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
                class "transition motion-reduce:transition-none ease-out duration-100 transform motion-reduce:transform-none opacity-100 scale-100"

              else
                class "transition motion-reduce:transition-none ease-in duration-75 transform motion-reduce:transform-none opacity-0 scale-95"
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model |> innerModel |> .visibility |> (==) Visible then
        Events.onClick <| Decode.succeed StartHiding

    else
        Sub.none
