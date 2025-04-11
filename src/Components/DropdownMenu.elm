module Components.DropdownMenu exposing
    ( ButtonShape(..)
    , Choice
    , Model
    , Msg
    , Property
    , choice
    , hidden
    , id
    , init
    , originTopLeft
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
import Data.GradualVisibility exposing (GradualVisibility(..))
import Extras.HtmlAttribute
import Extras.HtmlEvents
import Html
import Html.Attributes exposing (attribute, class, href)
import Html.Events
import Icons
import Internationalisation as I18n
import Json.Decode as Decode
import Process
import Svg.Attributes
import Task



-- MODEL


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
    | ActivateChoice ChoiceIndex
    | DeactivateChoice ChoiceIndex


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

                ActivateChoice choiceIndex ->
                    ( { model_ | activeChoice = Just choiceIndex }, Cmd.none )

                DeactivateChoice choiceIndex ->
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
    | OriginTopRight Bool


type alias Config =
    { id : Maybe String
    , originTopRight : Bool
    }


type ButtonShape parentMsg
    = Ellipsis
    | Chevron (List (Html parentMsg))


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


originTopLeft : Property msg
originTopLeft =
    OriginTopRight False


configFromProperties : List (Property msg) -> Config
configFromProperties =
    List.foldl
        (\property config ->
            case property of
                Id string ->
                    { config | id = Just string }

                OriginTopRight bool ->
                    { config | originTopRight = bool }
        )
        { id = Nothing
        , originTopRight = True
        }


view :
    (Msg -> parentMsg)
    -> Model
    -> Bool
    -> ButtonShape parentMsg
    -> List (Choice parentMsg)
    -> Html parentMsg
view toParentMsg model enabled buttonShape choices =
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
        [ let
            openOptionsMessage : Html msg
            openOptionsMessage =
                Html.span
                    [ class "sr-only" ]
                    [ Html.text I18n.openOptions ]

            buttonAttributes : List (Html.Attribute parentMsg)
            buttonAttributes =
                [ Extras.HtmlAttribute.showMaybe Html.Attributes.id config.id
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
                            if Extras.HtmlEvents.isEnter event then
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

                            else if Extras.HtmlEvents.isEscape event then
                                Just ( toParentMsg StartHiding, True )

                            else if Extras.HtmlEvents.isDownArrow event then
                                let
                                    numberOfChoices : Int
                                    numberOfChoices =
                                        List.length choices
                                in
                                case model_.activeChoice of
                                    Just active ->
                                        Just ( (active + 1) |> min (numberOfChoices - 1) |> max 0 |> ActivateChoice |> toParentMsg, True )

                                    Nothing ->
                                        if numberOfChoices > 0 then
                                            Just ( toParentMsg <| ActivateChoice 0, True )

                                        else
                                            Nothing

                            else if Extras.HtmlEvents.isUpArrow event then
                                let
                                    numberOfChoices : Int
                                    numberOfChoices =
                                        List.length choices
                                in
                                case model_.activeChoice of
                                    Just active ->
                                        Just ( (active - 1) |> min (numberOfChoices - 1) |> max 0 |> ActivateChoice |> toParentMsg, True )

                                    Nothing ->
                                        if numberOfChoices > 0 then
                                            Just ( toParentMsg <| ActivateChoice <| numberOfChoices - 1, True )

                                        else
                                            Nothing

                            else
                                Nothing
                        )
                    )
                ]
          in
          div
            []
            [ case buttonShape of
                Ellipsis ->
                    Components.Button.roundedWithoutBorder enabled
                        buttonAttributes
                        [ openOptionsMessage
                        , Icons.ellipsisVertical
                            [ Svg.Attributes.class "h-6 w-6" ]
                        ]

                Chevron body_ ->
                    Components.Button.white enabled
                        (class "w-full" :: buttonAttributes)
                        ((openOptionsMessage :: body_)
                            ++ [ Icons.chevronDown
                                    [ Svg.Attributes.class "-mr-1 ml-2 h-5 w-5" ]
                               ]
                        )
            ]
        , div
            [ Extras.HtmlAttribute.showMaybe Accessibility.Aria.labelledBy config.id
            , Accessibility.Aria.orientationVertical
            , class "absolute z-10 w-max mt-2 rounded-md shadow-lg bg-white dark:bg-gray-800 ring-1 ring-black/5 dark:ring-gray-600 divide-y divide-gray-100 focus:outline-hidden"
            , class "hidden" |> Extras.HtmlAttribute.showIf (model_.visibility == Invisible)
            , class <|
                if config.originTopRight then
                    "origin-top-right right-0"

                else
                    "origin-top-left left-0"
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
                                        , Html.Events.onMouseEnter <| toParentMsg <| ActivateChoice choiceIndex
                                        , Html.Events.onMouseLeave <| toParentMsg <| DeactivateChoice choiceIndex
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
