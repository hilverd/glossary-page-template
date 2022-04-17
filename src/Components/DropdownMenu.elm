module Components.DropdownMenu exposing (Choice, Model, Msg, hide, id, init, update, view)

import Accessibility exposing (..)
import Accessibility.Aria
import Accessibility.Key
import Accessibility.Role
import Components.Button
import Extras.HtmlAttribute
import Extras.HtmlEvents
import Extras.HtmlTree exposing (HtmlTree(..))
import Html
import Html.Attributes exposing (attribute, class, href)
import Icons
import Process
import Svg.Attributes exposing (fill, height, stroke, width)
import Task



-- MODEL


type GradualVisibility
    = Visible
    | Disappearing
    | Invisible


type Model parentMsg
    = Model
        { visibility : GradualVisibility
        , config : Config
        }


init : List (Property parentMsg) -> Model parentMsg
init properties =
    Model
        { visibility = Invisible
        , config = configFromProperties properties
        }


innerModel :
    Model parentMsg
    ->
        { visibility : GradualVisibility
        , config : Config
        }
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


hide : Msg
hide =
    StartHiding


update :
    (Model parentMsg -> parentModel)
    -> (Msg -> parentMsg)
    -> Msg
    -> Model parentMsg
    -> ( parentModel, Cmd parentMsg )
update updateParentModel toParentMsg msg model =
    let
        model_ =
            innerModel model

        ( model1, cmd ) =
            case msg of
                NoOp ->
                    ( model_, Cmd.none )

                Show ->
                    ( { model_ | visibility = Visible }, Cmd.none )

                StartHiding ->
                    ( { model_ | visibility = Disappearing }
                    , Process.sleep 100 |> Task.perform (always CompleteHiding)
                    )

                CompleteHiding ->
                    ( { model_ | visibility = Invisible }, Cmd.none )
    in
    ( updateParentModel <| Model model1, Cmd.map toParentMsg cmd )



-- VIEW


type Property msg
    = Id String


type alias Config =
    { id : Maybe String
    }


type alias Choice parentMsg =
    { id : String
    , body : List (Html parentMsg)
    , onSelect : parentMsg
    }


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
    -> Model parentMsg
    -> List (Html parentMsg)
    -> List (Choice parentMsg)
    -> Html parentMsg
view toParentMsg model body choices =
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
                        Maybe.map toParentMsg <|
                            if code == Extras.HtmlEvents.enter then
                                Just NoOp

                            else if code == Extras.HtmlEvents.escape then
                                Just StartHiding

                            else
                                Nothing
                    )
                ]
                (body
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
                    |> List.map
                        (\choice ->
                            Html.a
                                [ class "group flex items-center px-4 py-2 hover:no-underline text-gray-700 dark:text-gray-300 hover:text-gray-900 dark:hover:text-gray-100 hover:bg-gray-100 dark:hover:bg-gray-900"
                                , href "#"
                                , Html.Attributes.id choice.id
                                , Accessibility.Role.menuItem
                                , Accessibility.Key.tabbable False
                                , Extras.HtmlEvents.onClickPreventDefault choice.onSelect
                                ]
                                choice.body
                        )
                )
            ]
        ]
