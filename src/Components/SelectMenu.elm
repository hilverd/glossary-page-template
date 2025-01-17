module Components.SelectMenu exposing
    ( Choice
    , Property
    , ariaLabel
    , enabled
    , id
    , onChange
    , render
    , showValidationErrors
    , validationError
    )

import Accessibility exposing (Html, div, option, p, select, text)
import Accessibility.Aria
import Extras.Html
import Extras.HtmlAttribute
import Html
import Html.Attributes exposing (class, disabled, selected, value)
import Html.Events
import Internationalisation as I18n
import Json.Decode as Decode


type Property msg
    = Id String
    | AriaLabel String
    | ValidationError (Maybe String)
    | ShowValidationErrors Bool
    | OnChange (String -> msg)
    | Enabled Bool


type alias Config msg =
    { id : Maybe String
    , ariaLabel : Maybe String
    , validationError : Maybe String
    , showValidationErrors : Bool
    , onChange : Maybe (String -> msg)
    , enabled : Maybe Bool
    }


type alias Choice =
    { value : String
    , body : List (Html Never)
    , selected : Bool
    }


id : String -> Property msg
id =
    Id


ariaLabel : String -> Property msg
ariaLabel =
    AriaLabel


validationError : Maybe String -> Property msg
validationError =
    ValidationError


showValidationErrors : Bool -> Property msg
showValidationErrors =
    ShowValidationErrors


onChange : (String -> msg) -> Property msg
onChange =
    OnChange


enabled : Bool -> Property msg
enabled =
    Enabled


configFromProperties : List (Property msg) -> Config msg
configFromProperties =
    List.foldl
        (\property config ->
            case property of
                Id string ->
                    { config | id = Just string }

                AriaLabel string ->
                    { config | ariaLabel = Just string }

                ValidationError maybeString ->
                    { config | validationError = maybeString }

                ShowValidationErrors bool ->
                    { config | showValidationErrors = bool }

                OnChange handler ->
                    { config | onChange = Just handler }

                Enabled enabled_ ->
                    { config | enabled = Just enabled_ }
        )
        { id = Nothing
        , ariaLabel = Nothing
        , validationError = Nothing
        , showValidationErrors = False
        , onChange = Nothing
        , enabled = Nothing
        }


render : List (Property msg) -> List Choice -> Html msg
render properties choices =
    let
        config : Config msg
        config =
            configFromProperties properties
    in
    div
        [ class "flex-grow" ]
        [ select
            [ Extras.HtmlAttribute.showMaybe Html.Attributes.id config.id
            , class "mt-1 block w-full pl-3 pr-10 py-2 dark:bg-gray-700 dark:text-gray-200 border-gray-300 dark:border-gray-500 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 rounded-md"
            , Extras.HtmlAttribute.showMaybe Accessibility.Aria.label config.ariaLabel
            , Extras.HtmlAttribute.showMaybe
                (\handler ->
                    Html.Events.on "change" <|
                        Decode.map handler <|
                            Html.Events.targetValue
                )
                config.onChange
            , config.enabled |> Maybe.withDefault True |> not |> disabled
            ]
            ((if List.any .selected choices then
                []

              else
                [ option
                    [ value "" ]
                    [ text <| "--- " ++ I18n.pleaseSelect ++ " ---" ]
                ]
             )
                ++ (choices
                        |> List.map
                            (\choice ->
                                option
                                    [ value choice.value
                                    , selected choice.selected
                                    ]
                                    (List.map (Html.map Basics.never) choice.body)
                            )
                   )
            )
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
