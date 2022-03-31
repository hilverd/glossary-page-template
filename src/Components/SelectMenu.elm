module Components.SelectMenu exposing
    ( Choice
    , ariaLabel
    , id
    , onChange
    , render
    , showValidationErrors
    , validationError
    )

import Accessibility exposing (..)
import Accessibility.Aria
import Extras.Html
import Extras.HtmlAttribute
import Html.Attributes exposing (class, selected, value)
import Html.Events
import Json.Decode as Decode


type Property msg
    = Id String
    | AriaLabel String
    | ValidationError (Maybe String)
    | ShowValidationErrors Bool
    | OnChange (String -> msg)


type alias Config msg =
    { id : Maybe String
    , ariaLabel : Maybe String
    , validationError : Maybe String
    , showValidationErrors : Bool
    , onChange : Maybe (String -> msg)
    }


type alias Choice =
    { value : String
    , body : String
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
        )
        { id = Nothing
        , ariaLabel = Nothing
        , validationError = Nothing
        , showValidationErrors = False
        , onChange = Nothing
        }


render : List (Property msg) -> List Choice -> Html msg
render properties choices =
    let
        config =
            configFromProperties properties

        pleaseSelectOption =
            option
                [ value "" ]
                [ text "--- Please select ---" ]
    in
    div
        [ class "flex-auto" ]
        [ select
            [ Extras.HtmlAttribute.showMaybe Html.Attributes.id config.id
            , class "mt-1 block w-full pl-3 pr-10 py-2 dark:bg-gray-700 dark:text-gray-200 border-gray-300 dark:border-gray-500 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 rounded-md"
            , Extras.HtmlAttribute.showMaybe Accessibility.Aria.label config.ariaLabel
            , Extras.HtmlAttribute.showMaybe
                (\handler ->
                    Html.Events.on "change" <|
                        Decode.map handler <|
                            Decode.andThen Decode.succeed Html.Events.targetValue
                )
                config.onChange
            ]
            ((if List.any .selected choices then
                []

              else
                [ pleaseSelectOption ]
             )
                ++ (choices
                        |> List.map
                            (\choice ->
                                option
                                    [ value choice.value
                                    , selected choice.selected
                                    ]
                                    [ text choice.body ]
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
