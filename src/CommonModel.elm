module CommonModel exposing (CommonModel, initialUrlWithoutQueryOrFragment, relativeUrl)

import Browser.Navigation exposing (Key)
import Data.Editability exposing (Editability)
import Data.GlossaryForUi exposing (GlossaryForUi)
import Data.Theme exposing (Theme)
import QueryParameters exposing (QueryParameters)
import Url exposing (Url)


type alias CommonModel =
    { key : Key
    , initialUrl : Url
    , runningOnMacOs : Bool
    , filename : Maybe String
    , theme : Theme
    , editability : Editability
    , enableMathSupport : Bool
    , queryParameters : QueryParameters
    , fragment : Maybe String
    , glossaryForUi : Result String GlossaryForUi
    }


relativeUrl : CommonModel -> String
relativeUrl commonModel =
    let
        queryUrl : String
        queryUrl =
            commonModel.queryParameters
                |> QueryParameters.toRelativeUrl
                |> (\urlString ->
                        if urlString == "" then
                            "?"

                        else
                            urlString
                   )
    in
    case commonModel.fragment of
        Just fragment ->
            queryUrl ++ "#" ++ fragment

        Nothing ->
            queryUrl


initialUrlWithoutQueryOrFragment : CommonModel -> String
initialUrlWithoutQueryOrFragment commonModel =
    let
        url : Url
        url =
            commonModel.initialUrl

        protocolString : String
        protocolString =
            case url.protocol of
                Url.Http ->
                    "http://"

                Url.Https ->
                    "https://"

        portString : String
        portString =
            case url.port_ of
                Nothing ->
                    ""

                Just port_ ->
                    ":" ++ String.fromInt port_
    in
    protocolString ++ url.host ++ portString ++ url.path
