module CommonModel exposing (CommonModel, relativeUrl)

import Browser.Navigation exposing (Key)
import Data.Editability exposing (Editability)
import Data.Glossary exposing (Glossary)
import Data.GlossaryItemId exposing (GlossaryItemId)
import Data.Theme exposing (Theme)
import QueryParameters exposing (QueryParameters)
import Url exposing (Url)


type alias CommonModel =
    { key : Key
    , initialUrl : Url
    , filename : Maybe String
    , theme : Theme
    , editability : Editability
    , enableMathSupport : Bool
    , queryParameters : QueryParameters
    , separateBackendBaseUrl : Maybe String
    , maybeId : Maybe GlossaryItemId
    , fragment : Maybe String
    , glossary : Result String Glossary
    }


relativeUrl : CommonModel -> String
relativeUrl commonModel =
    commonModel.queryParameters
        |> QueryParameters.toRelativeUrl
        |> (\urlString ->
                if urlString == "" then
                    let
                        initialUrl =
                            commonModel.initialUrl
                    in
                    { initialUrl | query = Nothing }
                        |> Url.toString

                else
                    urlString
           )
