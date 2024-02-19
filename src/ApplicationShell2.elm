module ApplicationShell2 exposing (main)

import Browser
import Browser.Dom
import Browser.Navigation
import Json.Decode as Decode
import Pages.CreateOrEdit
import Pages.EditTitleAndAbout
import Pages.ListAll
import Pages.ManageTags
import Route exposing (Route)
import Url exposing (Url)



-- MODEL


type Page
    = ListAll Pages.ListAll.Model
    | CreateOrEdit Pages.CreateOrEdit.Model
    | EditTitleAndAbout Pages.EditTitleAndAbout.Model
    | ManageTags Pages.ManageTags.Model


type alias Model =
    { key : Browser.Navigation.Key
    , url : Url
    , page : Page
    }



-- UPDATE


type Msg
    = UrlChanged Url
    | UrlRequested Browser.UrlRequest
    | Navigate Route
    | ListAllMsg Pages.ListAll.Msg
    | CreateOrEditMsg Pages.CreateOrEdit.Msg
    | ManageTagsMsg Pages.ManageTags.Msg



-- SUBSCRIPTIONS
-- VIEW
-- MAIN


{-| Flags passed to the application from JavaScript.
-}
type alias Flags =
    Decode.Value


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }
