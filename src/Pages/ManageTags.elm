module Pages.ManageTags exposing (InternalMsg, Model, Msg, init, subscriptions, update, view)

import Accessibility exposing (Html, div, form, h1, h2, label, main_, p, span, text)
import Accessibility.Aria
import Array exposing (Array)
import Browser exposing (Document)
import Browser.Dom as Dom
import CommonModel exposing (CommonModel)
import Components.AboutSection
import Components.Button
import Components.Copy
import Components.Form
import Components.Spinner
import Data.AboutLink as AboutLink
import Data.AboutLinkIndex as AboutLinkIndex exposing (AboutLinkIndex)
import Data.AboutParagraph as AboutParagraph
import Data.AboutSection exposing (AboutSection)
import Data.Glossary as Glossary
import Data.GlossaryItems exposing (GlossaryItems)
import Data.GlossaryTitle as GlossaryTitle
import Data.Saving exposing (Saving(..))
import ElementIds
import Extras.Html
import Extras.HtmlEvents
import Extras.HtmlTree as HtmlTree
import Extras.Http
import Extras.Task
import Html
import Html.Attributes exposing (class, for, id, name, placeholder, required, spellcheck, type_)
import Html.Events
import Http
import Icons
import Json.Decode as Decode
import PageMsg exposing (PageMsg)
import Svg.Attributes
import TagsForm as Form exposing (TagsForm)
import Task



-- MODEL


type alias Model =
    { common : CommonModel
    }


type InternalMsg
    = NoOp


type alias Msg =
    PageMsg InternalMsg


init : CommonModel -> ( Model, Cmd Msg )
init common =
    case common.glossary of
        Ok _ ->
            ( { common = common }
            , Cmd.none
            )

        _ ->
            ( { common = common }
            , Cmd.none
            )



-- UPDATE


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Manage Tags"
    , body = [ Html.text "Manage Tags" ]
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
