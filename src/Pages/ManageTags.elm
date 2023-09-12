module Pages.ManageTags exposing (InternalMsg, Model, Msg, init, subscriptions, update, view)

import Browser exposing (Document)
import CommonModel exposing (CommonModel)
import Html
import PageMsg exposing (PageMsg)



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
    { title = ""
    , body = [ Html.text "Manage Tags" ]
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
