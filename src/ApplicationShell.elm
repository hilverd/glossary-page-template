port module ApplicationShell exposing (main)

import Browser
import Browser.Dom as Dom
import Data.LoadedGlossaryItems as LoadedGlossaryItems
import Html exposing (Html)
import Json.Decode as Decode
import PageMsg exposing (PageMsg(..))
import Pages.CreateOrEdit
import Pages.ListAll
import Task



-- MODEL


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    Decode.Value


type Page
    = ListAll Pages.ListAll.Model
    | CreateOrEdit Pages.CreateOrEdit.Model


type alias Model =
    { page : Page }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        editorIsRunning =
            flags
                |> Decode.decodeValue (Decode.field "editorIsRunning" Decode.bool)
                |> Result.toMaybe
                |> Maybe.withDefault False

        enableHelpForMakingChanges =
            flags
                |> Decode.decodeValue (Decode.field "enableHelpForMakingChanges" Decode.bool)
                |> Result.toMaybe
                |> Maybe.withDefault False

        loadedGlossaryItems =
            LoadedGlossaryItems.decodeFromFlags flags

        ( listAllModel, listAllCmd ) =
            Pages.ListAll.init
                editorIsRunning
                enableHelpForMakingChanges
                loadedGlossaryItems
    in
    ( { page = ListAll listAllModel }
    , Cmd.map ListAllMsg listAllCmd
    )


type Msg
    = NoOp
    | ListAllMsg Pages.ListAll.Msg
    | CreateOrEditMsg Pages.CreateOrEdit.Msg



-- PORTS


port showTitleHeaderAndAbout : () -> Cmd msg


port hideTitleHeaderAndAbout : () -> Cmd msg



-- UPDATE


withoutInternal : Msg -> PageMsg ()
withoutInternal msg =
    case msg of
        ListAllMsg (NavigateToListAll editorIsRunning enableHelpForMakingChanges glossaryItems) ->
            PageMsg.NavigateToListAll editorIsRunning enableHelpForMakingChanges glossaryItems

        ListAllMsg (NavigateToCreateOrEdit enableHelpForMakingChanges maybeIndex glossaryItems) ->
            PageMsg.NavigateToCreateOrEdit enableHelpForMakingChanges maybeIndex glossaryItems

        ListAllMsg (PageMsg.Internal _) ->
            PageMsg.Internal ()

        CreateOrEditMsg (NavigateToListAll editorIsRunning enableHelpForMakingChanges glossaryItems) ->
            PageMsg.NavigateToListAll editorIsRunning enableHelpForMakingChanges glossaryItems

        CreateOrEditMsg (NavigateToCreateOrEdit enableHelpForMakingChanges maybeIndex glossaryItems) ->
            PageMsg.NavigateToCreateOrEdit enableHelpForMakingChanges maybeIndex glossaryItems

        CreateOrEditMsg (PageMsg.Internal _) ->
            PageMsg.Internal ()

        NoOp ->
            PageMsg.Internal ()


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, withoutInternal msg, model.page ) of
        ( _, NavigateToListAll editorIsRunning enableHelpForMakingChanges glossaryItems, _ ) ->
            let
                ( listAllModel, listAllCmd ) =
                    Pages.ListAll.init editorIsRunning enableHelpForMakingChanges glossaryItems
            in
            ( { model | page = ListAll listAllModel }
            , Cmd.batch [ showTitleHeaderAndAbout (), resetViewport, Cmd.map ListAllMsg listAllCmd ]
            )

        ( _, NavigateToCreateOrEdit enableHelpForMakingChanges maybeIndex glossaryItems, _ ) ->
            let
                ( createOrEditModel, createOrEditCmd ) =
                    Pages.CreateOrEdit.init enableHelpForMakingChanges maybeIndex glossaryItems
            in
            ( { model | page = CreateOrEdit createOrEditModel }
            , Cmd.batch [ hideTitleHeaderAndAbout (), resetViewport, Cmd.map CreateOrEditMsg createOrEditCmd ]
            )

        ( ListAllMsg (PageMsg.Internal msg_), _, ListAll listAllModel ) ->
            let
                ( listAllModel_, listAllCmd ) =
                    Pages.ListAll.update msg_ listAllModel
            in
            ( { model | page = ListAll listAllModel_ }, listAllCmd |> Cmd.map ListAllMsg )

        ( CreateOrEditMsg (PageMsg.Internal msg_), _, CreateOrEdit createOrEditModel ) ->
            let
                ( createOrEditModel_, createOrEditCmd ) =
                    Pages.CreateOrEdit.update msg_ createOrEditModel
            in
            ( { model | page = CreateOrEdit createOrEditModel_ }, createOrEditCmd |> Cmd.map CreateOrEditMsg )

        _ ->
            ( model, Cmd.none )


resetViewport : Cmd Msg
resetViewport =
    Task.perform (always NoOp) <| Dom.setViewport 0 0



-- VIEW


view : Model -> Html Msg
view model =
    case model.page of
        ListAll page ->
            Pages.ListAll.view page
                |> Html.map ListAllMsg

        CreateOrEdit page ->
            Pages.CreateOrEdit.view page
                |> Html.map CreateOrEditMsg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
