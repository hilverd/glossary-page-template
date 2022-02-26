module ApplicationShell exposing (main)

import Browser
import Browser.Dom as Dom
import CommonModel exposing (CommonModel)
import Data.AboutLink as AboutLink
import Data.LoadedGlossaryItems as LoadedGlossaryItems
import Data.Title as Title
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
    Page


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        title =
            flags
                |> Decode.decodeValue (Decode.field "titleString" Decode.string)
                |> Result.withDefault "Element not found"
                |> Title.fromString

        aboutParagraph =
            flags
                |> Decode.decodeValue (Decode.field "aboutParagraph" Decode.string)
                |> Result.withDefault "Element not found"

        aboutLinks =
            flags
                |> Decode.decodeValue (Decode.field "aboutLinks" <| Decode.list AboutLink.decode)
                |> Result.withDefault []

        editorIsRunning =
            flags
                |> Decode.decodeValue (Decode.field "editorIsRunning" Decode.bool)
                |> Result.withDefault False

        enableHelpForMakingChanges =
            flags
                |> Decode.decodeValue (Decode.field "enableHelpForMakingChanges" Decode.bool)
                |> Result.withDefault False

        loadedGlossaryItems =
            LoadedGlossaryItems.decodeFromFlags flags

        ( listAllModel, listAllCmd ) =
            Pages.ListAll.init
                editorIsRunning
                (CommonModel
                    enableHelpForMakingChanges
                    title
                    aboutParagraph
                    aboutLinks
                    CommonModel.Alphabetically
                    loadedGlossaryItems
                    Nothing
                )
    in
    ( ListAll listAllModel
    , Cmd.map ListAllMsg listAllCmd
    )


type Msg
    = NoOp
    | ListAllMsg Pages.ListAll.Msg
    | CreateOrEditMsg Pages.CreateOrEdit.Msg



-- UPDATE


withoutInternal : Msg -> PageMsg ()
withoutInternal msg =
    case msg of
        ListAllMsg (NavigateToListAll commonModel) ->
            PageMsg.NavigateToListAll commonModel

        ListAllMsg (NavigateToCreateOrEdit commonModel) ->
            PageMsg.NavigateToCreateOrEdit commonModel

        ListAllMsg (PageMsg.Internal _) ->
            PageMsg.Internal ()

        CreateOrEditMsg (NavigateToListAll commonModel) ->
            PageMsg.NavigateToListAll commonModel

        CreateOrEditMsg (NavigateToCreateOrEdit commonModel) ->
            PageMsg.NavigateToCreateOrEdit commonModel

        CreateOrEditMsg (PageMsg.Internal _) ->
            PageMsg.Internal ()

        NoOp ->
            PageMsg.Internal ()


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, withoutInternal msg, model ) of
        ( _, NavigateToListAll commonModel, _ ) ->
            let
                ( listAllModel, listAllCmd ) =
                    Pages.ListAll.init True commonModel
            in
            ( ListAll listAllModel
            , Cmd.map ListAllMsg listAllCmd
            )

        ( _, NavigateToCreateOrEdit commonModel, _ ) ->
            let
                ( createOrEditModel, createOrEditCmd ) =
                    Pages.CreateOrEdit.init commonModel
            in
            ( CreateOrEdit createOrEditModel
            , Cmd.batch [ resetViewport, Cmd.map CreateOrEditMsg createOrEditCmd ]
            )

        ( ListAllMsg (PageMsg.Internal msg_), _, ListAll listAllModel ) ->
            let
                ( listAllModel_, listAllCmd ) =
                    Pages.ListAll.update msg_ listAllModel
            in
            ( ListAll listAllModel_, listAllCmd |> Cmd.map ListAllMsg )

        ( CreateOrEditMsg (PageMsg.Internal msg_), _, CreateOrEdit createOrEditModel ) ->
            let
                ( createOrEditModel_, createOrEditCmd ) =
                    Pages.CreateOrEdit.update msg_ createOrEditModel
            in
            ( CreateOrEdit createOrEditModel_, createOrEditCmd |> Cmd.map CreateOrEditMsg )

        _ ->
            ( model, Cmd.none )


resetViewport : Cmd Msg
resetViewport =
    Task.perform (always NoOp) <| Dom.setViewport 0 0



-- VIEW


view : Model -> Html Msg
view model =
    case model of
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
