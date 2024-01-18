module ApplicationShell exposing (main, Flags, Page, Model, Msg)

{-| The application shell which shows different "pages" depending on the application state.


# Definition

@docs main, Flags, Page, Model, Msg

-}

import Browser exposing (Document, UrlRequest)
import Browser.Dom as Dom
import Browser.Navigation exposing (Key)
import Codec
import CommonModel exposing (CommonModel)
import Data.Editability as Editability
import Data.Glossary as Glossary exposing (Glossary)
import Data.Theme as Theme exposing (Theme)
import Html
import Json.Decode as Decode
import PageMsg exposing (PageMsg(..))
import Pages.CreateOrEdit
import Pages.EditTitleAndAbout
import Pages.ListAll
import Pages.ManageTags
import QueryParameters exposing (QueryParameters)
import Task
import Url exposing (Url)



-- MODEL


{-| The main function for the application.
-}
main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = RequestUrl
        , onUrlChange = onUrlChange
        }


{-| Flags passed to the application from JavaScript.
-}
type alias Flags =
    Decode.Value


{-| A page that can be shown in the browser.
-}
type Page
    = ListAll Pages.ListAll.Model
    | CreateOrEdit Pages.CreateOrEdit.Model
    | EditTitleAndAbout Pages.EditTitleAndAbout.Model
    | ManageTags Pages.ManageTags.Model


{-| A model for the application.
-}
type alias Model =
    { key : Key
    , page : Page
    }


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        queryParameters : QueryParameters
        queryParameters =
            QueryParameters.fromUrl url

        filename : Maybe String
        filename =
            url.path
                |> String.split "/"
                |> List.reverse
                |> List.head

        fragment : Maybe String
        fragment =
            url.fragment

        glossary : Result String Glossary
        glossary =
            flags
                |> Decode.decodeValue (Codec.decoder Glossary.codec)
                |> Result.mapError Decode.errorToString

        enableHelpForMakingChanges : Bool
        enableHelpForMakingChanges =
            glossary
                |> Result.map Glossary.enableHelpForMakingChanges
                |> Result.withDefault False

        editorIsRunning : Bool
        editorIsRunning =
            flags
                |> Decode.decodeValue (Decode.field "editorIsRunning" Decode.bool)
                |> Result.withDefault False

        enableSavingChangesInMemory : Bool
        enableSavingChangesInMemory =
            flags
                |> Decode.decodeValue (Decode.field "enableSavingChangesInMemory" Decode.bool)
                |> Result.withDefault False

        separateBackendBaseUrl : Maybe String
        separateBackendBaseUrl =
            flags
                |> Decode.decodeValue (Decode.field "separateBackendBaseUrl" Decode.string)
                |> Result.toMaybe

        katexIsAvailable : Bool
        katexIsAvailable =
            flags
                |> Decode.decodeValue (Decode.field "katexIsAvailable" Decode.bool)
                |> Result.withDefault False

        theme : Theme
        theme =
            flags
                |> Decode.decodeValue (Decode.field "theme" Theme.decode)
                |> Result.withDefault Theme.System

        editability =
            Editability.create
                { enableHelpForMakingChanges = enableHelpForMakingChanges
                , enableSavingChangesInMemory = enableSavingChangesInMemory
                , separateBackendBaseUrl = separateBackendBaseUrl
                , editorIsRunning = editorIsRunning
                , currentlyEditing = False
                }

        common : CommonModel
        common =
            { key = key
            , initialUrl = url
            , filename = filename
            , theme = theme
            , editability = editability
            , enableMathSupport = katexIsAvailable
            , queryParameters = queryParameters
            , separateBackendBaseUrl = separateBackendBaseUrl
            , maybeId = Nothing
            , fragment = fragment
            , glossary = glossary
            }

        ( listAllModel, listAllCmd ) =
            Pages.ListAll.init common
    in
    ( { key = key, page = ListAll listAllModel }
    , Cmd.map ListAllMsg listAllCmd
    )


{-| Messages handled by the application.
-}
type Msg
    = NoOp
    | RequestUrl UrlRequest
    | ListAllMsg Pages.ListAll.Msg
    | CreateOrEditMsg Pages.CreateOrEdit.Msg
    | EditTitleAndAboutMsg Pages.EditTitleAndAbout.Msg
    | ManageTagsMsg Pages.ManageTags.Msg



-- UPDATE


withoutInternal : Msg -> PageMsg ()
withoutInternal msg =
    case msg of
        ListAllMsg (NavigateToListAll commonModel) ->
            PageMsg.NavigateToListAll commonModel

        ListAllMsg (NavigateToCreateOrEdit commonModel) ->
            PageMsg.NavigateToCreateOrEdit commonModel

        ListAllMsg (NavigateToEditTitleAndAbout commonModel) ->
            PageMsg.NavigateToEditTitleAndAbout commonModel

        ListAllMsg (NavigateToManageTags commonModel) ->
            PageMsg.NavigateToManageTags commonModel

        ListAllMsg (PageMsg.Internal _) ->
            PageMsg.Internal ()

        CreateOrEditMsg (NavigateToListAll commonModel) ->
            PageMsg.NavigateToListAll commonModel

        CreateOrEditMsg (NavigateToCreateOrEdit commonModel) ->
            PageMsg.NavigateToCreateOrEdit commonModel

        CreateOrEditMsg (NavigateToEditTitleAndAbout commonModel) ->
            PageMsg.NavigateToEditTitleAndAbout commonModel

        CreateOrEditMsg (NavigateToManageTags commonModel) ->
            PageMsg.NavigateToManageTags commonModel

        CreateOrEditMsg (PageMsg.Internal _) ->
            PageMsg.Internal ()

        EditTitleAndAboutMsg (NavigateToListAll commonModel) ->
            PageMsg.NavigateToListAll commonModel

        EditTitleAndAboutMsg (NavigateToCreateOrEdit commonModel) ->
            PageMsg.NavigateToCreateOrEdit commonModel

        EditTitleAndAboutMsg (NavigateToEditTitleAndAbout commonModel) ->
            PageMsg.NavigateToEditTitleAndAbout commonModel

        EditTitleAndAboutMsg (NavigateToManageTags commonModel) ->
            PageMsg.NavigateToManageTags commonModel

        EditTitleAndAboutMsg (PageMsg.Internal _) ->
            PageMsg.Internal ()

        ManageTagsMsg (NavigateToListAll commonModel) ->
            PageMsg.NavigateToListAll commonModel

        ManageTagsMsg (NavigateToCreateOrEdit commonModel) ->
            PageMsg.NavigateToCreateOrEdit commonModel

        ManageTagsMsg (NavigateToEditTitleAndAbout commonModel) ->
            PageMsg.NavigateToEditTitleAndAbout commonModel

        ManageTagsMsg (NavigateToManageTags commonModel) ->
            PageMsg.NavigateToManageTags commonModel

        ManageTagsMsg (PageMsg.Internal _) ->
            PageMsg.Internal ()

        RequestUrl _ ->
            PageMsg.Internal ()

        NoOp ->
            PageMsg.Internal ()


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, withoutInternal msg, model ) of
        ( RequestUrl urlRequest, _, { key } ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , url
                        |> Url.toString
                        |> Browser.Navigation.pushUrl key
                    )

                Browser.External string ->
                    ( model
                    , Browser.Navigation.load string
                    )

        ( _, NavigateToListAll commonModel, _ ) ->
            let
                ( listAllModel, listAllCmd ) =
                    Pages.ListAll.init commonModel
            in
            ( { model | page = ListAll listAllModel }
            , Cmd.map ListAllMsg listAllCmd
            )

        ( _, NavigateToCreateOrEdit commonModel, _ ) ->
            let
                ( createOrEditModel, createOrEditCmd ) =
                    Pages.CreateOrEdit.init commonModel
            in
            ( { model | page = CreateOrEdit createOrEditModel }
            , Cmd.batch [ resetViewport, Cmd.map CreateOrEditMsg createOrEditCmd ]
            )

        ( _, NavigateToEditTitleAndAbout commonModel, _ ) ->
            let
                ( editTitleAndAboutModel, editTitleAndAboutCmd ) =
                    Pages.EditTitleAndAbout.init commonModel
            in
            ( { model | page = EditTitleAndAbout editTitleAndAboutModel }
            , Cmd.batch [ resetViewport, Cmd.map EditTitleAndAboutMsg editTitleAndAboutCmd ]
            )

        ( _, NavigateToManageTags commonModel, _ ) ->
            let
                ( manageTagsModel, manageTagsCmd ) =
                    Pages.ManageTags.init commonModel
            in
            ( { model | page = ManageTags manageTagsModel }
            , Cmd.batch [ resetViewport, Cmd.map ManageTagsMsg manageTagsCmd ]
            )

        ( ListAllMsg (PageMsg.Internal msg_), _, { page } ) ->
            case page of
                ListAll listAllModel ->
                    let
                        ( listAllModel_, listAllCmd ) =
                            Pages.ListAll.update msg_ listAllModel
                    in
                    ( { model | page = ListAll listAllModel_ }, listAllCmd |> Cmd.map ListAllMsg )

                _ ->
                    ( model, Cmd.none )

        ( CreateOrEditMsg (PageMsg.Internal msg_), _, { page } ) ->
            case page of
                CreateOrEdit createOrEditModel ->
                    let
                        ( createOrEditModel_, createOrEditCmd ) =
                            Pages.CreateOrEdit.update msg_ createOrEditModel
                    in
                    ( { model | page = CreateOrEdit createOrEditModel_ }, createOrEditCmd |> Cmd.map CreateOrEditMsg )

                _ ->
                    ( model, Cmd.none )

        ( EditTitleAndAboutMsg (PageMsg.Internal msg_), _, { page } ) ->
            case page of
                EditTitleAndAbout editTitleAndAboutModel ->
                    let
                        ( editTitleAndAboutModel_, editTitleAndAboutCmd ) =
                            Pages.EditTitleAndAbout.update msg_ editTitleAndAboutModel
                    in
                    ( { model | page = EditTitleAndAbout editTitleAndAboutModel_ }, editTitleAndAboutCmd |> Cmd.map EditTitleAndAboutMsg )

                _ ->
                    ( model, Cmd.none )

        ( ManageTagsMsg (PageMsg.Internal msg_), _, { page } ) ->
            case page of
                ManageTags manageTagsModel ->
                    let
                        ( manageTagsModel_, manageTagsCmd ) =
                            Pages.ManageTags.update msg_ manageTagsModel
                    in
                    ( { model | page = ManageTags manageTagsModel_ }, manageTagsCmd |> Cmd.map ManageTagsMsg )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


resetViewport : Cmd Msg
resetViewport =
    Task.perform (always NoOp) <| Dom.setViewport 0 0


onUrlChange : Url -> Msg
onUrlChange _ =
    NoOp



-- VIEW


view : Model -> Document Msg
view model =
    let
        mapDocument : (a -> msg) -> Document a -> Document msg
        mapDocument msg { title, body } =
            { title = title
            , body = body |> List.map (Html.map msg)
            }
    in
    case model.page of
        ListAll page ->
            page |> Pages.ListAll.view |> mapDocument ListAllMsg

        CreateOrEdit page ->
            page |> Pages.CreateOrEdit.view |> mapDocument CreateOrEditMsg

        EditTitleAndAbout page ->
            page |> Pages.EditTitleAndAbout.view |> mapDocument EditTitleAndAboutMsg

        ManageTags page ->
            page |> Pages.ManageTags.view |> mapDocument ManageTagsMsg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        ListAll page ->
            page |> Pages.ListAll.subscriptions |> Sub.map ListAllMsg

        CreateOrEdit page ->
            page |> Pages.CreateOrEdit.subscriptions |> Sub.map CreateOrEditMsg

        EditTitleAndAbout page ->
            page |> Pages.EditTitleAndAbout.subscriptions |> Sub.map EditTitleAndAboutMsg

        ManageTags page ->
            page |> Pages.ManageTags.subscriptions |> Sub.map ManageTagsMsg
