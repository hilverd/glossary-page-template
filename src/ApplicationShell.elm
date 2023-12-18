module ApplicationShell exposing (main, Flags, Page, Model, Msg)

{-| The application shell which shows different "pages" depending on the application state.


# Definition

@docs main, Flags, Page, Model, Msg

-}

import Browser exposing (Document, UrlRequest)
import Browser.Dom as Dom
import Browser.Navigation exposing (Key)
import Data.AboutLink as AboutLink exposing (AboutLink)
import Data.AboutParagraph as AboutParagraph exposing (AboutParagraph)
import Data.AboutSection exposing (AboutSection)
import Data.CardWidth as CardWidth exposing (CardWidth)
import Data.Glossary exposing (Glossary)
import Data.GlossaryItems exposing (GlossaryItems)
import Data.GlossaryTitle as GlossaryTitle exposing (GlossaryTitle)
import Data.LoadedGlossaryItems as LoadedGlossaryItems
import Data.Theme as Theme exposing (Theme)
import Html
import Internationalisation as I18n
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

        theme : Theme
        theme =
            flags
                |> Decode.decodeValue (Decode.field "theme" Theme.decode)
                |> Result.withDefault Theme.System

        editorIsRunning : Bool
        editorIsRunning =
            flags
                |> Decode.decodeValue (Decode.field "editorIsRunning" Decode.bool)
                |> Result.withDefault False

        enableHelpForMakingChanges : Bool
        enableHelpForMakingChanges =
            flags
                |> Decode.decodeValue (Decode.field "enableHelpForMakingChanges" Decode.bool)
                |> Result.withDefault False

        enableSavingChangesInMemory : Bool
        enableSavingChangesInMemory =
            flags
                |> Decode.decodeValue (Decode.field "enableSavingChangesInMemory" Decode.bool)
                |> Result.withDefault False

        loadedGlossaryItems : Result String GlossaryItems
        loadedGlossaryItems =
            LoadedGlossaryItems.decodeFromFlags flags

        glossary : Result String Glossary
        glossary =
            loadedGlossaryItems
                |> Result.map
                    (\items ->
                        let
                            enableLastUpdatedDates : Bool
                            enableLastUpdatedDates =
                                flags
                                    |> Decode.decodeValue (Decode.field "enableLastUpdatedDates" Decode.bool)
                                    |> Result.withDefault False

                            katexIsAvailable : Bool
                            katexIsAvailable =
                                flags
                                    |> Decode.decodeValue (Decode.field "katexIsAvailable" Decode.bool)
                                    |> Result.withDefault False

                            enableExportMenu : Bool
                            enableExportMenu =
                                flags
                                    |> Decode.decodeValue (Decode.field "enableExportMenu" Decode.bool)
                                    |> Result.withDefault True

                            enableOrderItemsButtons : Bool
                            enableOrderItemsButtons =
                                flags
                                    |> Decode.decodeValue (Decode.field "enableOrderItemsButtons" Decode.bool)
                                    |> Result.withDefault True

                            cardWidth : CardWidth
                            cardWidth =
                                flags
                                    |> Decode.decodeValue CardWidth.decode
                                    |> Result.withDefault CardWidth.Compact

                            title : GlossaryTitle
                            title =
                                flags
                                    |> Decode.decodeValue (Decode.field "titleString" Decode.string)
                                    |> Result.withDefault I18n.elementNotFound
                                    |> GlossaryTitle.fromMarkdown

                            aboutParagraph : AboutParagraph
                            aboutParagraph =
                                flags
                                    |> Decode.decodeValue (Decode.field "aboutParagraph" Decode.string)
                                    |> Result.withDefault I18n.elementNotFound
                                    |> AboutParagraph.fromMarkdown

                            aboutLinks : List AboutLink
                            aboutLinks =
                                flags
                                    |> Decode.decodeValue (Decode.field "aboutLinks" <| Decode.list AboutLink.decode)
                                    |> Result.withDefault []

                            aboutSection : AboutSection
                            aboutSection =
                                { paragraph = aboutParagraph, links = aboutLinks }
                        in
                        { enableMathSupport = katexIsAvailable
                        , enableLastUpdatedDates = enableLastUpdatedDates
                        , enableExportMenu = enableExportMenu
                        , enableHelpForMakingChanges = enableHelpForMakingChanges
                        , enableOrderItemsButtons = enableOrderItemsButtons
                        , cardWidth = cardWidth
                        , title = title
                        , aboutSection = aboutSection
                        , items = items
                        }
                    )

        ( listAllModel, listAllCmd ) =
            Pages.ListAll.init
                { enableHelpForMakingChanges = enableHelpForMakingChanges
                , editorIsRunning = editorIsRunning
                , currentlyEditing = False
                }
                { key = key
                , initialUrl = url
                , filename = filename
                , theme = theme
                , enableSavingChangesInMemory = enableSavingChangesInMemory
                , queryParameters = queryParameters
                , maybeId = Nothing
                , fragment = fragment
                , glossary = glossary
                }
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
                    Pages.ListAll.init
                        { enableHelpForMakingChanges = False
                        , editorIsRunning = True
                        , currentlyEditing = True
                        }
                        commonModel
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
