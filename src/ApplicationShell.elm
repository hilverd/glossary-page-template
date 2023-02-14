module ApplicationShell exposing (main)

{-| The application shell which shows different "pages" depending on the application state.


# Definition

@docs main

-}

import Browser exposing (Document)
import Browser.Dom as Dom
import CommonModel exposing (CommonModel)
import Data.AboutLink as AboutLink
import Data.AboutParagraph as AboutParagraph
import Data.CardWidth as CardWidth
import Data.GlossaryTitle as GlossaryTitle
import Data.LoadedGlossaryItems as LoadedGlossaryItems
import Html
import Json.Decode as Decode
import PageMsg exposing (PageMsg(..))
import Pages.CreateOrEdit
import Pages.EditTitleAndAbout
import Pages.ListAll
import Task
import Url



-- MODEL


{-| The main function for the application.
-}
main : Program Flags Model Msg
main =
    Browser.document
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
    | EditTitleAndAbout Pages.EditTitleAndAbout.Model


type alias Model =
    Page


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        maybeUrl =
            flags
                |> Decode.decodeValue (Decode.field "windowLocationHref" Decode.string)
                |> Result.toMaybe
                |> Maybe.andThen Url.fromString

        filename =
            maybeUrl
                |> Maybe.map .path
                |> Maybe.andThen (String.split "/" >> List.reverse >> List.head)

        fragment =
            maybeUrl |> Maybe.andThen .fragment

        enableMarkdownBasedSyntax =
            flags
                |> Decode.decodeValue (Decode.field "enableMarkdownBasedSyntax" Decode.bool)
                |> Result.withDefault False

        title =
            flags
                |> Decode.decodeValue (Decode.field "titleString" Decode.string)
                |> Result.withDefault "Element not found"
                |> GlossaryTitle.fromString

        aboutParagraph =
            flags
                |> Decode.decodeValue (Decode.field "aboutParagraph" Decode.string)
                |> Result.withDefault "Element not found"
                |> (if enableMarkdownBasedSyntax then
                        AboutParagraph.fromMarkdown

                    else
                        AboutParagraph.fromPlaintext
                   )

        aboutLinks =
            flags
                |> Decode.decodeValue (Decode.field "aboutLinks" <| Decode.list AboutLink.decode)
                |> Result.withDefault []

        aboutSection =
            { paragraph = aboutParagraph, links = aboutLinks }

        editorIsRunning =
            flags
                |> Decode.decodeValue (Decode.field "editorIsRunning" Decode.bool)
                |> Result.withDefault False

        enableHelpForMakingChanges =
            flags
                |> Decode.decodeValue (Decode.field "enableHelpForMakingChanges" Decode.bool)
                |> Result.withDefault False

        enableExportMenu =
            flags
                |> Decode.decodeValue (Decode.field "enableExportMenu" Decode.bool)
                |> Result.withDefault True

        cardWidth =
            flags
                |> Decode.decodeValue CardWidth.decode
                |> Result.withDefault CardWidth.Compact

        enableSavingChangesInMemory =
            flags
                |> Decode.decodeValue (Decode.field "enableSavingChangesInMemory" Decode.bool)
                |> Result.withDefault False

        loadedGlossaryItems =
            LoadedGlossaryItems.decodeFromFlags enableMarkdownBasedSyntax flags

        glossary =
            loadedGlossaryItems
                |> Result.map
                    (\items ->
                        { enableMarkdownBasedSyntax = enableMarkdownBasedSyntax
                        , cardWidth = cardWidth
                        , title = title
                        , aboutSection = aboutSection
                        , items = items
                        }
                    )

        ( listAllModel, listAllCmd ) =
            Pages.ListAll.init
                editorIsRunning
                { filename = filename
                , enableHelpForMakingChanges = enableHelpForMakingChanges
                , enableExportMenu = enableExportMenu
                , enableSavingChangesInMemory = enableSavingChangesInMemory
                , orderItemsBy = CommonModel.Alphabetically
                , maybeIndex = Nothing
                , fragment = fragment
                , glossary = glossary
                }
    in
    ( ListAll listAllModel
    , Cmd.map ListAllMsg listAllCmd
    )


type Msg
    = NoOp
    | ListAllMsg Pages.ListAll.Msg
    | CreateOrEditMsg Pages.CreateOrEdit.Msg
    | EditTitleAndAboutMsg Pages.EditTitleAndAbout.Msg



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

        ListAllMsg (PageMsg.Internal _) ->
            PageMsg.Internal ()

        CreateOrEditMsg (NavigateToListAll commonModel) ->
            PageMsg.NavigateToListAll commonModel

        CreateOrEditMsg (NavigateToCreateOrEdit commonModel) ->
            PageMsg.NavigateToCreateOrEdit commonModel

        CreateOrEditMsg (NavigateToEditTitleAndAbout commonModel) ->
            PageMsg.NavigateToEditTitleAndAbout commonModel

        CreateOrEditMsg (PageMsg.Internal _) ->
            PageMsg.Internal ()

        EditTitleAndAboutMsg (NavigateToListAll commonModel) ->
            PageMsg.NavigateToListAll commonModel

        EditTitleAndAboutMsg (NavigateToCreateOrEdit commonModel) ->
            PageMsg.NavigateToCreateOrEdit commonModel

        EditTitleAndAboutMsg (NavigateToEditTitleAndAbout commonModel) ->
            PageMsg.NavigateToEditTitleAndAbout commonModel

        EditTitleAndAboutMsg (PageMsg.Internal _) ->
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

        ( _, NavigateToEditTitleAndAbout commonModel, _ ) ->
            let
                ( editTitleAndAboutModel, editTitleAndAboutCmd ) =
                    Pages.EditTitleAndAbout.init commonModel
            in
            ( EditTitleAndAbout editTitleAndAboutModel
            , Cmd.batch [ resetViewport, Cmd.map EditTitleAndAboutMsg editTitleAndAboutCmd ]
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

        ( EditTitleAndAboutMsg (PageMsg.Internal msg_), _, EditTitleAndAbout editTitleAndAboutModel ) ->
            let
                ( editTitleAndAboutModel_, editTitleAndAboutCmd ) =
                    Pages.EditTitleAndAbout.update msg_ editTitleAndAboutModel
            in
            ( EditTitleAndAbout editTitleAndAboutModel_, editTitleAndAboutCmd |> Cmd.map EditTitleAndAboutMsg )

        _ ->
            ( model, Cmd.none )


resetViewport : Cmd Msg
resetViewport =
    Task.perform (always NoOp) <| Dom.setViewport 0 0



-- VIEW


view : Model -> Document Msg
view model =
    let
        mapDocument msg { title, body } =
            { title = title
            , body = body |> List.map (Html.map msg)
            }
    in
    case model of
        ListAll page ->
            page |> Pages.ListAll.view |> mapDocument ListAllMsg

        CreateOrEdit page ->
            page |> Pages.CreateOrEdit.view |> mapDocument CreateOrEditMsg

        EditTitleAndAbout page ->
            page |> Pages.EditTitleAndAbout.view |> mapDocument EditTitleAndAboutMsg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        ListAll page ->
            page |> Pages.ListAll.subscriptions |> Sub.map ListAllMsg

        CreateOrEdit page ->
            page |> Pages.CreateOrEdit.subscriptions |> Sub.map CreateOrEditMsg

        EditTitleAndAbout page ->
            page |> Pages.EditTitleAndAbout.subscriptions |> Sub.map EditTitleAndAboutMsg
