module ApplicationShell exposing (main, Flags, Model, Msg)

{-| The application shell which shows different "pages" depending on the application state.


# Definition

@docs main, Flags, Model, Msg

-}

import Browser exposing (Document)
import Browser.Dom as Dom
import Data.AboutLink as AboutLink exposing (AboutLink)
import Data.AboutParagraph as AboutParagraph exposing (AboutParagraph)
import Data.AboutSection exposing (AboutSection)
import Data.CardWidth as CardWidth exposing (CardWidth)
import Data.Glossary exposing (Glossary)
import Data.GlossaryItems exposing (GlossaryItems)
import Data.GlossaryTitle as GlossaryTitle exposing (GlossaryTitle)
import Data.LoadedGlossaryItems as LoadedGlossaryItems
import Data.OrderItemsBy as OrderItemsBy exposing (OrderItemsBy)
import Data.Theme as Theme exposing (Theme)
import Html
import Json.Decode as Decode
import PageMsg exposing (PageMsg(..))
import Pages.CreateOrEdit
import Pages.EditTitleAndAbout
import Pages.ListAll
import Pages.ManageTags
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


{-| Flags passed to the application from JavaScript.
-}
type alias Flags =
    Decode.Value


type Page
    = ListAll Pages.ListAll.Model
    | CreateOrEdit Pages.CreateOrEdit.Model
    | EditTitleAndAbout Pages.EditTitleAndAbout.Model
    | ManageTags Pages.ManageTags.Model


{-| A model for the application.
-}
type alias Model =
    Page


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        maybeUrl : Maybe Url.Url
        maybeUrl =
            flags
                |> Decode.decodeValue (Decode.field "windowLocationHref" Decode.string)
                |> Result.toMaybe
                |> Maybe.andThen Url.fromString

        filename : Maybe String
        filename =
            maybeUrl
                |> Maybe.map .path
                |> Maybe.andThen (String.split "/" >> List.reverse >> List.head)

        fragment : Maybe String
        fragment =
            maybeUrl |> Maybe.andThen .fragment

        theme : Theme
        theme =
            flags
                |> Decode.decodeValue (Decode.field "theme" Theme.decode)
                |> Result.withDefault Theme.System

        orderItemsBy : OrderItemsBy
        orderItemsBy =
            flags
                |> Decode.decodeValue (Decode.field "orderItemsBy" OrderItemsBy.decode)
                |> Result.withDefault OrderItemsBy.Alphabetically

        enableMarkdownBasedSyntax : Bool
        enableMarkdownBasedSyntax =
            flags
                |> Decode.decodeValue (Decode.field "enableMarkdownBasedSyntax" Decode.bool)
                |> Result.withDefault False

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

        enableSavingChangesInMemory : Bool
        enableSavingChangesInMemory =
            flags
                |> Decode.decodeValue (Decode.field "enableSavingChangesInMemory" Decode.bool)
                |> Result.withDefault False

        loadedGlossaryItems : Result String GlossaryItems
        loadedGlossaryItems =
            LoadedGlossaryItems.decodeFromFlags enableMarkdownBasedSyntax flags

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

                            cardWidth : CardWidth
                            cardWidth =
                                flags
                                    |> Decode.decodeValue CardWidth.decode
                                    |> Result.withDefault CardWidth.Compact

                            title : GlossaryTitle
                            title =
                                flags
                                    |> Decode.decodeValue (Decode.field "titleString" Decode.string)
                                    |> Result.withDefault "Element not found"
                                    |> (if enableMarkdownBasedSyntax then
                                            GlossaryTitle.fromMarkdown

                                        else
                                            GlossaryTitle.fromPlaintext
                                       )

                            aboutParagraph : AboutParagraph
                            aboutParagraph =
                                flags
                                    |> Decode.decodeValue (Decode.field "aboutParagraph" Decode.string)
                                    |> Result.withDefault "Element not found"
                                    |> (if enableMarkdownBasedSyntax then
                                            AboutParagraph.fromMarkdown

                                        else
                                            AboutParagraph.fromPlaintext
                                       )

                            aboutLinks : List AboutLink
                            aboutLinks =
                                flags
                                    |> Decode.decodeValue (Decode.field "aboutLinks" <| Decode.list AboutLink.decode)
                                    |> Result.withDefault []

                            aboutSection : AboutSection
                            aboutSection =
                                { paragraph = aboutParagraph, links = aboutLinks }
                        in
                        { enableMarkdownBasedSyntax = enableMarkdownBasedSyntax
                        , enableMathSupport = enableMarkdownBasedSyntax && katexIsAvailable
                        , enableLastUpdatedDates = enableLastUpdatedDates
                        , cardWidth = cardWidth
                        , title = title
                        , aboutSection = aboutSection
                        , items = items
                        }
                    )

        ( listAllModel, listAllCmd ) =
            Pages.ListAll.init
                editorIsRunning
                False
                { filename = filename
                , enableHelpForMakingChanges = enableHelpForMakingChanges
                , theme = theme
                , enableExportMenu = enableExportMenu
                , enableOrderItemsButtons = enableOrderItemsButtons
                , enableSavingChangesInMemory = enableSavingChangesInMemory
                , filterByTag = Nothing
                , orderItemsBy = orderItemsBy
                , maybeId = Nothing
                , fragment = fragment
                , glossary = glossary
                }
    in
    ( ListAll listAllModel
    , Cmd.map ListAllMsg listAllCmd
    )


{-| Messages handled by the application.
-}
type Msg
    = NoOp
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

        NoOp ->
            PageMsg.Internal ()


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, withoutInternal msg, model ) of
        ( _, NavigateToListAll commonModel, _ ) ->
            let
                ( listAllModel, listAllCmd ) =
                    Pages.ListAll.init True True commonModel
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

        ( _, NavigateToManageTags commonModel, _ ) ->
            let
                ( manageTagsModel, manageTagsCmd ) =
                    Pages.ManageTags.init commonModel
            in
            ( ManageTags manageTagsModel
            , Cmd.batch [ resetViewport, Cmd.map ManageTagsMsg manageTagsCmd ]
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

        ( ManageTagsMsg (PageMsg.Internal msg_), _, ManageTags manageTagsModel ) ->
            let
                ( manageTagsModel_, manageTagsCmd ) =
                    Pages.ManageTags.update msg_ manageTagsModel
            in
            ( ManageTags manageTagsModel_, manageTagsCmd |> Cmd.map ManageTagsMsg )

        _ ->
            ( model, Cmd.none )


resetViewport : Cmd Msg
resetViewport =
    Task.perform (always NoOp) <| Dom.setViewport 0 0



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
    case model of
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
    case model of
        ListAll page ->
            page |> Pages.ListAll.subscriptions |> Sub.map ListAllMsg

        CreateOrEdit page ->
            page |> Pages.CreateOrEdit.subscriptions |> Sub.map CreateOrEditMsg

        EditTitleAndAbout page ->
            page |> Pages.EditTitleAndAbout.subscriptions |> Sub.map EditTitleAndAboutMsg

        ManageTags page ->
            page |> Pages.ManageTags.subscriptions |> Sub.map ManageTagsMsg
