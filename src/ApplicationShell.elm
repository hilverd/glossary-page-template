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
import Components.Notifications
import Data.Editability as Editability exposing (Editability)
import Data.GlossaryForUi as GlossaryForUi exposing (GlossaryForUi)
import Data.GlossaryFromDom as GlossaryFromDom
import Data.GlossaryItem.DisambiguatedTerm as DisambiguatedTerm
import Data.GlossaryItem.Term as Term
import Data.GlossaryItemForUi as GlossaryItemForUi exposing (GlossaryItemForUi)
import Data.GlossaryItemId exposing (GlossaryItemId)
import Data.GlossaryItemsForUi as GlossaryItems exposing (GlossaryItemsForUi)
import Data.Theme as Theme exposing (Theme)
import Extras.BrowserDom
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
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
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


commonModelForPage : Page -> CommonModel
commonModelForPage page =
    case page of
        ListAll { common } ->
            common

        CreateOrEdit { common } ->
            common

        EditTitleAndAbout { common } ->
            common

        ManageTags { common } ->
            common


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        runningOnMacOs : Bool
        runningOnMacOs =
            flags
                |> Decode.decodeValue (Decode.field "runningOnMacOs" Decode.bool)
                |> Result.withDefault False

        queryParameters : QueryParameters
        queryParameters =
            QueryParameters.fromUrl url

        filename : Maybe String
        filename =
            url.path
                |> String.split "/"
                |> List.reverse
                |> List.head

        maybeFragment : Maybe String
        maybeFragment =
            Maybe.andThen Url.percentDecode url.fragment

        glossaryForUi : Result String GlossaryForUi
        glossaryForUi =
            flags
                |> Decode.decodeValue (Codec.decoder GlossaryFromDom.codec)
                |> Result.mapError Decode.errorToString
                |> Result.map GlossaryForUi.fromGlossaryFromDom

        itemWithFocus : Maybe GlossaryItemId
        itemWithFocus =
            Maybe.map2
                glossaryItemIdForFragment
                maybeFragment
                (Result.toMaybe glossaryForUi)
                |> Maybe.andThen identity
                |> (\itemWithFocus_ ->
                        case itemWithFocus_ of
                            Just _ ->
                                itemWithFocus_

                            Nothing ->
                                startingItem glossaryForUi
                   )

        enableHelpForMakingChanges : Bool
        enableHelpForMakingChanges =
            glossaryForUi
                |> Result.map GlossaryForUi.enableHelpForMakingChanges
                |> Result.withDefault False

        usingIncludedBackend : Bool
        usingIncludedBackend =
            flags
                |> Decode.decodeValue (Decode.field "usingIncludedBackend" Decode.bool)
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

        bearerToken : Maybe String
        bearerToken =
            flags
                |> Decode.decodeValue (Decode.field "bearerToken" Decode.string)
                |> Result.toMaybe

        userName : Maybe String
        userName =
            flags
                |> Decode.decodeValue (Decode.field "userName" Decode.string)
                |> Result.toMaybe

        userEmailAddress : Maybe String
        userEmailAddress =
            flags
                |> Decode.decodeValue (Decode.field "userEmailAddress" Decode.string)
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

        editability : Editability
        editability =
            Editability.create
                { enableHelpForMakingChanges = enableHelpForMakingChanges
                , enableSavingChangesInMemory = enableSavingChangesInMemory
                , separateBackendBaseUrl = separateBackendBaseUrl
                , bearerToken = bearerToken
                , userName = userName
                , userEmailAddress = userEmailAddress
                , usingIncludedBackend = usingIncludedBackend
                , currentlyEditing = False
                }

        common : CommonModel
        common =
            { key = key
            , initialUrl = url
            , runningOnMacOs = runningOnMacOs
            , filename = filename
            , theme = theme
            , editability = editability
            , enableMathSupport = katexIsAvailable
            , queryParameters = queryParameters
            , fragment = maybeFragment
            , glossaryForUi = glossaryForUi
            }

        ( listAllModel, listAllCmd ) =
            Pages.ListAll.init common itemWithFocus Nothing Nothing
    in
    ( { key = key, page = ListAll listAllModel }
    , Cmd.map ListAllMsg listAllCmd
    )


startingItem : Result a GlossaryForUi -> Maybe GlossaryItemId
startingItem glossaryForUi =
    glossaryForUi
        |> Result.toMaybe
        |> Maybe.map GlossaryForUi.items
        |> Maybe.andThen GlossaryItems.startingItem
        |> Maybe.map GlossaryItemForUi.id


{-| Messages handled by the application.
-}
type Msg
    = NoOp
    | UrlRequested UrlRequest
    | UrlChanged Url
    | ListAllMsg Pages.ListAll.Msg
    | CreateOrEditMsg Pages.CreateOrEdit.Msg
    | EditTitleAndAboutMsg Pages.EditTitleAndAbout.Msg
    | ManageTagsMsg Pages.ManageTags.Msg



-- UPDATE


withoutInternal : Msg -> PageMsg ()
withoutInternal msg =
    case msg of
        ListAllMsg (NavigateToListAll commonModel itemWithFocus notification) ->
            PageMsg.NavigateToListAll commonModel itemWithFocus notification

        ListAllMsg (NavigateToCreateOrEdit itemWithFocus itemBeingEdited) ->
            PageMsg.NavigateToCreateOrEdit itemWithFocus itemBeingEdited

        ListAllMsg NavigateToEditTitleAndAbout ->
            PageMsg.NavigateToEditTitleAndAbout

        ListAllMsg NavigateToManageTags ->
            PageMsg.NavigateToManageTags

        ListAllMsg (PageMsg.Internal _) ->
            PageMsg.Internal ()

        CreateOrEditMsg (NavigateToListAll commonModel itemWithFocus notification) ->
            PageMsg.NavigateToListAll commonModel itemWithFocus notification

        CreateOrEditMsg (NavigateToCreateOrEdit itemWithFocus itemBeingEdited) ->
            PageMsg.NavigateToCreateOrEdit itemWithFocus itemBeingEdited

        CreateOrEditMsg NavigateToEditTitleAndAbout ->
            PageMsg.NavigateToEditTitleAndAbout

        CreateOrEditMsg NavigateToManageTags ->
            PageMsg.NavigateToManageTags

        CreateOrEditMsg (PageMsg.Internal _) ->
            PageMsg.Internal ()

        EditTitleAndAboutMsg (NavigateToListAll commonModel itemWithFocus notification) ->
            PageMsg.NavigateToListAll commonModel itemWithFocus notification

        EditTitleAndAboutMsg (NavigateToCreateOrEdit itemWithFocus itemBeingEdited) ->
            PageMsg.NavigateToCreateOrEdit itemWithFocus itemBeingEdited

        EditTitleAndAboutMsg NavigateToEditTitleAndAbout ->
            PageMsg.NavigateToEditTitleAndAbout

        EditTitleAndAboutMsg NavigateToManageTags ->
            PageMsg.NavigateToManageTags

        EditTitleAndAboutMsg (PageMsg.Internal _) ->
            PageMsg.Internal ()

        ManageTagsMsg (NavigateToListAll commonModel itemWithFocus notification) ->
            PageMsg.NavigateToListAll commonModel itemWithFocus notification

        ManageTagsMsg (NavigateToCreateOrEdit itemWithFocus itemBeingEdited) ->
            PageMsg.NavigateToCreateOrEdit itemWithFocus itemBeingEdited

        ManageTagsMsg NavigateToEditTitleAndAbout ->
            PageMsg.NavigateToEditTitleAndAbout

        ManageTagsMsg NavigateToManageTags ->
            PageMsg.NavigateToManageTags

        ManageTagsMsg (PageMsg.Internal _) ->
            PageMsg.Internal ()

        UrlRequested _ ->
            PageMsg.Internal ()

        UrlChanged _ ->
            PageMsg.Internal ()

        NoOp ->
            PageMsg.Internal ()


glossaryItemIdForFragment : String -> GlossaryForUi -> Maybe GlossaryItemId
glossaryItemIdForFragment fragment =
    GlossaryForUi.items
        >> GlossaryItems.itemIdFromFragmentIdentifier fragment


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, withoutInternal msg, model ) of
        -- This typically means a link has been clicked
        ( UrlRequested urlRequest, _, { key } ) ->
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

        -- This typically means the URL has been changed using pushUrl or replaceUrl.
        -- One way this can happen is via the browser's back/forward buttons.
        ( UrlChanged url, _, { page } ) ->
            case page of
                ListAll listAllModel ->
                    let
                        common0 : CommonModel
                        common0 =
                            listAllModel.common

                        queryParameters : QueryParameters
                        queryParameters =
                            QueryParameters.fromUrl url

                        maybeFragment : Maybe String
                        maybeFragment =
                            Maybe.andThen Url.percentDecode url.fragment

                        itemWithFocus : Maybe GlossaryItemId
                        itemWithFocus =
                            Maybe.map2
                                glossaryItemIdForFragment
                                maybeFragment
                                (Result.toMaybe common0.glossaryForUi)
                                |> Maybe.andThen identity
                                |> (\itemWithFocus_ ->
                                        case itemWithFocus_ of
                                            Just _ ->
                                                itemWithFocus_

                                            Nothing ->
                                                startingItem common0.glossaryForUi
                                   )

                        common1 : CommonModel
                        common1 =
                            { common0
                                | queryParameters = queryParameters
                                , fragment = maybeFragment
                            }

                        enableThreeColumnLayout : Bool
                        enableThreeColumnLayout =
                            common1.glossaryForUi
                                |> Result.map GlossaryForUi.enableThreeColumnLayout
                                |> Result.withDefault False

                        listAllModel1 : Pages.ListAll.Model
                        listAllModel1 =
                            { listAllModel
                                | common = common1
                                , itemWithFocus = itemWithFocus
                            }
                    in
                    ( if listAllModel == listAllModel1 then
                        -- Avoid constructing a new model as that might mess up Html.Lazy calls
                        model

                      else
                        { model
                            | page =
                                Pages.ListAll.init common1 itemWithFocus Nothing Nothing
                                    |> Tuple.first
                                    |> ListAll
                        }
                    , if enableThreeColumnLayout then
                        Extras.BrowserDom.scrollToTop NoOp

                      else
                        Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ( _, NavigateToListAll commonModel itemWithFocus notification, { page } ) ->
            let
                notifications0 : Maybe Components.Notifications.Model
                notifications0 =
                    case page of
                        ListAll { notifications } ->
                            Just notifications

                        _ ->
                            Nothing

                glossaryItemsForUi : Maybe GlossaryItemsForUi
                glossaryItemsForUi =
                    commonModel.glossaryForUi
                        |> Result.toMaybe
                        |> Maybe.map GlossaryForUi.items

                maybeFragment : Maybe String
                maybeFragment =
                    Maybe.map2
                        (\glossaryItemsForUi_ itemWithFocus_ ->
                            let
                                item : Maybe GlossaryItemForUi
                                item =
                                    GlossaryItems.get itemWithFocus_ glossaryItemsForUi_

                                fragment : Maybe String
                                fragment =
                                    item
                                        |> Maybe.map GlossaryItemForUi.disambiguatedPreferredTerm
                                        |> Maybe.map DisambiguatedTerm.toTerm
                                        |> Maybe.map Term.id
                            in
                            fragment
                        )
                        glossaryItemsForUi
                        itemWithFocus
                        |> Maybe.andThen identity

                -- Update the CommonModel to include the fragment for the focused item
                commonModelWithFragment : CommonModel
                commonModelWithFragment =
                    case maybeFragment of
                        Just fragment ->
                            { commonModel | fragment = Just fragment }

                        Nothing ->
                            commonModel

                ( listAllModel, listAllCmd ) =
                    Pages.ListAll.init
                        commonModelWithFragment
                        (if itemWithFocus == Nothing then
                            startingItem commonModel.glossaryForUi

                         else
                            itemWithFocus
                        )
                        notifications0
                        notification

                -- Update the URL with the fragment
                urlUpdateCmd : Cmd Msg
                urlUpdateCmd =
                    case maybeFragment of
                        Just fragment ->
                            let
                                url =
                                    CommonModel.initialUrlWithoutQueryOrFragment commonModelWithFragment ++ "#" ++ fragment
                            in
                            Browser.Navigation.pushUrl commonModelWithFragment.key url

                        Nothing ->
                            Cmd.none
            in
            ( { model | page = ListAll listAllModel }
            , Cmd.batch [ Cmd.map ListAllMsg listAllCmd, urlUpdateCmd ]
            )

        ( _, NavigateToCreateOrEdit itemWithFocus itemBeingEdited, _ ) ->
            let
                common0 : CommonModel
                common0 =
                    commonModelForPage model.page

                ( createOrEditModel, createOrEditCmd ) =
                    Pages.CreateOrEdit.init common0 itemWithFocus itemBeingEdited
            in
            ( { model | page = CreateOrEdit createOrEditModel }
            , Cmd.batch [ resetViewport, Cmd.map CreateOrEditMsg createOrEditCmd ]
            )

        ( _, NavigateToEditTitleAndAbout, _ ) ->
            let
                common0 : CommonModel
                common0 =
                    commonModelForPage model.page

                ( editTitleAndAboutModel, editTitleAndAboutCmd ) =
                    Pages.EditTitleAndAbout.init common0
            in
            ( { model | page = EditTitleAndAbout editTitleAndAboutModel }
            , Cmd.batch [ resetViewport, Cmd.map EditTitleAndAboutMsg editTitleAndAboutCmd ]
            )

        ( _, NavigateToManageTags, _ ) ->
            let
                common0 : CommonModel
                common0 =
                    commonModelForPage model.page

                ( manageTagsModel, manageTagsCmd ) =
                    Pages.ManageTags.init common0
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
