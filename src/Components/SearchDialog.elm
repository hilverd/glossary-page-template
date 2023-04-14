port module Components.SearchDialog exposing
    ( Model
    , Msg
    , Property
    , SearchResult
    , hide
    , init
    , onChangeSearchString
    , onHide
    , onShow
    , searchResult
    , show
    , update
    , view
    , visible
    )

import Accessibility exposing (Html, div, p, text, ul)
import Accessibility.Aria
import Accessibility.Key
import Accessibility.Role
import Array
import Browser.Dom as Dom
import Browser.Navigation
import Extras.Html
import Extras.HtmlAttribute
import Extras.HtmlEvents
import Html
import Html.Attributes exposing (class)
import Html.Events
import Process
import Svg
import Svg.Attributes
import Task



-- MODEL


type GradualVisibility
    = Visible
    | Disappearing
    | Invisible


type Model parentMsg
    = Model
        { idPrefix : String
        , visibility : GradualVisibility
        , config : Config parentMsg
        , activeSearchResultIndex : Maybe SearchResultIndex
        }


type alias SearchResultIndex =
    Int


type alias Config parentMsg =
    { onChangeSearchString : Maybe (String -> parentMsg)
    , onShow : Maybe (Cmd parentMsg)
    , onHide : Maybe (Cmd parentMsg)
    }


type Property parentMsg
    = OnChangeSearchString (String -> parentMsg)
    | OnShow (Cmd parentMsg)
    | OnHide (Cmd parentMsg)


visible : Model parentMsg -> Bool
visible =
    innerModel
        >> .visibility
        >> (/=) Invisible


init : String -> List (Property parentMsg) -> Model parentMsg
init idPrefix properties =
    Model
        { idPrefix = idPrefix
        , visibility = Invisible
        , config = configFromProperties properties
        , activeSearchResultIndex = Nothing
        }


innerModel :
    Model parentMsg
    ->
        { idPrefix : String
        , visibility : GradualVisibility
        , config : Config parentMsg
        , activeSearchResultIndex : Maybe SearchResultIndex
        }
innerModel model =
    case model of
        Model model_ ->
            model_



-- PORTS


port scrollSearchResultIntoView : String -> Cmd msg



-- UPDATE


type Msg
    = NoOp
    | Show
    | FocusOnSearchStringInputField String
    | StartHiding
    | CompleteHiding
    | MakeSearchResultActive SearchResultIndex
    | MakeSearchResultInactive SearchResultIndex
    | MakePreviousOrNextSearchResultActive Int Bool
    | LoadUrl String
    | LoadUrlForActiveSearchResult (List SearchResult)


show : Msg
show =
    Show


hide : Msg
hide =
    StartHiding


update : (Model parentMsg -> parentModel) -> (Msg -> parentMsg) -> Msg -> Model parentMsg -> ( parentModel, Cmd parentMsg )
update updateParentModel toParentMsg msg model =
    let
        model_ : { idPrefix : String, visibility : GradualVisibility, config : Config parentMsg, activeSearchResultIndex : Maybe SearchResultIndex }
        model_ =
            innerModel model

        ( model1, cmd ) =
            case msg of
                NoOp ->
                    ( model_, Cmd.none )

                Show ->
                    ( { model_ | visibility = Visible, activeSearchResultIndex = Nothing }
                    , Cmd.batch
                        [ model_.config.onShow |> Maybe.withDefault Cmd.none
                        , Process.sleep 200
                            |> Task.perform (always <| FocusOnSearchStringInputField <| searchStringFieldId model_.idPrefix)
                            |> Cmd.map toParentMsg
                        ]
                    )

                FocusOnSearchStringInputField elementId ->
                    ( model_
                    , Task.attempt (always <| toParentMsg NoOp) (Dom.focus elementId)
                    )

                StartHiding ->
                    ( { model_ | visibility = Disappearing, activeSearchResultIndex = Nothing }
                    , Process.sleep 100 |> Task.perform (always CompleteHiding) |> Cmd.map toParentMsg
                    )

                CompleteHiding ->
                    ( { model_ | visibility = Invisible, activeSearchResultIndex = Nothing }
                    , model_.config.onHide
                        |> Maybe.withDefault Cmd.none
                    )

                MakeSearchResultActive searchResultIndex ->
                    ( { model_ | activeSearchResultIndex = Just searchResultIndex }
                    , scrollSearchResultIntoView <| searchResultId searchResultIndex model_.idPrefix
                    )

                MakeSearchResultInactive searchResultIndex ->
                    ( { model_
                        | activeSearchResultIndex =
                            if model_.activeSearchResultIndex == Just searchResultIndex then
                                Nothing

                            else
                                model_.activeSearchResultIndex
                      }
                    , Cmd.none
                    )

                MakePreviousOrNextSearchResultActive numberOfSearchResults next ->
                    let
                        delta : Int
                        delta =
                            if next then
                                1

                            else
                                -1

                        initial : Int
                        initial =
                            if next then
                                0

                            else
                                numberOfSearchResults - 1

                        activeSearchResultIndex_ : Maybe Int
                        activeSearchResultIndex_ =
                            model_.activeSearchResultIndex
                                |> Maybe.map ((+) delta >> min (numberOfSearchResults - 1) >> max 0)
                                |> Maybe.withDefault initial
                                |> (\new ->
                                        if numberOfSearchResults == 0 then
                                            Nothing

                                        else
                                            Just new
                                   )
                    in
                    ( { model_ | activeSearchResultIndex = activeSearchResultIndex_ }
                    , activeSearchResultIndex_
                        |> Maybe.map (\index -> scrollSearchResultIntoView <| searchResultId index model_.idPrefix)
                        |> Maybe.withDefault Cmd.none
                        |> Cmd.map toParentMsg
                    )

                LoadUrl url ->
                    loadUrl toParentMsg model_ url

                LoadUrlForActiveSearchResult searchResults ->
                    let
                        activeSearchResult : Maybe String
                        activeSearchResult =
                            model_.activeSearchResultIndex
                                |> Maybe.andThen
                                    (\activeSearchResultIndex ->
                                        Array.fromList searchResults
                                            |> Array.get activeSearchResultIndex
                                    )
                                |> Maybe.map
                                    (\searchResult_ ->
                                        case searchResult_ of
                                            SearchResult { href } ->
                                                href
                                    )
                    in
                    activeSearchResult
                        |> Maybe.map (loadUrl toParentMsg model_)
                        |> Maybe.withDefault ( model_, Cmd.none )
    in
    ( updateParentModel <| Model model1, cmd )


loadUrl :
    (Msg -> parentMsg)
    ->
        { idPrefix : String
        , visibility : GradualVisibility
        , config : Config parentMsg
        , activeSearchResultIndex : Maybe SearchResultIndex
        }
    -> String
    ->
        ( { idPrefix : String
          , visibility : GradualVisibility
          , config : Config parentMsg
          , activeSearchResultIndex : Maybe SearchResultIndex
          }
        , Cmd parentMsg
        )
loadUrl toParentMsg model url =
    ( { model | visibility = Disappearing, activeSearchResultIndex = Nothing }
    , Cmd.batch
        [ Process.sleep 100 |> Task.perform (always CompleteHiding) |> Cmd.map toParentMsg
        , Browser.Navigation.load url
        ]
    )



-- VIEW


type SearchResult
    = SearchResult
        { href : String
        , body : List (Html Never)
        }


searchResult : String -> List (Html Never) -> SearchResult
searchResult href body =
    SearchResult { href = href, body = body }


onChangeSearchString : (String -> parentMsg) -> Property parentMsg
onChangeSearchString =
    OnChangeSearchString


onShow : Cmd parentMsg -> Property parentMsg
onShow =
    OnShow


onHide : Cmd parentMsg -> Property parentMsg
onHide =
    OnHide


withIdPrefix : String -> String -> String
withIdPrefix suffix idPrefix =
    idPrefix ++ "-" ++ suffix


searchStringFieldId : String -> String
searchStringFieldId =
    withIdPrefix "search-term-field"


searchResultsListId : String -> String
searchResultsListId =
    withIdPrefix "search-results-list"


searchResultId : Int -> String -> String
searchResultId index =
    withIdPrefix <| "result-" ++ String.fromInt index


configFromProperties : List (Property parentMsg) -> Config parentMsg
configFromProperties =
    List.foldl
        (\property config ->
            case property of
                OnChangeSearchString f ->
                    { config | onChangeSearchString = Just f }

                OnShow parentMsg ->
                    { config | onShow = Just parentMsg }

                OnHide parentMsg ->
                    { config | onHide = Just parentMsg }
        )
        { onChangeSearchString = Nothing
        , onShow = Nothing
        , onHide = Nothing
        }


view :
    (Msg -> parentMsg)
    -> Model parentMsg
    -> String
    -> List SearchResult
    -> Html parentMsg
view toParentMsg model searchString searchResults =
    let
        model_ :
            { idPrefix : String
            , visibility : GradualVisibility
            , config : Config parentMsg
            , activeSearchResultIndex : Maybe SearchResultIndex
            }
        model_ =
            innerModel model

        config : Config parentMsg
        config =
            model_.config
    in
    div
        [ Accessibility.Aria.modal True
        , Accessibility.Role.dialog
        , class "relative z-20"
        ]
        [ Html.div
            [ class "fixed inset-0 bg-gray-500 dark:bg-gray-800 bg-opacity-50 dark:bg-opacity-75 transition-opacity motion-reduce:transition-none"
            , class "invisible" |> Extras.HtmlAttribute.showIf (model_.visibility == Invisible)
            , if model_.visibility == Visible then
                class "ease-out duration-300 opacity-100"

              else
                class "ease-in duration-200 opacity-0"
            ]
            []
        , Html.div
            [ class "fixed inset-0 z-20 overflow-y-auto p-4 sm:p-6 md:p-20"
            , class "invisible" |> Extras.HtmlAttribute.showIf (model_.visibility == Invisible)
            , Extras.HtmlEvents.onClickPreventDefaultAndStopPropagation <|
                toParentMsg <|
                    case model_.visibility of
                        Visible ->
                            StartHiding

                        _ ->
                            NoOp
            ]
            [ Html.div
                [ class "mx-auto max-w-xl transform motion-reduce:transform-none divide-y divide-gray-100 dark:divide-gray-800 overflow-hidden rounded-xl bg-white dark:bg-gray-700 shadow-2xl ring-1 ring-black ring-opacity-5 transition-all motion-reduce:transition-none"
                , class "invisible" |> Extras.HtmlAttribute.showIf (model_.visibility == Invisible)
                , if model_.visibility == Visible then
                    class "ease-out duration-300 opacity-100 scale-100"

                  else
                    class "ease-in duration-200 opacity-0 scale-95"
                , Extras.HtmlEvents.onClickPreventDefaultAndStopPropagation <|
                    toParentMsg <|
                        NoOp
                ]
                [ div
                    [ class "relative" ]
                    [ Svg.svg
                        [ Svg.Attributes.class "pointer-events-none absolute top-3.5 left-4 h-5 w-5 text-gray-400 dark:text-gray-300"
                        , Svg.Attributes.viewBox "0 0 20 20"
                        , Svg.Attributes.fill "currentColor"
                        ]
                        [ Svg.path
                            [ Svg.Attributes.fillRule "evenodd"
                            , Svg.Attributes.d "M8 4a4 4 0 100 8 4 4 0 000-8zM2 8a6 6 0 1110.89 3.476l4.817 4.817a1 1 0 01-1.414 1.414l-4.816-4.816A6 6 0 012 8z"
                            , Svg.Attributes.clipRule "evenodd"
                            ]
                            []
                        ]
                    , Accessibility.inputText
                        searchString
                        [ Accessibility.Aria.controls [ "options" ]
                        , Accessibility.Role.comboBox
                        , Accessibility.Aria.expanded (model_.visibility == Visible && not (List.isEmpty searchResults))
                        , Accessibility.Aria.autoCompleteList
                        , Html.Attributes.autocomplete False
                        , Html.Attributes.attribute "autocorrect" "off"
                        , Html.Attributes.attribute "autocapitalize" "off"
                        , Html.Attributes.spellcheck False
                        , class "h-12 w-full border-0 bg-transparent dark:bg-gray-600 pl-11 pr-4 text-gray-800 dark:text-gray-200 placeholder-gray-400 dark:placeholder-gray-400 focus:ring-0"
                        , Html.Attributes.id <| searchStringFieldId model_.idPrefix
                        , Html.Attributes.placeholder "Search..."
                        , Extras.HtmlAttribute.showMaybe Html.Events.onInput config.onChangeSearchString
                        , Html.Events.preventDefaultOn "keydown"
                            (Extras.HtmlEvents.preventDefaultOnDecoder
                                (\event ->
                                    if event == Extras.HtmlEvents.downArrow then
                                        Just
                                            ( toParentMsg <| MakePreviousOrNextSearchResultActive (List.length searchResults) True
                                            , True
                                            )

                                    else if event == Extras.HtmlEvents.upArrow then
                                        Just
                                            ( toParentMsg <| MakePreviousOrNextSearchResultActive (List.length searchResults) False
                                            , True
                                            )

                                    else if event == Extras.HtmlEvents.home then
                                        Just
                                            ( toParentMsg <| MakeSearchResultActive 0
                                            , True
                                            )

                                    else if event == Extras.HtmlEvents.end then
                                        Just
                                            ( toParentMsg <| MakeSearchResultActive (List.length searchResults - 1)
                                            , True
                                            )

                                    else if event == Extras.HtmlEvents.enter then
                                        Just ( toParentMsg <| LoadUrlForActiveSearchResult searchResults, True )

                                    else
                                        Nothing
                                )
                            )
                        ]
                    ]
                , Extras.Html.showIf (not <| List.isEmpty searchResults) <|
                    ul
                        [ Accessibility.Role.listBox
                        , class "z-30 max-h-72 scroll-py-2 overflow-y-auto py-2 text-gray-800 dark:text-gray-200"
                        , Html.Attributes.id <| searchResultsListId model_.idPrefix
                        ]
                        (searchResults
                            |> List.indexedMap
                                (\index searchResult_ ->
                                    case searchResult_ of
                                        SearchResult { href, body } ->
                                            Html.li
                                                [ class "cursor-default select-none px-4 py-2"
                                                , class "bg-blue-200 dark:bg-blue-700" |> Extras.HtmlAttribute.showIf (Just index == model_.activeSearchResultIndex)
                                                , Accessibility.Role.option
                                                , Accessibility.Key.tabbable False
                                                , Html.Attributes.id <| searchResultId index model_.idPrefix
                                                , Html.Events.onMouseEnter (toParentMsg <| MakeSearchResultActive index)
                                                , Html.Events.onMouseLeave (toParentMsg <| MakeSearchResultInactive index)
                                                ]
                                                [ Html.a
                                                    [ class "hover:no-underline block"
                                                    , class <|
                                                        if Just index == model_.activeSearchResultIndex then
                                                            "text-white dark:text-black"

                                                        else
                                                            "text-black dark:text-white"
                                                    , Accessibility.Key.tabbable False
                                                    , Html.Attributes.href href
                                                    , Extras.HtmlEvents.onClickPreventDefaultAndStopPropagation <|
                                                        toParentMsg <|
                                                            LoadUrl href
                                                    ]
                                                    (List.map (Html.map Basics.never) body)
                                                ]
                                )
                        )
                , Extras.Html.showIf (String.trim searchString /= "" && List.isEmpty searchResults) <|
                    p
                        [ class "p-4 text-gray-500 dark:text-gray-300" ]
                        [ text "No results found." ]
                ]
            ]
        ]
