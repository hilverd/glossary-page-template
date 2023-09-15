module Pages.ManageTags exposing (InternalMsg, Model, Msg, init, subscriptions, update, view)

import Accessibility exposing (Html, div, form, h1, main_, span, text)
import Array exposing (Array)
import Browser exposing (Document)
import CommonModel exposing (CommonModel)
import Components.Button
import Components.Copy
import Data.GlossaryItem.Tag as Tag exposing (Tag)
import Data.Saving exposing (Saving(..))
import Extras.Html
import Html.Attributes exposing (class)
import Html.Events
import Icons
import PageMsg exposing (PageMsg)
import Svg.Attributes
import TagsForm as Form exposing (TagsForm)



-- MODEL


type alias Model =
    { common : CommonModel
    , form : TagsForm
    }


type InternalMsg
    = NoOp
    | AddTag


type alias Msg =
    PageMsg InternalMsg


init : CommonModel -> ( Model, Cmd Msg )
init common =
    case common.glossary of
        Ok { tags } ->
            ( { common = common
              , form = Form.create tags
              }
            , Cmd.none
            )

        _ ->
            ( { common = common
              , form = Form.create []
              }
            , Cmd.none
            )



-- UPDATE


update : InternalMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        AddTag ->
            ( model, Cmd.none )



-- VIEW


viewEditTag : { enableMathSupport : Bool, tabbable : Bool } -> Int -> Tag -> Html Msg
viewEditTag { enableMathSupport, tabbable } _ tag =
    div []
        [ Components.Button.soft
            tabbable
            []
            [ Tag.view enableMathSupport [] tag ]
        ]


viewAddTagButtonForEmptyState : Html Msg
viewAddTagButtonForEmptyState =
    Components.Button.emptyState
        [ Html.Events.onClick <| PageMsg.Internal AddTag
        ]
        [ Icons.plus
            [ Svg.Attributes.class "mx-auto h-12 w-12 text-gray-400" ]
        , span
            [ class "mt-2 block font-medium text-gray-900 dark:text-gray-200" ]
            [ text "Add tag" ]
        ]


viewAddTagButton : Html Msg
viewAddTagButton =
    div []
        [ Components.Button.secondary
            [ Html.Events.onClick <| PageMsg.Internal AddTag
            ]
            [ Icons.plus
                [ Svg.Attributes.class "mx-auto -ml-1 mr-2 h-5 w-5" ]
            , text "Add tag"
            ]
        ]


viewEditTags : { enableMathSupport : Bool, tabbable : Bool } -> Array Tag -> Html Msg
viewEditTags { enableMathSupport, tabbable } tagsArray =
    let
        tags =
            Array.toList tagsArray
    in
    div
        [ class "space-y-6 sm:space-y-5" ]
        [ div
            [ class "mt-6 sm:mt-5 space-y-6 sm:space-y-5" ]
            (List.indexedMap
                (viewEditTag { enableMathSupport = enableMathSupport, tabbable = tabbable })
                tags
            )
        , if List.isEmpty tags then
            viewAddTagButtonForEmptyState

          else
            viewAddTagButton
        ]


viewFooter : Model -> Html Msg
viewFooter model =
    div
        [ class "pt-5 lg:border-t dark:border-gray-700 flex flex-col items-center" ]
        [ Extras.Html.showIf model.common.enableSavingChangesInMemory <|
            div
                [ class "mt-2 mb-2 text-sm text-gray-500 dark:text-gray-400 sm:text-right" ]
                [ text Components.Copy.sandboxModeMessage ]
        , div
            [ class "flex items-center" ]
            [ Components.Button.white
                True
                [ Html.Events.onClick <| PageMsg.NavigateToListAll model.common ]
                [ text "Finished editing" ]
            ]
        ]


view : Model -> Document Msg
view model =
    case model.common.glossary of
        Ok { enableMathSupport } ->
            { title = "Manage Tags"
            , body =
                [ div
                    [ class "container mx-auto px-6 pb-12 lg:px-8 max-w-4xl lg:max-w-screen-2xl" ]
                    [ main_
                        []
                        [ h1
                            [ class "text-3xl font-bold leading-tight text-gray-900 dark:text-gray-100 print:text-black pt-6" ]
                            [ text "Manage Tags"
                            ]
                        , form
                            [ class "pt-7" ]
                            [ div
                                [ class "lg:flex lg:space-x-8" ]
                                [ div
                                    []
                                    [ model.form
                                        |> Form.tags
                                        |> viewEditTags { enableMathSupport = enableMathSupport, tabbable = True }
                                    ]
                                ]
                            , div
                                [ class "mt-4 lg:mt-8" ]
                                [ viewFooter model ]
                            ]
                        ]
                    ]
                ]
            }

        Err _ ->
            { title = "Manage Tags"
            , body = [ text "Something went wrong." ]
            }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
