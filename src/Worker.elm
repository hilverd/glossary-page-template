port module Worker exposing (main)

import Codec
import Data.GlossaryChangelist as GlossaryChangelist exposing (GlossaryChangelist)
import Data.GlossaryForUi as GlossaryForUi exposing (GlossaryForUi)
import Data.GlossaryFromDom as GlossaryFromDom
import Extras.HtmlTree as HtmlTree
import Json.Decode as D
import Json.Encode as E
import Platform exposing (worker)


type alias Model =
    ()


type Msg
    = ApplyGlossaryChanges RawApplyGlossaryChangesRequest
    | ConvertGlossaryToHtml RawConvertGlossaryToHtmlRequest


type alias Resolve =
    D.Value


type alias RawApplyGlossaryChangesRequest =
    { glossary : D.Value
    , changelist : D.Value
    , updatedByName : D.Value
    , updatedByEmailAddress : D.Value
    , resolve : D.Value
    }


type alias ApplyGlossaryChangesRequest =
    { glossary : GlossaryForUi
    , changelist : GlossaryChangelist
    , updatedByName : String
    , updatedByEmailAddress : String
    }


type alias RawConvertGlossaryToHtmlRequest =
    { glossary : D.Value
    , resolve : D.Value
    }


type alias ConvertGlossaryToHtmlRequest =
    { glossary : GlossaryForUi
    }


main : Program () Model Msg
main =
    worker
        { init = \() -> ( (), Cmd.none )
        , subscriptions =
            \_ ->
                Sub.batch
                    [ applyGlossaryChanges ApplyGlossaryChanges
                    , convertGlossaryToHtml ConvertGlossaryToHtml
                    ]
        , update = update
        }


port applyGlossaryChanges : (RawApplyGlossaryChangesRequest -> msg) -> Sub msg


port resolveApplyGlossaryChanges : ( Resolve, E.Value ) -> Cmd msg


port convertGlossaryToHtml : (RawConvertGlossaryToHtmlRequest -> msg) -> Sub msg


port resolveConvertGlossaryToHtml : ( Resolve, E.Value ) -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg () =
    case msg of
        ApplyGlossaryChanges rawRequest ->
            case decodeApplyGlossaryChangesRequest rawRequest of
                Ok request ->
                    let
                        changelist : GlossaryChangelist
                        changelist =
                            request.changelist
                                |> GlossaryChangelist.setLastUpdatedBy
                                    { name = request.updatedByName
                                    , emailAddress = request.updatedByEmailAddress
                                    }

                        applyChangesResult : GlossaryForUi.ApplyChangesResult
                        applyChangesResult =
                            GlossaryForUi.applyChanges changelist request.glossary
                    in
                    ( ()
                    , resolveApplyGlossaryChanges
                        ( rawRequest.resolve
                        , encodeApplyChangesResultToValue applyChangesResult
                        )
                    )

                Err err ->
                    ( ()
                    , resolveApplyGlossaryChanges
                        ( rawRequest.resolve
                        , E.object [ ( "errorDecodingRequest", err |> D.errorToString |> E.string ) ]
                        )
                    )

        ConvertGlossaryToHtml rawRequest ->
            case decodeConvertGlossaryToHtmlRequest rawRequest of
                Ok request ->
                    let
                        html : String
                        html =
                            request.glossary
                                |> GlossaryForUi.toGlossaryFromDom
                                |> GlossaryFromDom.toHtmlTree
                                |> HtmlTree.toHtml
                    in
                    ( ()
                    , resolveConvertGlossaryToHtml
                        ( rawRequest.resolve
                        , E.object [ ( "html", E.string html ) ]
                        )
                    )

                Err err ->
                    ( ()
                    , resolveConvertGlossaryToHtml
                        ( rawRequest.resolve
                        , E.object [ ( "errorDecodingRequest", err |> D.errorToString |> E.string ) ]
                        )
                    )


decodeApplyGlossaryChangesRequest : RawApplyGlossaryChangesRequest -> Result D.Error ApplyGlossaryChangesRequest
decodeApplyGlossaryChangesRequest rawRequest =
    Result.map4
        (\glossary changelist updatedByName updatedByEmailAddress ->
            { glossary = glossary
            , changelist = changelist
            , updatedByName = updatedByName
            , updatedByEmailAddress = updatedByEmailAddress
            }
        )
        (Codec.decodeValue GlossaryForUi.codec rawRequest.glossary)
        (Codec.decodeValue GlossaryChangelist.codec rawRequest.changelist)
        (D.decodeValue D.string rawRequest.updatedByName)
        (D.decodeValue D.string rawRequest.updatedByEmailAddress)


encodeApplyChangesResultToValue : GlossaryForUi.ApplyChangesResult -> E.Value
encodeApplyChangesResultToValue result =
    case result of
        GlossaryForUi.ChangesApplied ( _, glossary ) ->
            E.object
                [ ( "newGlossaryJson"
                  , Codec.encodeToValue GlossaryForUi.codec glossary
                  )
                , ( "newGlossaryHtml"
                  , glossary
                        |> GlossaryForUi.toGlossaryFromDom
                        |> GlossaryFromDom.toHtmlTree
                        |> HtmlTree.toHtml
                        |> E.string
                  )
                ]

        GlossaryForUi.VersionsDoNotMatch ->
            E.string "versionsDoNotMatch"

        GlossaryForUi.LogicalErrorWhenApplyingChanges err ->
            E.object [ ( "logicalError", E.string err ) ]


decodeConvertGlossaryToHtmlRequest : RawConvertGlossaryToHtmlRequest -> Result D.Error ConvertGlossaryToHtmlRequest
decodeConvertGlossaryToHtmlRequest rawRequest =
    Result.map
        (\glossary -> { glossary = glossary })
        (Codec.decodeValue GlossaryForUi.codec rawRequest.glossary)
