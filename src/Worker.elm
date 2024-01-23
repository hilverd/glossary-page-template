port module Worker exposing (main)

import Codec
import Data.Glossary as Glossary exposing (Glossary)
import Data.GlossaryChangelist as GlossaryChangelist exposing (GlossaryChangelist)
import Extras.HtmlTree as HtmlTree
import Json.Decode as D
import Json.Encode as E
import Platform exposing (worker)


type alias Model =
    Int


type Msg
    = ApplyGlossaryChanges RawApplyGlossaryChangesRequest


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
    { glossary : Glossary
    , changelist : GlossaryChangelist
    , updatedByName : String
    , updatedByEmailAddress : String
    }


main : Program () Model Msg
main =
    worker
        { init = \() -> ( 0, Cmd.none )
        , subscriptions = \_ -> applyGlossaryChanges ApplyGlossaryChanges
        , update = update
        }


port applyGlossaryChanges : (RawApplyGlossaryChangesRequest -> msg) -> Sub msg


port resolveApplyGlossaryChanges : ( Resolve, E.Value ) -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg count =
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

                        applyChangesResult : Glossary.ApplyChangesResult
                        applyChangesResult =
                            Glossary.applyChanges changelist request.glossary
                    in
                    ( count
                    , resolveApplyGlossaryChanges
                        ( rawRequest.resolve
                        , encodeApplyChangesResultToValue applyChangesResult
                        )
                    )

                Err _ ->
                    ( count
                    , resolveApplyGlossaryChanges
                        ( rawRequest.resolve
                        , E.int 500
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
        (Codec.decodeValue Glossary.codec rawRequest.glossary)
        (Codec.decodeValue GlossaryChangelist.codec rawRequest.changelist)
        (D.decodeValue D.string rawRequest.updatedByName)
        (D.decodeValue D.string rawRequest.updatedByEmailAddress)


encodeApplyChangesResultToValue : Glossary.ApplyChangesResult -> E.Value
encodeApplyChangesResultToValue result =
    case result of
        Glossary.ChangesApplied ( _, glossary ) ->
            E.object
                [ ( "newGlossaryJson"
                  , Codec.encodeToValue Glossary.codec glossary
                  )
                , ( "newGlossaryHtml"
                  , glossary
                        |> Glossary.toHtmlTree
                        |> HtmlTree.toHtml
                        |> E.string
                  )
                ]

        Glossary.VersionsDoNotMatch ->
            E.string "VersionsDoNotMatch"

        Glossary.LogicalErrorWhenApplyingChanges err ->
            E.string ("LogicalErrorWhenApplyingChanges: " ++ err)
