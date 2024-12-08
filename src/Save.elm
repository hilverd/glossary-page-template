module Save exposing (changeAndSave)

{-| Functions for saving changes to a glossary.


# Saving

@docs changeAndSave

-}

import Codec
import Data.Editability exposing (Editability(..))
import Data.GlossaryChangelist as GlossaryChangelist exposing (GlossaryChangelist)
import Data.GlossaryForUi as GlossaryForUi exposing (GlossaryForUi)
import Data.GlossaryFromDom as GlossaryFromDom
import Data.GlossaryItemId exposing (GlossaryItemId)
import Data.Saving exposing (Saving(..))
import Extras.HtmlTree as HtmlTree
import Extras.Task
import Http
import Internationalisation as I18n


{-| Apply changes to a glossary and save the result.
-}
changeAndSave :
    Editability
    -> GlossaryForUi
    -> GlossaryChangelist
    -> (Http.Error -> msg)
    -> (( Maybe GlossaryItemId, GlossaryForUi ) -> msg)
    -> ( Saving, Cmd msg )
changeAndSave editability glossary changelist_ errorMsg successMsg =
    let
        changelist : GlossaryChangelist
        changelist =
            case editability of
                EditingWithSeparateBackend { userName, userEmailAddress } ->
                    Maybe.map2
                        (\userName_ userEmailAddress_ ->
                            GlossaryChangelist.setLastUpdatedBy
                                { name = userName_, emailAddress = userEmailAddress_ }
                                changelist_
                        )
                        userName
                        userEmailAddress
                        |> Maybe.withDefault changelist_

                _ ->
                    changelist_

        resultOfApplyingChanges : GlossaryFromDom.ApplyChangesResult
        resultOfApplyingChanges =
            glossary
                |> GlossaryForUi.toGlossaryFromDom
                |> GlossaryFromDom.applyChanges changelist
    in
    case resultOfApplyingChanges of
        GlossaryFromDom.ChangesApplied result ->
            let
                result_ : ( Maybe GlossaryItemId, GlossaryForUi )
                result_ =
                    Tuple.mapSecond GlossaryForUi.fromGlossaryFromDom result
            in
            case editability of
                EditingInMemory ->
                    ( NotCurrentlySaving, successMsg result_ |> Extras.Task.messageToCommand )

                EditingWithIncludedBackend ->
                    ( SavingInProgress, patchHtmlFile result_ errorMsg successMsg )

                EditingWithSeparateBackend { baseUrl, bearerToken } ->
                    ( SavingInProgress
                    , sendChangesAsPatch
                        baseUrl
                        bearerToken
                        changelist
                        result_
                        errorMsg
                        successMsg
                    )

                _ ->
                    ( NotCurrentlySaving, Cmd.none )

        GlossaryFromDom.VersionsDoNotMatch ->
            ( SavingFailed I18n.otherChangesWereMadePleaseReload, Cmd.none )

        GlossaryFromDom.LogicalErrorWhenApplyingChanges err ->
            ( SavingNotAttempted err, Cmd.none )


patchHtmlFile :
    ( Maybe GlossaryItemId, GlossaryForUi )
    -> (Http.Error -> msg)
    -> (( Maybe GlossaryItemId, GlossaryForUi ) -> msg)
    -> Cmd msg
patchHtmlFile ( maybeGlossaryItemId, glossary ) errorMsg successMsg =
    Http.request
        { method = "PATCH"
        , headers = []
        , url = "/"
        , body =
            glossary
                |> GlossaryForUi.toGlossaryFromDom
                |> GlossaryFromDom.toHtmlTree
                |> HtmlTree.toHtmlReplacementString
                |> Http.stringBody "text/html"
        , expect =
            Http.expectWhatever
                (\result ->
                    case result of
                        Ok _ ->
                            successMsg ( maybeGlossaryItemId, glossary )

                        Err error ->
                            errorMsg error
                )
        , timeout = Nothing
        , tracker = Nothing
        }


sendChangesAsPatch :
    String
    -> Maybe String
    -> GlossaryChangelist
    -> ( Maybe GlossaryItemId, GlossaryForUi )
    -> (Http.Error -> msg)
    -> (( Maybe GlossaryItemId, GlossaryForUi ) -> msg)
    -> Cmd msg
sendChangesAsPatch baseUrl bearerToken changelist ( maybeGlossaryItemId, glossary ) errorMsg successMsg =
    let
        authorisationHeader : Maybe Http.Header
        authorisationHeader =
            bearerToken |> Maybe.map (\token -> Http.header "Authorization" ("Bearer " ++ token))

        headers : List Http.Header
        headers =
            [ authorisationHeader ]
                |> List.filterMap identity
    in
    Http.request
        { method = "PATCH"
        , headers = headers
        , url = baseUrl
        , body =
            changelist
                |> Codec.encodeToValue GlossaryChangelist.codec
                |> Http.jsonBody
        , expect =
            Http.expectWhatever
                (\result ->
                    case result of
                        Ok _ ->
                            successMsg ( maybeGlossaryItemId, glossary )

                        Err error ->
                            errorMsg error
                )
        , timeout = Nothing
        , tracker = Nothing
        }
