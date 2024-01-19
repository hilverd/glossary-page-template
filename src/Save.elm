module Save exposing (changeAndSave)

{-| Functions for saving changes to a glossary.


# Saving

@docs changeAndSave

-}

import Codec
import Data.Editability exposing (Editability(..))
import Data.Glossary as Glossary exposing (Glossary)
import Data.GlossaryChangelist as GlossaryChangelist exposing (GlossaryChangelist)
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
    -> Glossary
    -> GlossaryChangelist
    -> (Http.Error -> msg)
    -> (( Maybe GlossaryItemId, Glossary ) -> msg)
    -> ( Saving, Cmd msg )
changeAndSave editability glossary changelist errorMsg successMsg =
    case Glossary.applyChanges changelist glossary of
        Glossary.ChangesApplied resultOfApplyingChanges ->
            case editability of
                EditingInMemory ->
                    ( NotCurrentlySaving, successMsg resultOfApplyingChanges |> Extras.Task.messageToCommand )

                EditingWithIncludedBackend ->
                    ( SavingInProgress, patchHtmlFile resultOfApplyingChanges errorMsg successMsg )

                EditingWithSeparateBackend { baseUrl, userName, userEmailAddress } ->
                    let
                        changelistWithUserDetails =
                            Maybe.map2
                                (\userName_ userEmailAddress_ ->
                                    GlossaryChangelist.setLastUpdatedBy
                                        { name = userName_, emailAddress = userEmailAddress_ }
                                        changelist
                                )
                                userName
                                userEmailAddress
                                |> Maybe.withDefault changelist
                    in
                    ( SavingInProgress
                    , sendChangesAsPatch
                        baseUrl
                        changelistWithUserDetails
                        resultOfApplyingChanges
                        errorMsg
                        successMsg
                    )

                _ ->
                    ( NotCurrentlySaving, Cmd.none )

        Glossary.VersionsDoNotMatch ->
            ( SavingFailed I18n.otherChangesWereMadePleaseReload, Cmd.none )

        Glossary.LogicalErrorWhenApplyingChanges err ->
            ( SavingNotAttempted err, Cmd.none )


patchHtmlFile :
    ( Maybe GlossaryItemId, Glossary )
    -> (Http.Error -> msg)
    -> (( Maybe GlossaryItemId, Glossary ) -> msg)
    -> Cmd msg
patchHtmlFile ( maybeGlossaryItemId, glossary ) errorMsg successMsg =
    Http.request
        { method = "PATCH"
        , headers = []
        , url = "/"
        , body =
            glossary
                |> Glossary.toHtmlTree
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
    -> GlossaryChangelist
    -> ( Maybe GlossaryItemId, Glossary )
    -> (Http.Error -> msg)
    -> (( Maybe GlossaryItemId, Glossary ) -> msg)
    -> Cmd msg
sendChangesAsPatch baseUrl changes ( maybeGlossaryItemId, glossary ) errorMsg successMsg =
    Http.request
        { method = "PATCH"
        , headers = []
        , url = baseUrl
        , body =
            changes
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
