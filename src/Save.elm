module Save exposing (changeAndSave)

{-| Functions for saving changes to a glossary.


# Saving

@docs changeAndSave

-}

import Codec
import Data.Editability exposing (Editability(..))
import Data.Glossary as Glossary exposing (Glossary)
import Data.GlossaryChanges as GlossaryChanges exposing (GlossaryChanges)
import Data.GlossaryItemId exposing (GlossaryItemId)
import Data.Saving exposing (Saving(..))
import Extras.HtmlTree as HtmlTree
import Extras.Task
import Http


{-| Apply changes to a glossary and save the result.
-}
changeAndSave :
    Editability
    -> Glossary
    -> GlossaryChanges
    -> (Http.Error -> msg)
    -> (( Maybe GlossaryItemId, Glossary ) -> msg)
    -> ( Saving, Cmd msg )
changeAndSave editability glossary changes errorMsg successMsg =
    case Glossary.applyChanges changes glossary of
        Ok resultOfApplyingChanges ->
            case editability of
                EditingInMemory ->
                    ( NotCurrentlySaving, successMsg resultOfApplyingChanges |> Extras.Task.messageToCommand )

                EditingWithIncludedBackend ->
                    ( SavingInProgress, patchHtmlFile resultOfApplyingChanges errorMsg successMsg )

                EditingWithSeparateBackend baseUrl ->
                    ( SavingInProgress, sendChangesAsPatch baseUrl changes resultOfApplyingChanges errorMsg successMsg )

                _ ->
                    ( NotCurrentlySaving, Cmd.none )

        Err err ->
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
    -> GlossaryChanges
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
                |> Codec.encodeToValue GlossaryChanges.codec
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
