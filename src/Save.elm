module Save exposing (patchHtmlFile, patchHtmlFileSimpler)

import CommonModel exposing (CommonModel)
import Data.Glossary as Glossary exposing (Glossary)
import Data.GlossaryItems exposing (GlossaryItems)
import Extras.HtmlTree as HtmlTree
import Extras.Task
import Http


patchHtmlFile : CommonModel -> GlossaryItems -> (Http.Error -> msg) -> msg -> Cmd msg
patchHtmlFile common glossaryItems errorMsg msg =
    if common.enableSavingChangesInMemory then
        Extras.Task.messageToCommand msg

    else
        case common.glossary of
            Ok glossary0 ->
                let
                    glossary : Glossary
                    glossary =
                        { glossary0 | items = glossaryItems }
                in
                Http.request
                    { method = "PATCH"
                    , headers = []
                    , url = "/"
                    , body =
                        glossary
                            |> Glossary.toHtmlTree common.enableExportMenu common.enableOrderItemsButtons common.enableHelpForMakingChanges
                            |> HtmlTree.toHtmlReplacementString
                            |> Http.stringBody "text/html"
                    , expect =
                        Http.expectWhatever
                            (\result ->
                                case result of
                                    Ok _ ->
                                        msg

                                    Err error ->
                                        errorMsg error
                            )
                    , timeout = Nothing
                    , tracker = Nothing
                    }

            _ ->
                Cmd.none


patchHtmlFileSimpler : CommonModel -> Glossary -> (Http.Error -> msg) -> msg -> Cmd msg
patchHtmlFileSimpler common glossary errorMsg msg =
    if common.enableSavingChangesInMemory then
        Extras.Task.messageToCommand msg

    else
        Http.request
            { method = "PATCH"
            , headers = []
            , url = "/"
            , body =
                glossary
                    |> Glossary.toHtmlTree common.enableExportMenu common.enableOrderItemsButtons common.enableHelpForMakingChanges
                    |> HtmlTree.toHtmlReplacementString
                    |> Http.stringBody "text/html"
            , expect =
                Http.expectWhatever
                    (\result ->
                        case result of
                            Ok _ ->
                                msg

                            Err error ->
                                errorMsg error
                    )
            , timeout = Nothing
            , tracker = Nothing
            }
