module Save exposing (patchHtmlFile)

import CommonModel exposing (CommonModel)
import Data.Glossary as Glossary exposing (Glossary)
import Extras.HtmlTree as HtmlTree
import Extras.Task
import Http


patchHtmlFile : CommonModel -> Glossary -> (Http.Error -> msg) -> msg -> Cmd msg
patchHtmlFile common glossary errorMsg msg =
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
