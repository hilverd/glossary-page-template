module Save exposing (save)

import CommonModel exposing (CommonModel)
import Data.Glossary as Glossary exposing (Glossary)
import Extras.HtmlTree as HtmlTree
import Extras.Task
import Http


save : CommonModel -> Glossary -> (Http.Error -> msg) -> msg -> Cmd msg
save common glossary errorMsg msg =
    if common.enableSavingChangesInMemory then
        Extras.Task.messageToCommand msg

    else
        patchHtmlFile glossary errorMsg msg


patchHtmlFile : Glossary -> (Http.Error -> msg) -> msg -> Cmd msg
patchHtmlFile glossary errorMsg msg =
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
                            msg

                        Err error ->
                            errorMsg error
                )
        , timeout = Nothing
        , tracker = Nothing
        }
