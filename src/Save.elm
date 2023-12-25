module Save exposing (save)

import Data.Editability exposing (Editability(..))
import Data.Glossary as Glossary exposing (Glossary)
import Extras.HtmlTree as HtmlTree
import Extras.Task
import Http


save : Editability -> Glossary -> (Http.Error -> msg) -> msg -> Cmd msg
save editability glossary errorMsg msg =
    case editability of
        EditingInMemory ->
            Extras.Task.messageToCommand msg

        _ ->
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
