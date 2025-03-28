module Extras.BrowserDom exposing (scrollElementIntoView, scrollToBottom, scrollToTop, scrollToTopInElement)

import Browser.Dom as Dom
import Task


scrollToTop : msg -> Cmd msg
scrollToTop noOpMsg =
    Dom.setViewport 0 0
        |> Task.onError (always <| Task.succeed ())
        |> Task.attempt (always noOpMsg)


scrollElementIntoView : msg -> String -> Cmd msg
scrollElementIntoView noOpMsg id =
    id
        |> Dom.getElement
        |> Task.andThen (\element -> Dom.setViewport 0 <| element.element.y - 96)
        |> Task.onError (always <| Task.succeed ())
        |> Task.attempt (always noOpMsg)


scrollToTopInElement : msg -> String -> Cmd msg
scrollToTopInElement noOpMsg id =
    id
        |> Dom.getViewportOf
        |> Task.andThen (always <| Dom.setViewportOf id 0 0)
        |> Task.attempt (always noOpMsg)


scrollToBottom : msg -> Cmd msg
scrollToBottom noOpMsg =
    Dom.getViewport
        |> Task.andThen (\info -> Dom.setViewport 0 info.scene.height)
        |> Task.onError (always <| Task.succeed ())
        |> Task.attempt (always noOpMsg)
