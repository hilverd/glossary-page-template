module Extras.Http exposing (errorToHumanReadable)

import Http


errorToHumanReadable : Http.Error -> String
errorToHumanReadable error =
    case error of
        Http.BadUrl url ->
            "bad URL: " ++ url

        Http.Timeout ->
            "the request timed out"

        Http.NetworkError ->
            "there was a network error"

        Http.BadStatus statusCode ->
            "unexpected status code " ++ String.fromInt statusCode

        Http.BadBody body ->
            "unexpected response body: " ++ body
