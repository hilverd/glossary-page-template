module Data.Notification exposing (Notification)

import Html exposing (Html)


type alias Notification =
    { title : Html Never
    , body : Html Never
    }
