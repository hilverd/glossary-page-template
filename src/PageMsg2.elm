module PageMsg2 exposing (PageMsg2(..))

import Route exposing (Route)


type PageMsg2 a
    = Navigate Route
    | Internal2 a
