module PageMsg exposing (PageMsg(..))

import CommonModel exposing (CommonModel)


type PageMsg a
    = NavigateToListAll CommonModel
    | NavigateToCreateOrEdit CommonModel
    | Internal a
