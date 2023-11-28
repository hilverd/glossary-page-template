module QueryParameters exposing
    ( QueryParameters
    , fromUrl, setOrderItemsBy
    , orderItemsBy
    , toRelativeUrl
    )

{-| Query parameters that can be present in the URL.


# Query Parameters

@docs QueryParameters


# Build

@docs fromUrl, setOrderItemsBy


# Query

@docs orderItemsBy


# Converting to URLs

@docs toRelativeUrl

-}

import Data.OrderItemsBy as OrderItemsBy exposing (OrderItemsBy)
import Html exposing (param)
import Url exposing (Url)
import Url.Builder exposing (QueryParameter)
import Url.Parser
import Url.Parser.Query


{-| A set of query parameters.
-}
type QueryParameters
    = QueryParameters
        { orderItemsBy : OrderItemsBy
        }


create : OrderItemsBy -> QueryParameters
create orderItemsBy_ =
    QueryParameters { orderItemsBy = orderItemsBy_ }


default : QueryParameters
default =
    QueryParameters { orderItemsBy = OrderItemsBy.Alphabetically }


query : Url.Parser.Query.Parser QueryParameters
query =
    Url.Parser.Query.map create OrderItemsBy.fromQuery


parser : Url.Parser.Parser (QueryParameters -> a) a
parser =
    Url.Parser.query query


{-| Parse the current URL to obtain a set of query parameters.
-}
fromUrl : Url -> QueryParameters
fromUrl url =
    { url | path = "" }
        |> Url.Parser.parse parser
        |> Maybe.withDefault default


{-| Change the way items are ordered.
-}
setOrderItemsBy : OrderItemsBy -> QueryParameters -> QueryParameters
setOrderItemsBy orderItemsBy_ queryParameters =
    case queryParameters of
        QueryParameters parameters ->
            QueryParameters { parameters | orderItemsBy = orderItemsBy_ }


{-| The way items are ordered.
-}
orderItemsBy : QueryParameters -> OrderItemsBy
orderItemsBy queryParameters =
    case queryParameters of
        QueryParameters parameters ->
            parameters.orderItemsBy


{-| Convert a list of query parameters to a relative URL.
-}
toRelativeUrl : QueryParameters -> String
toRelativeUrl queryParameters =
    case queryParameters of
        QueryParameters parameters ->
            [ OrderItemsBy.toQueryParameter parameters.orderItemsBy ]
                |> List.filterMap identity
                |> Url.Builder.relative []
