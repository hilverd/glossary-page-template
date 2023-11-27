module QueryParameters exposing
    ( QueryParameters
    , fromUrl
    )

{-| Query parameters that can be present in the URL.


# Query Parameters

@docs QueryParameters


# Build

@docs fromUrl

-}

import Data.OrderItemsBy as OrderItemsBy exposing (OrderItemsBy)
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
create orderItemsBy =
    QueryParameters { orderItemsBy = orderItemsBy }


default : QueryParameters
default =
    QueryParameters { orderItemsBy = OrderItemsBy.Alphabetically }


query : Url.Parser.Query.Parser QueryParameters
query =
    Url.Parser.Query.map create OrderItemsBy.decodeQuery


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
