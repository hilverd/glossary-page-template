module QueryParameters exposing
    ( QueryParameters
    , fromUrl, setOrderItemsBy, setFilterByTag
    , orderItemsBy, filterByTag
    , toRelativeUrl
    )

{-| Query parameters that can be present in the URL.


# Query Parameters

@docs QueryParameters


# Build

@docs fromUrl, setOrderItemsBy, setFilterByTag


# Query

@docs orderItemsBy, filterByTag


# Converting to URLs

@docs toRelativeUrl

-}

import Data.GlossaryItem.Tag as Tag exposing (Tag)
import Data.OrderItemsBy as OrderItemsBy exposing (OrderItemsBy)
import Url exposing (Url)
import Url.Builder
import Url.Parser
import Url.Parser.Query


{-| A set of query parameters.
-}
type QueryParameters
    = QueryParameters
        { orderItemsBy : OrderItemsBy
        , filterByTag : Maybe Tag
        }


create : OrderItemsBy -> Maybe Tag -> QueryParameters
create orderItemsBy_ filterByTag_ =
    QueryParameters
        { orderItemsBy = orderItemsBy_
        , filterByTag = filterByTag_
        }


default : QueryParameters
default =
    QueryParameters
        { orderItemsBy = OrderItemsBy.Alphabetically
        , filterByTag = Nothing
        }


query : Url.Parser.Query.Parser QueryParameters
query =
    Url.Parser.Query.map2 create
        OrderItemsBy.fromQuery
        Tag.fromQuery


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


{-| Change the tag being filtered by.
-}
setFilterByTag : Maybe Tag -> QueryParameters -> QueryParameters
setFilterByTag filterByTag_ queryParameters =
    case queryParameters of
        QueryParameters parameters ->
            QueryParameters { parameters | filterByTag = filterByTag_ }


{-| The way items are ordered.
-}
orderItemsBy : QueryParameters -> OrderItemsBy
orderItemsBy queryParameters =
    case queryParameters of
        QueryParameters parameters ->
            parameters.orderItemsBy


{-| The tag being filtered by.
-}
filterByTag : QueryParameters -> Maybe Tag
filterByTag queryParameters =
    case queryParameters of
        QueryParameters parameters ->
            parameters.filterByTag


{-| Convert a set of query parameters to a relative URL.
-}
toRelativeUrl : QueryParameters -> String
toRelativeUrl queryParameters =
    case queryParameters of
        QueryParameters parameters ->
            [ OrderItemsBy.toQueryParameter parameters.orderItemsBy
            , Maybe.map Tag.toQueryParameter parameters.filterByTag
            ]
                |> List.filterMap identity
                |> Url.Builder.relative []
