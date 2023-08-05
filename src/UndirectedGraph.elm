module UndirectedGraph exposing (UndirectedGraph, empty, insertVertex, insertEdge, vertices, adjacentVertices, verticesByDistance)

{-| An undirected graph.


# Undirected Graphs

@docs UndirectedGraph, empty, insertVertex, insertEdge, vertices, adjacentVertices, verticesByDistance

-}

import AmortizedQueue exposing (AmortizedQueue)
import Dict exposing (Dict)
import Set exposing (Set)


{-| An opaque type representing an undirected graph.
-}
type UndirectedGraph a
    = UndirectedGraph
        { vertexToString : a -> String
        , vertexFromString : String -> a
        , vertices_ : Set String
        , edges_ : Dict String (Set String)
        }


{-| An empty graph.
-}
empty : (a -> String) -> (String -> a) -> UndirectedGraph a
empty vertexToString vertexFromString =
    UndirectedGraph
        { vertexToString = vertexToString
        , vertexFromString = vertexFromString
        , vertices_ = Set.empty
        , edges_ = Dict.empty
        }


{-| Insert a vertex.
-}
insertVertex : a -> UndirectedGraph a -> UndirectedGraph a
insertVertex vertex graph =
    case graph of
        UndirectedGraph { vertexToString, vertexFromString, vertices_, edges_ } ->
            let
                vertexAsString =
                    vertexToString vertex
            in
            UndirectedGraph
                { vertexToString = vertexToString
                , vertexFromString = vertexFromString
                , vertices_ = Set.insert vertexAsString vertices_
                , edges_ = edges_
                }


{-| Insert an edge.
If one or both of the vertices that make up the edge are not in the graph, they are added.
-}
insertEdge : a -> a -> UndirectedGraph a -> UndirectedGraph a
insertEdge vertex1 vertex2 graph =
    let
        graph1 =
            graph
                |> insertVertex vertex1
                |> insertVertex vertex2
    in
    case graph1 of
        UndirectedGraph { vertexToString, vertexFromString, vertices_, edges_ } ->
            let
                vertex1AsString =
                    vertexToString vertex1

                vertex2AsString =
                    vertexToString vertex2
            in
            UndirectedGraph
                { vertexToString = vertexToString
                , vertexFromString = vertexFromString
                , vertices_ = vertices_
                , edges_ =
                    edges_
                        |> Dict.update vertex1AsString
                            (\existingNeighbours ->
                                existingNeighbours
                                    |> Maybe.withDefault Set.empty
                                    |> Set.insert vertex2AsString
                                    |> Just
                            )
                        |> Dict.update vertex2AsString
                            (\existingNeighbours ->
                                existingNeighbours
                                    |> Maybe.withDefault Set.empty
                                    |> Set.insert vertex1AsString
                                    |> Just
                            )
                }


{-| Return the list of vertices.

    empty identity identity
    |> insertEdge "R" "A"
    |> insertVertex "B"
    |> vertices
    --> ["A", "B", "R"]

-}
vertices : UndirectedGraph a -> List a
vertices graph =
    case graph of
        UndirectedGraph { vertexFromString, vertices_ } ->
            vertices_
                |> Set.toList
                |> List.map vertexFromString


{-| Return the list of vertices adjacent to a vertex.

    empty identity identity
    |> insertEdge "R" "A"
    |> insertEdge "A" "B"
    |> adjacentVertices "A"
    --> ["B", "R"]

    empty identity identity
    |> adjacentVertices "A"
    --> []

-}
adjacentVertices : a -> UndirectedGraph a -> List a
adjacentVertices vertex graph =
    case graph of
        UndirectedGraph { vertexToString, vertexFromString, edges_ } ->
            edges_
                |> Dict.get (vertexToString vertex)
                |> Maybe.withDefault Set.empty
                |> Set.toList
                |> List.map vertexFromString


{-| Given a vertex `v` which is assumed to be in the graph, list the vertices in the graph in an order such that for any distinct vertices `v1`, `v2` in the graph, if `v1` can be reached from `v` using a shorter path than `v2`, then `v1` appears before `v2` in the list.
If `v1` and `v2` have the same distance to `v` then the vertex that is smaller according to `comparable` order appears first.

    empty identity identity
    |> insertEdge "R" "A"
    |> insertEdge "B" "A"
    |> insertEdge "B" "R"
    |> insertEdge "R" "C"
    |> insertEdge "C" "D"
    |> insertEdge "C" "E"
    |> insertVertex "F"
    |> verticesByDistance "R"
    --> ["R", "A", "B", "C", "D", "E", "F"]

-}
verticesByDistance : a -> UndirectedGraph a -> List a
verticesByDistance startingVertex graph =
    case graph of
        UndirectedGraph { vertexToString, vertexFromString, vertices_ } ->
            let
                f : AmortizedQueue ( a, Int ) -> Dict String Int -> Dict String Int
                f queue distances =
                    -- Breadth-first search
                    case AmortizedQueue.dequeue queue of
                        Just ( ( vertex, distance ), tail ) ->
                            let
                                ( queue1, distances1 ) =
                                    adjacentVertices vertex graph
                                        |> List.foldl
                                            (\adjacentVertex ( queue0, distances0 ) ->
                                                let
                                                    adjacentVertexAsString =
                                                        vertexToString adjacentVertex
                                                in
                                                case Dict.get adjacentVertexAsString distances0 of
                                                    Nothing ->
                                                        ( queue0
                                                            |> AmortizedQueue.enqueue ( adjacentVertex, distance + 1 )
                                                        , distances0
                                                            |> Dict.insert adjacentVertexAsString (distance + 1)
                                                        )

                                                    _ ->
                                                        ( queue0, distances0 )
                                            )
                                            ( tail, distances )
                            in
                            f queue1 distances1

                        _ ->
                            distances

                distancesFromStartingVertex : Dict String Int
                distancesFromStartingVertex =
                    f
                        (AmortizedQueue.enqueue ( startingVertex, 0 ) AmortizedQueue.empty)
                        (Dict.singleton (vertexToString startingVertex) 0)

                reachableVertices : List a
                reachableVertices =
                    distancesFromStartingVertex
                        |> Dict.toList
                        |> List.sortBy Tuple.second
                        |> List.map (Tuple.first >> vertexFromString)

                unreachableVertices : List a
                unreachableVertices =
                    reachableVertices
                        |> List.map vertexToString
                        |> Set.fromList
                        |> Set.diff vertices_
                        |> Set.toList
                        |> List.map vertexFromString
            in
            List.append reachableVertices unreachableVertices
