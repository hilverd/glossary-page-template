module DirectedGraph exposing (DirectedGraph, empty, insertVertex, insertEdge, vertices, outNeighbours, verticesByDistance)

{-| A directed graph.


# Directed Graphs

@docs DirectedGraph, empty, insertVertex, insertEdge, vertices, outNeighbours, verticesByDistance

-}

import AmortizedQueue exposing (AmortizedQueue)
import Dict exposing (Dict)
import Set exposing (Set)


{-| An opaque type representing a directed graph.
-}
type DirectedGraph a
    = DirectedGraph
        { vertexToString : a -> String
        , vertexFromString : String -> a
        , vertices_ : Set String
        , edges_ : Dict String (Set String)
        }


{-| An empty graph.
-}
empty : (a -> String) -> (String -> a) -> DirectedGraph a
empty vertexToString vertexFromString =
    DirectedGraph
        { vertexToString = vertexToString
        , vertexFromString = vertexFromString
        , vertices_ = Set.empty
        , edges_ = Dict.empty
        }


{-| Insert a vertex.
-}
insertVertex : a -> DirectedGraph a -> DirectedGraph a
insertVertex vertex graph =
    case graph of
        DirectedGraph { vertexToString, vertexFromString, vertices_, edges_ } ->
            let
                vertexAsString =
                    vertexToString vertex
            in
            DirectedGraph
                { vertexToString = vertexToString
                , vertexFromString = vertexFromString
                , vertices_ = Set.insert vertexAsString vertices_
                , edges_ = edges_
                }


{-| Insert an edge.
If one or both of the vertices that make up the edge are not in the graph, they are added.
-}
insertEdge : a -> a -> DirectedGraph a -> DirectedGraph a
insertEdge vertex1 vertex2 graph =
    let
        graph1 =
            graph
                |> insertVertex vertex1
                |> insertVertex vertex2
    in
    case graph1 of
        DirectedGraph { vertexToString, vertexFromString, vertices_, edges_ } ->
            let
                vertex1AsString =
                    vertexToString vertex1

                vertex2AsString =
                    vertexToString vertex2
            in
            DirectedGraph
                { vertexToString = vertexToString
                , vertexFromString = vertexFromString
                , vertices_ = vertices_
                , edges_ =
                    edges_
                        |> Dict.update vertex1AsString
                            (\existingOutNeighbours ->
                                existingOutNeighbours
                                    |> Maybe.withDefault Set.empty
                                    |> Set.insert vertex2AsString
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
vertices : DirectedGraph a -> List a
vertices graph =
    case graph of
        DirectedGraph { vertexFromString, vertices_ } ->
            vertices_
                |> Set.toList
                |> List.map vertexFromString


{-| Return the list of out-neighbours of a vertex.

    empty identity identity
    |> insertEdge "R" "A"
    |> insertEdge "A" "B"
    |> insertEdge "A" "C"
    |> outNeighbours "A"
    --> ["B", "C"]

    empty identity identity
    |> outNeighbours "A"
    --> []

-}
outNeighbours : a -> DirectedGraph a -> List a
outNeighbours vertex graph =
    case graph of
        DirectedGraph { vertexToString, vertexFromString, edges_ } ->
            edges_
                |> Dict.get (vertexToString vertex)
                |> Maybe.withDefault Set.empty
                |> Set.toList
                |> List.map vertexFromString


{-| Given a vertex `v` which is assumed to be in the graph, list the vertices in the graph in an order such that for any distinct vertices `v1`, `v2` in the graph, if `v1` can be reached from `v` using a shorter path than `v2`, then `v1` appears before `v2` in the list.
If `v1` and `v2` have the same distance to `v` then the vertex that is smaller according to `comparable` order appears first.
The result is separated into two lists --- the first contains items vertices reachable from `v` the second contains those that are not.

    empty identity identity
    |> insertEdge "R" "A"
    |> insertEdge "A" "B"
    |> insertEdge "B" "R"
    |> insertEdge "R" "C"
    |> insertEdge "D" "C"
    |> insertEdge "E" "C"
    |> insertVertex "F"
    |> verticesByDistance "R"
    --> (["R", "A", "C", "B"], ["D", "E", "F"])

-}
verticesByDistance : a -> DirectedGraph a -> ( List a, List a )
verticesByDistance startingVertex graph =
    case graph of
        DirectedGraph { vertexToString, vertexFromString, vertices_ } ->
            let
                f : AmortizedQueue ( a, Int ) -> Dict String Int -> Dict String Int
                f queue distances =
                    -- Breadth-first search
                    case AmortizedQueue.dequeue queue of
                        Just ( ( vertex, distance ), tail ) ->
                            let
                                ( queue1, distances1 ) =
                                    outNeighbours vertex graph
                                        |> List.foldl
                                            (\outNeighbour ( queue0, distances0 ) ->
                                                let
                                                    outNeighbourAsString =
                                                        vertexToString outNeighbour
                                                in
                                                case Dict.get outNeighbourAsString distances0 of
                                                    Nothing ->
                                                        ( queue0
                                                            |> AmortizedQueue.enqueue ( outNeighbour, distance + 1 )
                                                        , distances0
                                                            |> Dict.insert outNeighbourAsString (distance + 1)
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
            ( reachableVertices, unreachableVertices )
