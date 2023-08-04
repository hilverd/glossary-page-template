module AmortizedQueue exposing (AmortizedQueue, empty, enqueue, dequeue, toList)

{-| An amortized queue.
Inserting an element at the back of the queue takes O(1) time.
Removing an element from the front takes O(1) time on average.


# Amortized Queues

@docs AmortizedQueue, empty, enqueue, dequeue, toList

-}


{-| An opaque type representing an amortized queue.
-}
type AmortizedQueue a
    = AmortizedQueue
        { front : List a
        , rearReversed : List a
        }


{-| An empty queue.
-}
empty : AmortizedQueue a
empty =
    AmortizedQueue { front = [], rearReversed = [] }


{-| Add an element to the back of the queue.
This takes O(1) time.

    empty
    |> enqueue "a"
    |> enqueue "b"
    |> dequeue
    |> Maybe.map Tuple.first
    --> Just "a"

-}
enqueue : a -> AmortizedQueue a -> AmortizedQueue a
enqueue element queue =
    case queue of
        AmortizedQueue { front, rearReversed } ->
            AmortizedQueue { front = element :: front, rearReversed = rearReversed }


{-| Remove the element at the head of the queue, if there is one.
This takes O(1) amortized time.

    empty |> dequeue --> Nothing

    empty
    |> enqueue "a"
    |> enqueue "b"
    |> enqueue "c"
    |> dequeue
    |> Maybe.map Tuple.second
    |> Maybe.map (enqueue "d")
    |> Maybe.map toList
    --> Just ["b", "c", "d"]

-}
dequeue : AmortizedQueue a -> Maybe ( a, AmortizedQueue a )
dequeue queue =
    case queue of
        AmortizedQueue { front, rearReversed } ->
            case ( front, rearReversed ) of
                ( [], [] ) ->
                    Nothing

                ( _, head :: tail ) ->
                    Just ( head, AmortizedQueue { front = front, rearReversed = tail } )

                ( _, [] ) ->
                    AmortizedQueue { front = [], rearReversed = List.reverse front }
                        |> dequeue


{-| Return the queue represented as a list where the first element is the head of the queue.

    empty
    |> enqueue "a"
    |> enqueue "b"
    |> toList
    --> ["a", "b"]

-}
toList : AmortizedQueue a -> List a
toList queue =
    case queue of
        AmortizedQueue { front, rearReversed } ->
            List.append rearReversed (List.reverse front)
