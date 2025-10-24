module Extras.Maybe exposing (orElse)

{-| This module provides additional convenience functions for [`Maybe`](https://package.elm-lang.org/packages/elm/core/latest/Maybe).
Adapted from <https://github.com/elm-community/maybe-extra>.


# Or

@docs orElse

-}


{-| Piping-friendly version of [`or`](#or).

    Just 5
        |> orElse (Just 4)
    --> Just 5

    orElse (Just 4) (Just 5)
    --> Just 5

    List.head []
        |> orElse (List.head [ 4 ])
    --> Just 4

-}
orElse : Maybe a -> Maybe a -> Maybe a
orElse ma mb =
    case mb of
        Nothing ->
            ma

        Just _ ->
            mb
