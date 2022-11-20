module Extras.Url exposing (fragmentOnly)

{-| This module provides additional functions for building URL strings.


# Builders

@docs fragmentOnly

-}

import Url.Builder


{-| Build a relative URL string containing only a fragment identifier.

    fragmentOnly "foo" --> "#foo"

    fragmentOnly "" --> "#"

-}
fragmentOnly : String -> String
fragmentOnly identifier =
    Url.Builder.custom Url.Builder.Relative [] [] <| Just identifier
