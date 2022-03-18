module Icons exposing
    ( chevronDownSolid
    , chevronRightSolid
    , exclamationSolidRed
    , pencilSolid
    , plus
    , trashSolid
    , viewGridAddSolid
    )

import Html exposing (Html)
import Svg exposing (path, svg)
import Svg.Attributes exposing (clipRule, d, fill, fillRule, stroke, viewBox)


pencilSolid : Html msg
pencilSolid =
    svg
        [ Svg.Attributes.class "h-5 w-5"
        , viewBox "0 0 20 20"
        , fill "currentColor"
        ]
        [ path
            [ d "M13.586 3.586a2 2 0 112.828 2.828l-.793.793-2.828-2.828.793-.793zM11.379 5.793L3 14.172V17h2.828l8.38-8.379-2.83-2.828z" ]
            []
        ]


trashSolid : Html msg
trashSolid =
    svg
        [ Svg.Attributes.class "h-5 w-5"
        , viewBox "0 0 20 20"
        , fill "currentColor"
        ]
        [ path
            [ d "M9 2a1 1 0 00-.894.553L7.382 4H4a1 1 0 000 2v10a2 2 0 002 2h8a2 2 0 002-2V6a1 1 0 100-2h-3.382l-.724-1.447A1 1 0 0011 2H9zM7 8a1 1 0 012 0v6a1 1 0 11-2 0V8zm5-1a1 1 0 00-1 1v6a1 1 0 102 0V8a1 1 0 00-1-1z"
            , fillRule "evenodd"
            , clipRule "evenodd"
            ]
            []
        ]


exclamationSolidRed : Html msg
exclamationSolidRed =
    svg
        [ Svg.Attributes.class "h-5 w-5 text-red-500 dark:text-red-400"
        , viewBox "0 0 20 20"
        , fill "currentColor"
        ]
        [ path
            [ d "M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-7 4a1 1 0 11-2 0 1 1 0 012 0zm-1-9a1 1 0 00-1 1v4a1 1 0 102 0V6a1 1 0 00-1-1z"
            , fillRule "evenodd"
            , clipRule "evenodd"
            ]
            []
        ]


chevronRightSolid : Html msg
chevronRightSolid =
    svg
        [ Svg.Attributes.class "h-5 w-5 mb-0.5"
        , viewBox "0 0 20 20"
        , fill "currentColor"
        ]
        [ path
            [ d "M7.293 14.707a1 1 0 010-1.414L10.586 10 7.293 6.707a1 1 0 011.414-1.414l4 4a1 1 0 010 1.414l-4 4a1 1 0 01-1.414 0z"
            , fillRule "evenodd"
            , clipRule "evenodd"
            ]
            []
        ]


chevronDownSolid : Html msg
chevronDownSolid =
    svg
        [ Svg.Attributes.class "h-5 w-5 mb-0.5"
        , viewBox "0 0 20 20"
        , fill "currentColor"
        ]
        [ path
            [ d "M5.293 7.293a1 1 0 011.414 0L10 10.586l3.293-3.293a1 1 0 111.414 1.414l-4 4a1 1 0 01-1.414 0l-4-4a1 1 0 010-1.414z"
            , fillRule "evenodd"
            , clipRule "evenodd"
            ]
            []
        ]


viewGridAddSolid : List (Html.Attribute msg) -> Html msg
viewGridAddSolid attributes =
    svg
        ([ Svg.Attributes.class "mx-auto"
         , stroke "currentColor"
         , fill "none"
         , viewBox "0 0 20 20"
         ]
            ++ attributes
        )
        [ path
            [ d "M5 3a2 2 0 00-2 2v2a2 2 0 002 2h2a2 2 0 002-2V5a2 2 0 00-2-2H5zM5 11a2 2 0 00-2 2v2a2 2 0 002 2h2a2 2 0 002-2v-2a2 2 0 00-2-2H5zM11 5a2 2 0 012-2h2a2 2 0 012 2v2a2 2 0 01-2 2h-2a2 2 0 01-2-2V5zM14 11a1 1 0 011 1v1h1a1 1 0 110 2h-1v1a1 1 0 11-2 0v-1h-1a1 1 0 110-2h1v-1a1 1 0 011-1z" ]
            []
        ]


plus : List (Html.Attribute msg) -> Html msg
plus attributes =
    svg
        ([ Svg.Attributes.class "mx-auto"
         , stroke "none"
         , fill "currentColor"
         , viewBox "0 0 20 20"
         ]
            ++ attributes
        )
        [ path
            [ d "M10 3a1 1 0 011 1v5h5a1 1 0 110 2h-5v5a1 1 0 11-2 0v-5H4a1 1 0 110-2h5V4a1 1 0 011-1z" ]
            []
        ]
