module Icons exposing
    ( chevronDown
    , chevronRight
    , exclamation
    , exclamationCircle
    , menu
    , pencil
    , plus
    , search
    , trash
    , viewGridAdd
    , x
    )

import Html exposing (Html)
import Svg exposing (circle, path, svg)
import Svg.Attributes exposing (clipRule, cx, cy, d, fill, fillRule, r, stroke, strokeLinecap, strokeLinejoin, strokeWidth, viewBox)


withAdditionalAttributes :
    List (Html.Attribute msg)
    -> List (Html msg)
    -> List (Html.Attribute msg)
    -> Html msg
withAdditionalAttributes attributes children additionalAttributes =
    svg (attributes ++ additionalAttributes) children


pencil : List (Html.Attribute msg) -> Html msg
pencil =
    withAdditionalAttributes
        [ viewBox "0 0 20 20"
        , fill "currentColor"
        ]
        [ path
            [ d "M13.586 3.586a2 2 0 112.828 2.828l-.793.793-2.828-2.828.793-.793zM11.379 5.793L3 14.172V17h2.828l8.38-8.379-2.83-2.828z" ]
            []
        ]


trash : List (Html.Attribute msg) -> Html msg
trash =
    withAdditionalAttributes
        [ viewBox "0 0 20 20"
        , fill "currentColor"
        ]
        [ path
            [ d "M9 2a1 1 0 00-.894.553L7.382 4H4a1 1 0 000 2v10a2 2 0 002 2h8a2 2 0 002-2V6a1 1 0 100-2h-3.382l-.724-1.447A1 1 0 0011 2H9zM7 8a1 1 0 012 0v6a1 1 0 11-2 0V8zm5-1a1 1 0 00-1 1v6a1 1 0 102 0V8a1 1 0 00-1-1z"
            , fillRule "evenodd"
            , clipRule "evenodd"
            ]
            []
        ]


exclamationCircle : List (Html.Attribute msg) -> Html msg
exclamationCircle =
    withAdditionalAttributes
        [ viewBox "0 0 20 20"
        , fill "currentColor"
        ]
        [ path
            [ d "M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-7 4a1 1 0 11-2 0 1 1 0 012 0zm-1-9a1 1 0 00-1 1v4a1 1 0 102 0V6a1 1 0 00-1-1z"
            , fillRule "evenodd"
            , clipRule "evenodd"
            ]
            []
        ]


chevronRight : List (Html.Attribute msg) -> Html msg
chevronRight =
    withAdditionalAttributes
        [ viewBox "0 0 20 20"
        , fill "currentColor"
        ]
        [ path
            [ d "M7.293 14.707a1 1 0 010-1.414L10.586 10 7.293 6.707a1 1 0 011.414-1.414l4 4a1 1 0 010 1.414l-4 4a1 1 0 01-1.414 0z"
            , fillRule "evenodd"
            , clipRule "evenodd"
            ]
            []
        ]


chevronDown : List (Html.Attribute msg) -> Html msg
chevronDown =
    withAdditionalAttributes
        [ viewBox "0 0 20 20"
        , fill "currentColor"
        ]
        [ path
            [ d "M5.293 7.293a1 1 0 011.414 0L10 10.586l3.293-3.293a1 1 0 111.414 1.414l-4 4a1 1 0 01-1.414 0l-4-4a1 1 0 010-1.414z"
            , fillRule "evenodd"
            , clipRule "evenodd"
            ]
            []
        ]


viewGridAdd : List (Html.Attribute msg) -> Html msg
viewGridAdd =
    withAdditionalAttributes
        [ stroke "currentColor"
        , fill "none"
        , viewBox "0 0 20 20"
        ]
        [ path
            [ d "M5 3a2 2 0 00-2 2v2a2 2 0 002 2h2a2 2 0 002-2V5a2 2 0 00-2-2H5zM5 11a2 2 0 00-2 2v2a2 2 0 002 2h2a2 2 0 002-2v-2a2 2 0 00-2-2H5zM11 5a2 2 0 012-2h2a2 2 0 012 2v2a2 2 0 01-2 2h-2a2 2 0 01-2-2V5zM14 11a1 1 0 011 1v1h1a1 1 0 110 2h-1v1a1 1 0 11-2 0v-1h-1a1 1 0 110-2h1v-1a1 1 0 011-1z" ]
            []
        ]


plus : List (Html.Attribute msg) -> Html msg
plus =
    withAdditionalAttributes
        [ stroke "none"
        , fill "currentColor"
        , viewBox "0 0 20 20"
        ]
        [ path
            [ d "M10 3a1 1 0 011 1v5h5a1 1 0 110 2h-5v5a1 1 0 11-2 0v-5H4a1 1 0 110-2h5V4a1 1 0 011-1z" ]
            []
        ]


exclamation : List (Html.Attribute msg) -> Html msg
exclamation =
    withAdditionalAttributes
        [ fill "none"
        , viewBox "0 0 24 24"
        , stroke "currentColor"
        ]
        [ path
            [ strokeLinecap "round"
            , strokeLinejoin "round"
            , strokeWidth "2"
            , d
                "M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z"
            ]
            []
        ]


x : List (Html.Attribute msg) -> Html msg
x =
    withAdditionalAttributes
        [ fill "none"
        , viewBox "0 0 24 24"
        , stroke "currentColor"
        ]
        [ path
            [ strokeLinecap "round"
            , strokeLinejoin "round"
            , strokeWidth "2"
            , d "M6 18L18 6M6 6l12 12"
            ]
            []
        ]


search : List (Html.Attribute msg) -> Html msg
search =
    withAdditionalAttributes
        [ fill "none"
        , stroke "currentColor"
        ]
        [ path
            [ d "m19 19-3.5-3.5"
            , stroke "currentColor"
            , strokeWidth "2"
            , strokeLinecap "round"
            , strokeLinejoin "round"
            ]
            []
        , circle
            [ cx "11"
            , cy "11"
            , r "6"
            , stroke "currentColor"
            , strokeWidth "2"
            , strokeLinecap "round"
            , strokeLinejoin "round"
            ]
            []
        ]


menu : List (Html.Attribute msg) -> Html msg
menu =
    withAdditionalAttributes
        [ fill "none"
        , viewBox "0 0 24 24"
        , stroke "currentColor"
        ]
        [ path
            [ strokeLinecap "round"
            , strokeLinejoin "round"
            , strokeWidth "2"
            , d "M4 6h16M4 12h8m-8 6h16"
            ]
            []
        ]
