module Icons exposing
    ( anki
    , arrowDown
    , arrowLongLeft
    , arrowLongRight
    , arrowUp
    , braces
    , chevronDown
    , computerDesktop
    , copy
    , cornerDownRight
    , cornerLeftUp
    , documentDownload
    , ellipsisVertical
    , exclamation
    , exclamationCircle
    , filter
    , markdown
    , maximize2
    , menu
    , moon
    , pencil
    , plus
    , search
    , sun
    , tick
    , trash
    , viewGridAdd
    , xMark
    )

import Html exposing (Html)
import Svg exposing (circle, clipPath, defs, g, line, node, path, polygon, polyline, rect, svg)
import Svg.Attributes
    exposing
        ( clipPathUnits
        , clipRule
        , cx
        , cy
        , d
        , fill
        , fillRule
        , gradientTransform
        , gradientUnits
        , height
        , offset
        , points
        , r
        , rx
        , ry
        , stroke
        , strokeLinecap
        , strokeLinejoin
        , strokeWidth
        , style
        , transform
        , viewBox
        , width
        , x
        , x1
        , x2
        , y
        , y1
        , y2
        )


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


tick : List (Html.Attribute msg) -> Html msg
tick =
    withAdditionalAttributes
        [ stroke "currentColor"
        , fill "none"
        , viewBox "0 0 24 24"
        , strokeWidth "2.5"
        ]
        [ path
            [ strokeLinecap "round"
            , strokeLinejoin "round"
            , d "M4.5 12.75l6 6 9-13.5"
            ]
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


xMark : List (Html.Attribute msg) -> Html msg
xMark =
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


documentDownload : List (Html.Attribute msg) -> Html msg
documentDownload =
    withAdditionalAttributes
        [ fill "none"
        , viewBox "0 0 24 24"
        , stroke "currentColor"
        ]
        [ path
            [ strokeLinecap "round"
            , strokeLinejoin "round"
            , strokeWidth "2"
            , d "M12 10v6m0 0l-3-3m3 3l3-3m2 8H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z"
            ]
            []
        ]


arrowUp : List (Html.Attribute msg) -> Html msg
arrowUp =
    withAdditionalAttributes
        [ fill "none"
        , viewBox "0 0 24 24"
        , stroke "currentColor"
        ]
        [ path
            [ strokeLinecap "round"
            , strokeLinejoin "round"
            , strokeWidth "2"
            , d "M5 10l7-7m0 0l7 7m-7-7v18"
            ]
            []
        ]


arrowDown : List (Html.Attribute msg) -> Html msg
arrowDown =
    withAdditionalAttributes
        [ fill "none"
        , viewBox "0 0 24 24"
        , stroke "currentColor"
        ]
        [ path
            [ strokeLinecap "round"
            , strokeLinejoin "round"
            , strokeWidth "2"
            , d "M19.5 13.5L12 21m0 0l-7.5-7.5M12 21V3"
            ]
            []
        ]


markdown : List (Html.Attribute msg) -> Html msg
markdown =
    withAdditionalAttributes
        [ fill "none"
        , height "128"
        , viewBox "0 0 208 128"
        , width "208"
        ]
        [ Svg.g
            [ fill "currentColor"
            ]
            [ path
                [ clipRule "evenodd"
                , d "m15 10c-2.7614 0-5 2.2386-5 5v98c0 2.761 2.2386 5 5 5h178c2.761 0 5-2.239 5-5v-98c0-2.7614-2.239-5-5-5zm-15 5c0-8.28427 6.71573-15 15-15h178c8.284 0 15 6.71573 15 15v98c0 8.284-6.716 15-15 15h-178c-8.28427 0-15-6.716-15-15z"
                , fillRule "evenodd"
                ]
                []
            , path
                [ d "m30 98v-68h20l20 25 20-25h20v68h-20v-39l-20 25-20-25v39zm125 0-30-33h20v-35h20v35h20z"
                ]
                []
            ]
        ]


anki : List (Html.Attribute msg) -> Html msg
anki =
    withAdditionalAttributes
        [ viewBox "0 0 6.0854168 7.9375"
        , fill "currentColor"
        ]
        [ defs []
            [ clipPath
                [ clipPathUnits "userSpaceOnUse"
                ]
                [ node "rect"
                    [ height "30"
                    , rx "4.6187186"
                    , ry "4.6187186"
                    , width "23"
                    , Svg.Attributes.x "5"
                    , y "1"
                    ]
                    []
                ]
            , node "linearGradient"
                [ gradientTransform "matrix(0.954545,0,0,0.965517,1.704545,0.551724)"
                , gradientUnits "userSpaceOnUse"
                , x1 "15.5"
                , x2 "4"
                , y1 "5.9497476"
                , y2 "31"
                ]
                []
            , node "linearGradient"
                []
                [ node "stop"
                    [ offset "0"
                    , style "stop-color:#3c3c3c;stop-opacity:1"
                    ]
                    []
                , node "stop"
                    [ offset "1"
                    , style "stop-color:#9e9e9e;stop-opacity:1"
                    ]
                    []
                ]
            , node "linearGradient"
                [ gradientUnits "userSpaceOnUse"
                , x1 "253.74718"
                , x2 "188.00023"
                , y1 "412.82977"
                , y2 "542.33295"
                ]
                []
            , node "linearGradient"
                []
                [ node "stop"
                    [ offset "0"
                    , style "stop-color:#0084dd;stop-opacity:1"
                    ]
                    []
                , node "stop"
                    [ offset "1"
                    , style "stop-color:white;stop-opacity:1"
                    ]
                    []
                ]
            , node "linearGradient"
                [ gradientTransform "matrix(0.09240924,0,0,0.09240924,-5.65772,-25.6423)"
                , gradientUnits "userSpaceOnUse"
                , x1 "247"
                , x2 "292"
                , y1 "441.86218"
                , y2 "282.36218"
                ]
                []
            , node "linearGradient"
                []
                [ node "stop"
                    [ offset "0"
                    , style "stop-color:white;stop-opacity:0.49411765;"
                    ]
                    []
                , node "stop"
                    [ offset "1"
                    , style "stop-color:white;stop-opacity:0.25098041;"
                    ]
                    []
                ]
            , node "linearGradient"
                [ gradientUnits "userSpaceOnUse"
                , x1 "253.74718"
                , x2 "188.00023"
                , y1 "412.82977"
                , y2 "542.33295"
                ]
                []
            ]
        , g [ transform "translate(-137.84645,-70.369748)" ]
            [ g
                [ style "display:inline"
                , transform "matrix(0.26458333,0,0,0.26458333,136.52353,70.105165)"
                ]
                [ node "rect"
                    [ height "32"
                    , rx "0"
                    , ry "0"
                    , width "32"
                    , Svg.Attributes.x "0"
                    , y "0"
                    ]
                    []
                , node "rect"
                    [ height "28"
                    , rx "4"
                    , ry "4"
                    , width "21"
                    , Svg.Attributes.x "6"
                    , y "2"
                    ]
                    []
                , path
                    [ d "m 312.50679,544.32837 c -20.28396,20.1643 -45.83498,-32.08006 -74.19864,-28.40036 -29.32333,3.80421 -41.202,60.28859 -67.50841,46.78663 -25.44547,-13.06007 16.34617,-53.50493 4.08171,-79.34329 -12.67942,-26.71258 -70.06997,-20.55523 -65.35797,-49.74645 4.55778,-28.23586 55.93746,-0.9878 76.72126,-20.63649 21.48703,-20.31349 -2.10362,-72.99241 27.11497,-77.53162 28.26233,-4.39065 18.22508,52.89443 43.33464,66.58923 25.95913,14.15816 68.76986,-24.55656 82.11594,1.82928 12.9093,25.52229 -44.67374,33.67836 -49.93898,61.7909 -5.4434,29.06371 44.60573,57.81562 23.63548,78.66217 z"
                    , style "fill:none;fill-opacity:1;stroke:none;stroke-width:16.2321;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1"
                    , transform "matrix(0.09169279,0.01148462,-0.01148462,0.09169279,1.24989,-28.62005)"
                    ]
                    []
                , path
                    [ d "m 294.46869,523.45259 c -9.08043,8.81335 -45.57491,-17.76993 -58.10148,-15.97712 -12.52658,1.79282 -40.09949,37.54547 -51.28749,31.63294 -11.18801,-5.91252 2.81678,-48.83551 -2.75921,-60.19499 -5.57599,-11.35947 -48.09928,-26.53469 -45.93342,-39.00218 2.16586,-12.46749 47.31577,-12.41208 56.39619,-21.22544 9.08043,-8.81335 10.3725,-53.9448 22.89908,-55.73762 12.52657,-1.79281 26.42597,41.16443 37.61397,47.07695 11.188,5.91252 54.50984,-6.80503 60.08583,4.55445 5.57599,11.35947 -30.98363,37.85309 -33.14948,50.32059 -2.16586,12.46749 23.31643,49.73906 14.23601,58.55242 z"
                    , style "fill:url(#linearGradient1230);fill-opacity:1;stroke:#ffffff;stroke-width:15.9837;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1"
                    , transform "matrix(0.07507522,4.398431e-4,-4.398431e-4,0.07507522,-2.36792,-12.92398)"
                    ]
                    []
                , path
                    [ d "m 294.46869,523.45259 c -9.08043,8.81335 -45.57491,-17.76993 -58.10148,-15.97712 -12.52658,1.79282 -40.09949,37.54547 -51.28749,31.63294 -11.18801,-5.91252 2.81678,-48.83551 -2.75921,-60.19499 -5.57599,-11.35947 -48.09928,-26.53469 -45.93342,-39.00218 2.16586,-12.46749 47.31577,-12.41208 56.39619,-21.22544 9.08043,-8.81335 10.3725,-53.9448 22.89908,-55.73762 12.52657,-1.79281 26.42597,41.16443 37.61397,47.07695 11.188,5.91252 54.50984,-6.80503 60.08583,4.55445 5.57599,11.35947 -30.98363,37.85309 -33.14948,50.32059 -2.16586,12.46749 23.31643,49.73906 14.23601,58.55242 z"
                    , style "fill:url(#linearGradient2975);fill-opacity:1;stroke:#ffffff;stroke-width:24.8699;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1"
                    , transform "matrix(0.03131375,-0.02522366,0.02522366,0.03131375,2.32085,0.206819)"
                    ]
                    []
                , path
                    [ d "M 2.659112,0.45056835 H 30.289475 L 29.457792,20.39883 C 25.879683,16.517642 12.825421,10.535309 3.6153,10.535309 Z"
                    , style "display:inline;opacity:1;fill:url(#linearGradient2979);fill-opacity:1;stroke:none;stroke-width:3;stroke-miterlimit:4;stroke-opacity:1"
                    ]
                    []
                ]
            ]
        ]


sun : List (Html.Attribute msg) -> Html msg
sun =
    withAdditionalAttributes
        [ fill "none"
        , viewBox "0 0 24 24"
        , strokeWidth "1.5"
        , stroke "currentColor"
        ]
        [ path
            [ strokeLinecap "round"
            , strokeLinejoin "round"
            , d "M12 3v2.25m6.364.386l-1.591 1.591M21 12h-2.25m-.386 6.364l-1.591-1.591M12 18.75V21m-4.773-4.227l-1.591 1.591M5.25 12H3m4.227-4.773L5.636 5.636M15.75 12a3.75 3.75 0 11-7.5 0 3.75 3.75 0 017.5 0z"
            ]
            []
        ]


moon : List (Html.Attribute msg) -> Html msg
moon =
    withAdditionalAttributes
        [ fill "none"
        , viewBox "0 0 24 24"
        , strokeWidth "1.5"
        , stroke "currentColor"
        ]
        [ path
            [ strokeLinecap "round"
            , strokeLinejoin "round"
            , d "M21.752 15.002A9.718 9.718 0 0118 15.75c-5.385 0-9.75-4.365-9.75-9.75 0-1.33.266-2.597.748-3.752A9.753 9.753 0 003 11.25C3 16.635 7.365 21 12.75 21a9.753 9.753 0 009.002-5.998z"
            ]
            []
        ]


computerDesktop : List (Html.Attribute msg) -> Html msg
computerDesktop =
    withAdditionalAttributes
        [ fill "none"
        , viewBox "0 0 24 24"
        , strokeWidth "1.5"
        , stroke "currentColor"
        ]
        [ path
            [ strokeLinecap "round"
            , strokeLinejoin "round"
            , d "M9 17.25v1.007a3 3 0 01-.879 2.122L7.5 21h9l-.621-.621A3 3 0 0115 18.257V17.25m6-12V15a2.25 2.25 0 01-2.25 2.25H5.25A2.25 2.25 0 013 15V5.25m18 0A2.25 2.25 0 0018.75 3H5.25A2.25 2.25 0 003 5.25m18 0V12a2.25 2.25 0 01-2.25 2.25H5.25A2.25 2.25 0 013 12V5.25"
            ]
            []
        ]


maximize2 : List (Html.Attribute msg) -> Html msg
maximize2 =
    withAdditionalAttributes
        [ fill "none"
        , viewBox "0 0 24 24"
        , strokeWidth "1.5"
        , stroke "currentColor"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ polyline
            [ points "15 3 21 3 21 9" ]
            []
        , polyline
            [ points "9 21 3 21 3 15" ]
            []
        , line
            [ x1 "21", x2 "14", y1 "3", y2 "10" ]
            []
        , line
            [ x1 "3", x2 "10", y1 "21", y2 "14" ]
            []
        ]


arrowLongLeft : List (Html.Attribute msg) -> Html msg
arrowLongLeft =
    withAdditionalAttributes
        [ fill "none"
        , viewBox "0 0 24 24"
        , strokeWidth "1.5"
        , stroke "currentColor"
        ]
        [ path
            [ strokeLinecap "round"
            , strokeLinejoin "round"
            , d "M6.75 15.75L3 12m0 0l3.75-3.75M3 12h18"
            ]
            []
        ]


arrowLongRight : List (Html.Attribute msg) -> Html msg
arrowLongRight =
    withAdditionalAttributes
        [ fill "none"
        , viewBox "0 0 24 24"
        , strokeWidth "1.5"
        , stroke "currentColor"
        ]
        [ path
            [ strokeLinecap "round"
            , strokeLinejoin "round"
            , d "M17.25 8.25L21 12m0 0l-3.75 3.75M21 12H3"
            ]
            []
        ]


ellipsisVertical : List (Html.Attribute msg) -> Html msg
ellipsisVertical =
    withAdditionalAttributes
        [ fill "none"
        , viewBox "0 0 24 24"
        , strokeWidth "1.5"
        , stroke "currentColor"
        ]
        [ path
            [ strokeLinecap "round"
            , strokeLinejoin "round"
            , d "M12 6.75a.75.75 0 110-1.5.75.75 0 010 1.5zM12 12.75a.75.75 0 110-1.5.75.75 0 010 1.5zM12 18.75a.75.75 0 110-1.5.75.75 0 010 1.5z"
            ]
            []
        ]


copy : List (Html.Attribute msg) -> Html msg
copy =
    withAdditionalAttributes
        [ fill "none"
        , viewBox "0 0 24 24"
        , strokeWidth "2"
        , stroke "currentColor"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ rect
            [ width "8"
            , height "4"
            , x "8"
            , y "2"
            , rx "1"
            , ry "1"
            ]
            []
        , path
            [ d "M8 4H6a2 2 0 0 0-2 2v14a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2v-2" ]
            []
        , path
            [ d "M16 4h2a2 2 0 0 1 2 2v4" ]
            []
        , path
            [ d "M21 14H11" ]
            []
        , path
            [ d "m15 10-4 4 4 4" ]
            []
        ]


cornerDownRight : List (Html.Attribute msg) -> Html msg
cornerDownRight =
    withAdditionalAttributes
        [ fill "none"
        , viewBox "0 0 24 24"
        , strokeWidth "1.5"
        , stroke "currentColor"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ polyline
            [ points "15 10 20 15 15 20" ]
            []
        , path
            [ d "M4 4v7a4 4 0 0 0 4 4h12" ]
            []
        ]


cornerLeftUp : List (Html.Attribute msg) -> Html msg
cornerLeftUp =
    withAdditionalAttributes
        [ fill "none"
        , viewBox "0 0 24 24"
        , strokeWidth "1.5"
        , stroke "currentColor"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ polyline
            [ points "14 9 9 4 4 9" ]
            []
        , path
            [ d "M20 20h-7a4 4 0 0 1-4-4V4" ]
            []
        ]


braces : List (Html.Attribute msg) -> Html msg
braces =
    withAdditionalAttributes
        [ fill "none"
        , viewBox "0 0 24 24"
        , strokeWidth "2"
        , stroke "currentColor"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ path
            [ d "M8 3H7a2 2 0 0 0-2 2v5a2 2 0 0 1-2 2 2 2 0 0 1 2 2v5c0 1.1.9 2 2 2h1" ]
            []
        , path
            [ d "M16 21h1a2 2 0 0 0 2-2v-5c0-1.1.9-2 2-2a2 2 0 0 1-2-2V5a2 2 0 0 0-2-2h-1" ]
            []
        ]


filter : List (Html.Attribute msg) -> Html msg
filter =
    withAdditionalAttributes
        [ fill "none"
        , viewBox "0 0 24 24"
        , strokeWidth "2"
        , stroke "currentColor"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        ]
        [ polygon
            [ points "22 3 2 3 10 12.46 10 19 14 21 14 12.46 22 3" ]
            []
        ]
