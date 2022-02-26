module ElementIds exposing
    ( about
    , items
    , modalTitle
    , orderItemsAlphabetically
    , orderItemsMostFrequentFirst
    , outer
    , reserved
    , title
    )


prefixed : String -> String
prefixed =
    (++) "glossary-page-"


reserved : String -> Bool
reserved id =
    List.member id
        [ outer
        , title
        , about
        , items
        , orderItemsAlphabetically
        , orderItemsMostFrequentFirst
        , modalTitle
        ]


outer =
    prefixed "outer"


title =
    prefixed "title"


about =
    prefixed "about"


items =
    prefixed "items"


orderItemsAlphabetically =
    prefixed "order-items-alphabetically"


orderItemsMostFrequentFirst =
    prefixed "order-items-most-frequent-first"


modalTitle =
    prefixed "modal-title"
