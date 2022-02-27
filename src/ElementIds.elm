module ElementIds exposing
    ( about
    , container
    , items
    , modalTitle
    , orderItemsAlphabetically
    , orderItemsMostFrequentFirst
    , reserved
    , title
    )


prefixed : String -> String
prefixed =
    (++) "glossary-page-"


reserved : String -> Bool
reserved id =
    List.member id
        [ container
        , title
        , about
        , items
        , orderItemsAlphabetically
        , orderItemsMostFrequentFirst
        , modalTitle
        ]


container : String
container =
    prefixed "container"


title : String
title =
    prefixed "title"


about : String
about =
    prefixed "about"


items : String
items =
    prefixed "items"


orderItemsAlphabetically : String
orderItemsAlphabetically =
    prefixed "order-items-alphabetically"


orderItemsMostFrequentFirst : String
orderItemsMostFrequentFirst =
    prefixed "order-items-most-frequent-first"


modalTitle : String
modalTitle =
    prefixed "modal-title"
