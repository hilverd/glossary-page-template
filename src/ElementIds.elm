module ElementIds exposing
    ( about
    , aboutLinkBody
    , aboutLinkHref
    , container
    , descriptionDetailsSingle
    , glossaryItemDiv
    , indexForMobile
    , items
    , modalTitle
    , orderItemsAlphabetically
    , orderItemsMostFrequentFirst
    , quickSearchButtonAndLetterGrid
    , reserved
    , seeAlsoSelect
    , staticSidebarForDesktop
    , termIndexGroupLabel
    , termInputField
    , termsIndex
    , title
    )

import Data.AboutLinkIndex as AboutLinkIndex exposing (AboutLinkIndex)
import Data.DetailsIndex as DetailsIndex exposing (DetailsIndex)
import Data.GlossaryItemIndex as GlossaryItemIndex exposing (GlossaryItemIndex)
import Data.RelatedTermIndex as RelatedTermIndex exposing (RelatedTermIndex)
import Data.TermIndex as TermIndex exposing (TermIndex)


prefix : String
prefix =
    "glossary-page-"


prefixed : String -> String
prefixed =
    (++) prefix


reserved : String -> Bool
reserved =
    String.startsWith prefix


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


termInputField : TermIndex -> String
termInputField termIndex =
    prefixed <| "term-" ++ (termIndex |> TermIndex.toInt |> String.fromInt)


descriptionDetailsSingle : DetailsIndex -> String
descriptionDetailsSingle index =
    prefixed <| "details-" ++ (index |> DetailsIndex.toInt |> String.fromInt)


seeAlsoSelect : RelatedTermIndex -> String
seeAlsoSelect index =
    prefixed <| "see-also-" ++ (index |> RelatedTermIndex.toInt |> String.fromInt)


termIndexGroupLabel : Bool -> String -> String
termIndexGroupLabel staticSidebar groupLabel =
    if staticSidebar then
        prefixed <| "index-group-static-sidebar-" ++ groupLabel

    else
        prefixed <| "index-group-" ++ groupLabel


glossaryItemDiv : GlossaryItemIndex -> String
glossaryItemDiv index =
    prefixed <| "item-" ++ (index |> GlossaryItemIndex.toInt |> String.fromInt)


indexForMobile : String
indexForMobile =
    prefixed <| "menu-for-mobile"


staticSidebarForDesktop : String
staticSidebarForDesktop =
    prefixed <| "static-sidebar-for-desktop"


quickSearchButtonAndLetterGrid : String
quickSearchButtonAndLetterGrid =
    prefixed <| "search-button-and-letter-grid"


termsIndex : Bool -> String
termsIndex staticSidebar =
    if staticSidebar then
        prefixed <| "terms-index-static-sidebar"

    else
        prefixed <| "terms-index"


aboutLinkHref : AboutLinkIndex -> String
aboutLinkHref index =
    prefixed <| "about-link-href-" ++ (index |> AboutLinkIndex.toInt |> String.fromInt)


aboutLinkBody : AboutLinkIndex -> String
aboutLinkBody index =
    prefixed <| "about-link-body-" ++ (index |> AboutLinkIndex.toInt |> String.fromInt)
