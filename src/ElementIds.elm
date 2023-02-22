module ElementIds exposing
    ( abbreviationLabel
    , about
    , aboutLinkBody
    , aboutLinkHref
    , cardWidthCompact
    , cardWidthIntermediate
    , cardWidthWide
    , confirmDeleteModalTitle
    , container
    , descriptionDetailsSingle
    , exportDropdownButton
    , glossaryItemDiv
    , indexForMobile
    , inputSyntaxMarkdownBased
    , inputSyntaxPlainText
    , items
    , orderItemsAlphabetically
    , orderItemsMostFrequentFirst
    , outer
    , quickSearchButtonAndLetterGrid
    , reserved
    , searchDialog
    , seeAlsoSelect
    , showExportMenuLabel
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


outer : String
outer =
    prefixed "outer"


about : String
about =
    prefixed "about"


items : String
items =
    prefixed "items"


inputSyntaxPlainText : String
inputSyntaxPlainText =
    prefixed "input-syntax-plain-text"


inputSyntaxMarkdownBased : String
inputSyntaxMarkdownBased =
    prefixed "input-syntax-markdown-based"


cardWidthCompact : String
cardWidthCompact =
    prefixed "card-width-compact"


cardWidthIntermediate : String
cardWidthIntermediate =
    prefixed "card-width-intermediate"


cardWidthWide : String
cardWidthWide =
    prefixed "card-width-wide"


orderItemsAlphabetically : String
orderItemsAlphabetically =
    prefixed "order-items-alphabetically"


orderItemsMostFrequentFirst : String
orderItemsMostFrequentFirst =
    prefixed "order-items-most-frequent-first"


confirmDeleteModalTitle : String
confirmDeleteModalTitle =
    prefixed "confirm-delete-modal-title"


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
    prefixed "menu-for-mobile"


staticSidebarForDesktop : String
staticSidebarForDesktop =
    prefixed "static-sidebar-for-desktop"


quickSearchButtonAndLetterGrid : String
quickSearchButtonAndLetterGrid =
    prefixed "search-button-and-letter-grid"


termsIndex : Bool -> String
termsIndex staticSidebar =
    if staticSidebar then
        prefixed "terms-index-static-sidebar"

    else
        prefixed "terms-index"


aboutLinkHref : AboutLinkIndex -> String
aboutLinkHref index =
    prefixed <| "about-link-href-" ++ (index |> AboutLinkIndex.toInt |> String.fromInt)


aboutLinkBody : AboutLinkIndex -> String
aboutLinkBody index =
    prefixed <| "about-link-body-" ++ (index |> AboutLinkIndex.toInt |> String.fromInt)


exportDropdownButton : String
exportDropdownButton =
    prefixed "export-dropdown-button"


abbreviationLabel : TermIndex -> String
abbreviationLabel termIndex =
    prefixed <| "term-" ++ (termIndex |> TermIndex.toInt |> String.fromInt) ++ "-abbreviation"


searchDialog : String
searchDialog =
    prefixed "search-dialog"


showExportMenuLabel : String
showExportMenuLabel =
    prefixed "show-export-menu"
