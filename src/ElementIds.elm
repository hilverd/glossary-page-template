module ElementIds exposing
    ( abbreviationLabel
    , about
    , aboutLinkBody
    , aboutLinkHref
    , aboutParagraphInputField
    , cardWidthCompact
    , cardWidthIntermediate
    , cardWidthWide
    , confirmDeleteModalTitle
    , container
    , definitionSingle
    , exportDropdownButton
    , glossaryItemDiv
    , indexForMobile
    , inputSyntaxMarkdownBased
    , inputSyntaxPlainText
    , items
    , moreOptionsForRelatedTermDropdownMenu
    , needsUpdatingToggleLabel
    , orderItemsAlphabetically
    , orderItemsFocusedOn
    , orderItemsFocusedOnSelect
    , orderItemsMostMentionedFirst
    , outer
    , quickSearchButtonAndLetterGrid
    , reserved
    , searchDialog
    , seeAlsoSelect
    , showExportMenuLabel
    , showLastUpdatedDatesLabel
    , staticSidebarForDesktop
    , termIndexGroupLabel
    , termInputField
    , termsIndex
    , textFieldWithCommandToRunEditor
    , themeDropdownButton
    , title
    , titleInputField
    , viewSingleItemModalTitle
    )

import Data.AboutLinkIndex as AboutLinkIndex exposing (AboutLinkIndex)
import Data.DefinitionIndex as DefinitionIndex exposing (DefinitionIndex)
import Data.GlossaryItem.TermId as TermId exposing (TermId)
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


orderItemsMostMentionedFirst : String
orderItemsMostMentionedFirst =
    prefixed "order-items-most-mentioned-first"


orderItemsFocusedOn : String
orderItemsFocusedOn =
    prefixed "order-items-focused-on"


confirmDeleteModalTitle : String
confirmDeleteModalTitle =
    prefixed "confirm-delete-modal-title"


viewSingleItemModalTitle : String
viewSingleItemModalTitle =
    prefixed "view-single-item-modal-title"


termInputField : TermIndex -> String
termInputField termIndex =
    prefixed <| "term-" ++ (termIndex |> TermIndex.toInt |> String.fromInt)


definitionSingle : DefinitionIndex -> String
definitionSingle index =
    prefixed <| "definition-" ++ (index |> DefinitionIndex.toInt |> String.fromInt)


seeAlsoSelect : RelatedTermIndex -> String
seeAlsoSelect index =
    prefixed <| "see-also-" ++ (index |> RelatedTermIndex.toInt |> String.fromInt)


orderItemsFocusedOnSelect : String
orderItemsFocusedOnSelect =
    prefixed "order-items-focused-on-select"


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


themeDropdownButton : String
themeDropdownButton =
    prefixed "theme-dropdown-button"


exportDropdownButton : String
exportDropdownButton =
    prefixed "export-dropdown-button"


moreOptionsForRelatedTermDropdownMenu : Int -> String
moreOptionsForRelatedTermDropdownMenu relatedTermIndexInt =
    prefixed "more-options-for-related-term-" ++ String.fromInt relatedTermIndexInt ++ "-dropdown-menu"


abbreviationLabel : TermIndex -> String
abbreviationLabel termIndex =
    prefixed <| "term-" ++ (termIndex |> TermIndex.toInt |> String.fromInt) ++ "-abbreviation"


needsUpdatingToggleLabel : String
needsUpdatingToggleLabel =
    prefixed "needs-updating-toggle"


searchDialog : String
searchDialog =
    prefixed "search-dialog"


showExportMenuLabel : String
showExportMenuLabel =
    prefixed "show-export-menu"


showLastUpdatedDatesLabel : String
showLastUpdatedDatesLabel =
    prefixed "show-last-updated-dates"


textFieldWithCommandToRunEditor : String
textFieldWithCommandToRunEditor =
    prefixed "text-field-with-command-to-run-editor"


titleInputField : String
titleInputField =
    prefixed "title-input-field"


aboutParagraphInputField : String
aboutParagraphInputField =
    prefixed "about-paragraph-input-field"
