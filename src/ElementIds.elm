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
    , defaultThemeDark
    , defaultThemeLight
    , defaultThemeSystem
    , definition
    , deleteRelatedTermButton
    , deleteTermButton
    , disambiguationTagSelect
    , dragRelatedTermButton
    , dragTermButton
    , draggableSeeAlsoSelect
    , draggableTermInputField
    , exportDropdownButton
    , glossaryItemDiv
    , indexFilterInputField
    , indexForMobile
    , items
    , letterGrid
    , moreOptionsForRelatedTermDropdownMenu
    , moreOptionsForTermDropdownMenu
    , moveRelatedTermDownButton
    , moveRelatedTermUpButton
    , moveTermDownButton
    , moveTermUpButton
    , needsUpdatingToggleLabel
    , orderItemsAlphabetically
    , orderItemsFocusedOn
    , orderItemsFocusedOnSelect
    , orderItemsMostMentionedFirst
    , outer
    , reserved
    , searchDialog
    , seeAlsoSelect
    , showExportMenuLabel
    , showLastUpdatedDatesLabel
    , showOrderItemsButtons
    , staticSidebarForDesktop
    , tagDescriptionInputField
    , tagInputField
    , tags
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
import Data.GlossaryItemId as GlossaryItemId exposing (GlossaryItemId)
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
    String.toLower >> String.startsWith prefix


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


tags : String
tags =
    prefixed "tags"


items : String
items =
    prefixed "items"


cardWidthCompact : String
cardWidthCompact =
    prefixed "card-width-compact"


cardWidthIntermediate : String
cardWidthIntermediate =
    prefixed "card-width-intermediate"


cardWidthWide : String
cardWidthWide =
    prefixed "card-width-wide"


defaultThemeLight : String
defaultThemeLight =
    prefixed "default-theme-light"


defaultThemeDark : String
defaultThemeDark =
    prefixed "default-theme-dark"


defaultThemeSystem : String
defaultThemeSystem =
    prefixed "default-theme-system"


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


draggableTermInputField : TermIndex -> String
draggableTermInputField termIndex =
    prefixed <| "draggable-term-" ++ (termIndex |> TermIndex.toInt |> String.fromInt)


definition : String
definition =
    prefixed <| "definition"


seeAlsoSelect : RelatedTermIndex -> String
seeAlsoSelect index =
    prefixed <| "see-also-" ++ (index |> RelatedTermIndex.toInt |> String.fromInt)


draggableSeeAlsoSelect : RelatedTermIndex -> String
draggableSeeAlsoSelect index =
    prefixed <| "draggable-see-also-" ++ (index |> RelatedTermIndex.toInt |> String.fromInt)


disambiguationTagSelect : String
disambiguationTagSelect =
    prefixed "disambiguation-tag-select"


orderItemsFocusedOnSelect : String
orderItemsFocusedOnSelect =
    prefixed "order-items-focused-on-select"


termIndexGroupLabel : Bool -> String -> String
termIndexGroupLabel staticSidebar groupLabel =
    if staticSidebar then
        prefixed <| "index-group-static-sidebar-" ++ groupLabel

    else
        prefixed <| "index-group-" ++ groupLabel


glossaryItemDiv : GlossaryItemId -> String
glossaryItemDiv index =
    prefixed <| "item-" ++ (index |> GlossaryItemId.toString)


indexForMobile : String
indexForMobile =
    prefixed "menu-for-mobile"


staticSidebarForDesktop : String
staticSidebarForDesktop =
    prefixed "static-sidebar-for-desktop"


letterGrid : String
letterGrid =
    prefixed "letter-grid"


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


moreOptionsForTermDropdownMenu : Int -> String
moreOptionsForTermDropdownMenu relatedTermIndexInt =
    prefixed "more-options-for-term-" ++ String.fromInt relatedTermIndexInt ++ "-dropdown-menu"


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


showOrderItemsButtons : String
showOrderItemsButtons =
    prefixed "show-order-items-buttons"


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


tagInputField : Int -> String
tagInputField index =
    prefixed <| "tag-" ++ String.fromInt index


tagDescriptionInputField : Int -> String
tagDescriptionInputField index =
    prefixed <| "tag-description-" ++ String.fromInt index


indexFilterInputField : String
indexFilterInputField =
    prefixed "index-filter-input-field"


moveTermUpButton : Int -> String
moveTermUpButton index =
    prefixed <| "move-term-up-" ++ String.fromInt index ++ "-button"


moveTermDownButton : Int -> String
moveTermDownButton index =
    prefixed <| "move-term-down-" ++ String.fromInt index ++ "-button"


deleteTermButton : Int -> String
deleteTermButton index =
    prefixed <| "delete-term-" ++ String.fromInt index ++ "-button"


dragTermButton : Int -> String
dragTermButton index =
    prefixed <| "drag-term-" ++ String.fromInt index ++ "-button"


moveRelatedTermUpButton : Int -> String
moveRelatedTermUpButton index =
    prefixed <| "move-related-term-up-" ++ String.fromInt index ++ "-button"


moveRelatedTermDownButton : Int -> String
moveRelatedTermDownButton index =
    prefixed <| "move-related-term-down-" ++ String.fromInt index ++ "-button"


deleteRelatedTermButton : Int -> String
deleteRelatedTermButton index =
    prefixed <| "delete-related-term-" ++ String.fromInt index ++ "-button"


dragRelatedTermButton : Int -> String
dragRelatedTermButton index =
    prefixed <| "drag-related-term-" ++ String.fromInt index ++ "-button"
