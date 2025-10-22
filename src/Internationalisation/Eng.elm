module Internationalisation.Eng exposing (abbreviation, about, addLinkButton, addRelatedItem, addTag, addTermButton, alphabetically, alternativeTerm, ankiDeck, areYouSureYouWantToDeleteThisItem, backToTop, builtUsingGlossaryPageTemplate, builtUsingGlossaryPageTemplateHtmlTree, cancel, cardWidth, cardWidthCompact, cardWidthIntermediate, cardWidthWide, changesAreLostWhenYouReload, chooseWhichTagShouldBeUsedToDistinguishThisItem, close, closeSidebar, commandK, controlK, copy, copyToClipboard, createANewGlossaryItem, createANewGlossaryItemCapitalised, defaultTheme, definition, delete, deleteItem, deleted, description, disambiguationTag, disambiguationTagOptional, dragOrUseUpAndDownArrowsToMoveTerm, edit, editGlossaryItemCapitalised, editTitleAndAboutSectionButton, editTitleAndAboutSectionHeading, elementNotFound, example, explanationForFocusedOn, explanationForMostMentionedFirst, export, failedToParseMarkdown, failedToRenderMarkdown, failedToSave, filter, filteringByTag, focusOnTerm, focusedOn, glossaryCapitalised, glossaryContainsTooManyItems, howToEnableMathSupport, howToMakeChangesTitle, httpErrorDescription, json, links, listTheGroupOfTermsBeingDefined, loadingEllipsis, manageTags, manageTagsTitle, markdownAndTeXSupported, mathSupportIsEnabled, miscellaneous, mostMentionedFirst, moveDown, moveUp, needsUpdating, noMatchesFound, noMatchingItemsFound, none, openOptions, openSidebar, orderItems, otherChangesWereMadePleaseReload, otherItems, pleaseSelect, pleaseSelectAnItem, pointToAnyRelatedItems, preferredTerm, preferredTermCannotAlsoAppearAsAnAlternativeTerm, preview, provideADefinitionForThisGroupOfTerms, readMore, relatedItem, relatedItems, remove, runTheFollowingCommand, save, saved, search, searchPlaceholder, see, seeAlso, selectAllTagsThatApplyToThisItem, settings, showExportMenu, showLastUpdatedDates, showOrderItemsButtons, showSavingChangesInMemoryMessage, showingXOfYMatches, somethingWentWrong, startEditing, startingItemForNewLayout, stopEditing, suggestions, switchBackToOldLayout, tag, tagAppearsMultipleTimes, tags, term, terms, textLabel, theItemHasBeenDeleted, themeDark, themeLight, themeSystem, thereAreErrorsOnThisFormSeeAbove, thereAreMultipleItemsWithDisambiguatedPreferredTerm, thereAreMultipleItemsWithTheSameDisambiguatedPreferredTerm, thereIsAlreadyATag, thereIsAlreadyATagWithId, thereIsAlreadyAnItemWithDisambiguatedPreferredTermId, thereIsAlreadyAnItemWithId, thereIsNoItemWithId, thereIsNoTagWithId, theseSettingsAreUpdatedInTheHtmlFile, thisAlternativeTermOccursMultipleTimes, thisFieldCannotBeEmpty, thisFileIsMeantToBeImportedIntoAnki, thisTagIsADuplicateOfAnEarlierOne, thisTermAlreadyExistsElsewhere, thisTermIsReserved, thisTermOccursMultipleTimes, title, unableToSaveAsItWouldResultInTheFollowing, unauthorisedPleaseReload, unknownTheme, updatedOn, url, useSetting, viewAsSingleItem, webInterfaceDescription, whyTagsMayBeUseful, youCanHideTheseInstructions, youCanUseTagsToAttachLabels, yourChangesHaveBeenSaved)

{-| User interface text in the English language.
-}

import Accessibility.Key
import ElementIds
import Extras.Html
import Extras.HtmlAttribute
import Extras.HtmlTree as HtmlTree
import Html exposing (Html, a, code, div, p, pre, span, text)
import Html.Attributes exposing (class, href)
import Http


showSavingChangesInMemoryMessage : String
showSavingChangesInMemoryMessage =
    "[Sandbox mode — changes are lost when you reload the page]"


changesAreLostWhenYouReload : String
changesAreLostWhenYouReload =
    "[Changes are lost when you reload]"


backToTop : String
backToTop =
    "Back to top"


search : String
search =
    "Search"


searchPlaceholder : String
searchPlaceholder =
    "Search..."


noMatchesFound : String
noMatchesFound =
    "No matches found."


showingXOfYMatches : String -> String -> String
showingXOfYMatches x y =
    "Showing " ++ x ++ " of " ++ y ++ " matches."


export : String
export =
    "Export"


ankiDeck : String
ankiDeck =
    "Anki deck"


json : String
json =
    "JSON"


defaultTheme : String
defaultTheme =
    "Default theme"


themeLight : String
themeLight =
    "Light"


themeDark : String
themeDark =
    "Dark"


themeSystem : String
themeSystem =
    "System"


howToMakeChangesTitle : String
howToMakeChangesTitle =
    "How to Make Changes"


webInterfaceDescription : Html msg
webInterfaceDescription =
    p
        [ class "mt-3" ]
        [ text "This page includes a web interface for making changes that are saved back to the HTML file itself."
        , text " This is meant to be used "
        , span [ class "font-semibold" ] [ text "locally" ]
        , text " by a "
        , span [ class "font-semibold" ] [ text "single user" ]
        , text " at a time and works best if the file is kept under version control."
        ]


runTheFollowingCommand : Bool -> Html msg
runTheFollowingCommand tabbable =
    p []
        [ text "If you're on macOS, Linux, or Cygwin and have "
        , a
            [ href "https://nodejs.org/"
            , Html.Attributes.target "_blank"
            , Accessibility.Key.tabbable tabbable
            ]
            [ text "Node.js" ]
        , text " installed, then run the following command."
        ]


youCanHideTheseInstructions : Html msg
youCanHideTheseInstructions =
    p
        [ class "mt-3" ]
        [ text "You can hide these instructions altogether by setting the "
        , Extras.Html.inlineCode "data-enable-help-for-making-changes"
        , text " attribute to "
        , Extras.Html.inlineCode "false"
        , text " on the "
        , code [] [ text <| "<div id=\"" ++ ElementIds.container ++ "\">" ]
        , text " element."
        ]


tag : String
tag =
    "Tag"


tags : String
tags =
    "Tags"


filteringByTag : String
filteringByTag =
    "Filtering by tag"


orderItems : String
orderItems =
    "Order items"


alphabetically : String
alphabetically =
    "alphabetically"


mostMentionedFirst : String
mostMentionedFirst =
    "most mentioned first"


focusedOn : String
focusedOn =
    "focused on"


pleaseSelect : String
pleaseSelect =
    "Please select"


builtUsingGlossaryPageTemplate : Bool -> Html msg
builtUsingGlossaryPageTemplate tabbable =
    p []
        [ text "Built using "
        , a
            [ Html.Attributes.target "_blank"
            , Accessibility.Key.tabbable tabbable
            , href "https://glossary.page/template"
            ]
            [ span
                [ class "font-semibold print:font-normal" ]
                [ text "Glossary Page Template" ]
            ]
        , span
            [ class "hidden print:inline" ]
            [ text " (https://glossary.page/template)" ]
        , text "."
        ]


builtUsingGlossaryPageTemplateHtmlTree : List HtmlTree.HtmlTree
builtUsingGlossaryPageTemplateHtmlTree =
    [ HtmlTree.Leaf "Built using "
    , HtmlTree.Node "a"
        False
        [ HtmlTree.Attribute "target" "_blank"
        , HtmlTree.Attribute "href" "https://glossary.page/template"
        ]
        [ HtmlTree.Leaf "Glossary Page Template" ]
    , HtmlTree.Leaf "."
    ]


updatedOn : Maybe String -> Maybe String -> String -> Html msg
updatedOn name emailAddress date =
    div
        [ class "text-right text-sm mt-1.5 mb-2.5 text-gray-500 dark:text-gray-400" ]
        [ text "Updated: "
        , Html.node "last-updated"
            [ Html.Attributes.attribute "datetime" date ]
            []
        , Extras.Html.showMaybe
            (\name_ ->
                span
                    []
                    [ text " by "
                    , span
                        [ Extras.HtmlAttribute.showMaybe Html.Attributes.title emailAddress ]
                        [ text name_ ]
                    ]
            )
            name
        ]


startEditing : String
startEditing =
    "Start editing"


stopEditing : String
stopEditing =
    "Stop editing"


settings : String
settings =
    "Settings"


seeAlso : String
seeAlso =
    "See also"


see : String
see =
    "See"


explanationForMostMentionedFirst : String
explanationForMostMentionedFirst =
    "Items that are mentioned in many other items are shown first."


explanationForFocusedOn : Html msg -> Html msg
explanationForFocusedOn termHtml =
    p
        [ class "mt-2 text-gray-700 dark:text-gray-300" ]
        [ text "Items closely related to \""
        , termHtml
        , text "\" are shown first. This is determined based on \"See also\" links."
        ]


cardWidth : String
cardWidth =
    "Card width"


cardWidthCompact : String
cardWidthCompact =
    "Compact"


cardWidthIntermediate : String
cardWidthIntermediate =
    "Intermediate"


cardWidthWide : String
cardWidthWide =
    "Wide"


showExportMenu : String
showExportMenu =
    "Show \"Export\" menu"


showOrderItemsButtons : String
showOrderItemsButtons =
    "Show \"Order items\" buttons"


showLastUpdatedDates : String
showLastUpdatedDates =
    "Show last updated date for each item"


editTitleAndAboutSectionButton : String
editTitleAndAboutSectionButton =
    "Edit title and about section"


editTitleAndAboutSectionHeading : String
editTitleAndAboutSectionHeading =
    "Edit Title and About Section"


createANewGlossaryItem : String
createANewGlossaryItem =
    "Create a new glossary item"


manageTags : String
manageTags =
    "Manage tags"


manageTagsTitle : String
manageTagsTitle =
    "Manage Tags"


edit : String
edit =
    "Edit"


remove : String
remove =
    "Remove"


delete : String
delete =
    "Delete"


deleteItem : String
deleteItem =
    "Delete item"


areYouSureYouWantToDeleteThisItem : String
areYouSureYouWantToDeleteThisItem =
    "Are you sure you want to delete this item?"


cancel : String
cancel =
    "Cancel"


deleted : String
deleted =
    "Deleted"


theItemHasBeenDeleted : String
theItemHasBeenDeleted =
    "The item has been deleted."


saved : String
saved =
    "Saved"


yourChangesHaveBeenSaved : String
yourChangesHaveBeenSaved =
    "Your changes have been saved."


title : String
title =
    "Title"


about : String
about =
    "About"


addLinkButton : String
addLinkButton =
    "Add link"


url : String
url =
    "URL"


textLabel : String
textLabel =
    "Text"


markdownAndTeXSupported : Bool -> Html msg
markdownAndTeXSupported mathSupportEnabled =
    Html.span
        []
        [ Html.a
            [ Html.Attributes.href "https://commonmark.org/help/"
            , Html.Attributes.target "_blank"
            , class "text-inherit no-underline text-gray-500 dark:text-gray-400 font-normal"
            ]
            [ Html.text "Markdown" ]
        , Extras.Html.showIf mathSupportEnabled <|
            Html.span []
                [ Html.text " and "
                , Html.a
                    [ Html.Attributes.href "https://katex.org/docs/supported.html"
                    , Html.Attributes.target "_blank"
                    , class "text-inherit no-underline text-gray-500 dark:text-gray-400 font-normal"
                    ]
                    [ Html.node "katex-inline"
                        [ Html.Attributes.attribute "data-expr" "\\TeX"
                        ]
                        []
                    ]
                ]
        , Html.text " supported."
        ]


preview : String
preview =
    "Preview"


save : String
save =
    "Save"


thereAreErrorsOnThisFormSeeAbove : String
thereAreErrorsOnThisFormSeeAbove =
    "There are errors on this form — see above."


somethingWentWrong : String
somethingWentWrong =
    "Something went wrong."


links : String
links =
    "Links"


glossaryContainsTooManyItems : Int -> Html msg
glossaryContainsTooManyItems recommendedMaximumNumberOfItems =
    div
        [ class "mt-4 text-red-600 dark:text-red-400 flex items-center max-w-prose" ]
        [ span
            [ class "font-medium" ]
            [ text "⚠ This glossary contains more than "
            , text <| String.fromInt recommendedMaximumNumberOfItems
            , text " items, which is currently "
            , a
                [ href "https://github.com/hilverd/glossary-page-template#known-limitations" ]
                [ text "not recommended" ]
            , text " for performance reasons."
            ]
        ]


noMatchingItemsFound : String
noMatchingItemsFound =
    "No matching items found."


openSidebar : String
openSidebar =
    "Open sidebar"


closeSidebar : String
closeSidebar =
    "Close sidebar"


controlK : String
controlK =
    "Ctrl K"


commandK : String
commandK =
    "⌘ K"


theseSettingsAreUpdatedInTheHtmlFile : String
theseSettingsAreUpdatedInTheHtmlFile =
    "These settings are updated in the HTML file when you change them, and the page will reload."


failedToSave : String
failedToSave =
    "Failed to save"


unauthorisedPleaseReload : String
unauthorisedPleaseReload =
    "unauthorized, please reload the page and try again"


otherChangesWereMadePleaseReload : String
otherChangesWereMadePleaseReload =
    "other changes were made, please reload the page and try again"


httpErrorDescription : Http.Error -> String
httpErrorDescription error =
    case error of
        Http.BadUrl urlString ->
            "bad URL: " ++ urlString

        Http.Timeout ->
            "the request timed out"

        Http.NetworkError ->
            "there was a network error"

        Http.BadStatus statusCode ->
            "unexpected status code " ++ String.fromInt statusCode

        Http.BadBody body ->
            "unexpected response body: " ++ body


howToEnableMathSupport : Html msg
howToEnableMathSupport =
    div
        [ class "mt-2 max-w-prose" ]
        [ text "To add support for math typesetting, include KaTeX's stylesheet and script inside the "
        , code [] [ text "<head>" ]
        , text " element as shown in the "
        , a
            [ class "font-semibold"
            , href "https://github.com/hilverd/glossary-page-template/releases/latest/download/glossary.html"
            , Html.Attributes.download "glossary.html"
            ]
            [ text "glossary.html" ]
        , text " template."
        ]


mathSupportIsEnabled : Html msg
mathSupportIsEnabled =
    div
        [ class "mt-2 max-w-prose" ]
        [ text "Math typesetting support is enabled. Inline math is written like"
        , pre
            [ class "mt-4" ]
            [ code
                []
                [ text "`$e = mc^2$`" ]
            ]
        , p [ class "mt-4" ] [ text "and display math is written like this:" ]
        , pre
            [ class "mt-4" ]
            [ code
                []
                [ text "```math\ne = mc^2\n```" ]
            ]
        ]


copy : String
copy =
    "Copy"


copyToClipboard : String
copyToClipboard =
    "Copy to clipboard"


focusOnTerm : String
focusOnTerm =
    "Focus on term"


glossaryCapitalised : String
glossaryCapitalised =
    "Glossary"


otherItems : String
otherItems =
    "Other items"


description : String
description =
    "Description"


addTag : String
addTag =
    "Add tag"


unableToSaveAsItWouldResultInTheFollowing : String
unableToSaveAsItWouldResultInTheFollowing =
    "Unable to save as it would result in the following"


thereAreMultipleItemsWithDisambiguatedPreferredTerm : String -> String
thereAreMultipleItemsWithDisambiguatedPreferredTerm rawTerm =
    "There are multiple items with (disambiguated) preferred term identifier \""
        ++ rawTerm
        ++ "\""


thereAreMultipleItemsWithTheSameDisambiguatedPreferredTerm : String
thereAreMultipleItemsWithTheSameDisambiguatedPreferredTerm =
    "There are multiple items with the same (disambiguated) preferred term identifier"


tagAppearsMultipleTimes : String -> String
tagAppearsMultipleTimes rawTag =
    "tag \"" ++ rawTag ++ "\" appears multiple times"


youCanUseTagsToAttachLabels : String
youCanUseTagsToAttachLabels =
    "You can use tags to attach \"labels\" to items that people can then filter by."


readMore : String
readMore =
    "Read more"


whyTagsMayBeUseful : String
whyTagsMayBeUseful =
    "This may be useful for large glossaries that span multiple topics, where there is a need to categorize or group items."
        ++ " Tags can also be used to \"disambiguate\" items that have the same preferred term but whose meaning depends on some \"context\"."
        ++ " For example, the term \"default\" has a different meaning in the context of computer science than it does in the context of finance."


example : String
example =
    "Example"


abbreviation : String
abbreviation =
    "Abbreviation"


terms : String
terms =
    "Terms"


listTheGroupOfTermsBeingDefined : List (Html msg)
listTheGroupOfTermsBeingDefined =
    [ text "List the group of terms being defined. The first one is considered the "
    , Html.em
        []
        [ text "preferred" ]
    , text " term."
    ]


addTermButton : String
addTermButton =
    "Add term"


definition : String
definition =
    "Definition"


provideADefinitionForThisGroupOfTerms : String
provideADefinitionForThisGroupOfTerms =
    "Provide a definition (optional) for this group of terms."


selectAllTagsThatApplyToThisItem : String
selectAllTagsThatApplyToThisItem =
    "Select all tags that apply to this item."


disambiguationTagOptional : String
disambiguationTagOptional =
    "Disambiguation tag (optional)"


disambiguationTag : String
disambiguationTag =
    "Disambiguation tag"


none : String
none =
    "None"


chooseWhichTagShouldBeUsedToDistinguishThisItem : String
chooseWhichTagShouldBeUsedToDistinguishThisItem =
    "If another item has the same preferred term, then choose which tag should be used to distinguish this item."


relatedItem : String
relatedItem =
    "Related item"


moveUp : String
moveUp =
    "Move up"


moveDown : String
moveDown =
    "Move down"


addRelatedItem : String
addRelatedItem =
    "Add related item"


relatedItems : String
relatedItems =
    "Related items"


pointToAnyRelatedItems : String
pointToAnyRelatedItems =
    "Point to any related items the reader might want to look up."


suggestions : String
suggestions =
    "Suggestions"


miscellaneous : String
miscellaneous =
    "Miscellaneous"


needsUpdating : String
needsUpdating =
    "Needs updating"


createANewGlossaryItemCapitalised : String
createANewGlossaryItemCapitalised =
    "Create a New Glossary Item"


editGlossaryItemCapitalised : String
editGlossaryItemCapitalised =
    "Edit Glossary Item"


preferredTerm : String
preferredTerm =
    "Preferred term"


alternativeTerm : String
alternativeTerm =
    "Alternative term"


term : String
term =
    "Term"


openOptions : String
openOptions =
    "Open options"


viewAsSingleItem : String
viewAsSingleItem =
    "View as single item"


loadingEllipsis : String
loadingEllipsis =
    "Loading..."


failedToRenderMarkdown : String
failedToRenderMarkdown =
    "Failed to render Markdown"


failedToParseMarkdown : String
failedToParseMarkdown =
    "Failed to parse Markdown"


unknownTheme : String
unknownTheme =
    "Unknown theme"


thisFileIsMeantToBeImportedIntoAnki : String
thisFileIsMeantToBeImportedIntoAnki =
    "This file is meant to be imported into Anki (https://docs.ankiweb.net/importing.html#text-files)."


elementNotFound : String
elementNotFound =
    "Element not found"


thisFieldCannotBeEmpty : String
thisFieldCannotBeEmpty =
    "This field can't be empty"


thisTermAlreadyExistsElsewhere : String
thisTermAlreadyExistsElsewhere =
    "This term already exists elsewhere. Please pick a different one or use a disambiguation tag."


thisTermOccursMultipleTimes : String
thisTermOccursMultipleTimes =
    "This term occurs multiple times"


thisAlternativeTermOccursMultipleTimes : String -> String -> String
thisAlternativeTermOccursMultipleTimes alternativeTerm_ preferredTerm_ =
    "The alternative term \"" ++ alternativeTerm_ ++ "\" occurs multiple times in the item with preferred term \"" ++ preferredTerm_ ++ "\"."


preferredTermCannotAlsoAppearAsAnAlternativeTerm : String -> String
preferredTermCannotAlsoAppearAsAnAlternativeTerm term_ =
    "A preferred term cannot also appear as an alternative term: \"" ++ term_ ++ "\""


thisTermIsReserved : String
thisTermIsReserved =
    "This term is reserved"


pleaseSelectAnItem : String
pleaseSelectAnItem =
    "Please select an item"


thisTagIsADuplicateOfAnEarlierOne : String
thisTagIsADuplicateOfAnEarlierOne =
    "This tag is a duplicate of an earlier one"


thereIsAlreadyATagWithId : String -> String
thereIsAlreadyATagWithId tagId =
    "There is already a tag with ID \"" ++ tagId ++ "\""


thereIsAlreadyATag : String -> String
thereIsAlreadyATag tag_ =
    "There is already a tag \"" ++ tag_ ++ "\""


thereIsNoTagWithId : String -> String
thereIsNoTagWithId tagId =
    "There is no tag with ID \"" ++ tagId ++ "\""


thereIsAlreadyAnItemWithId : String -> String
thereIsAlreadyAnItemWithId itemId =
    "There is already an item with ID \"" ++ itemId ++ "\""


thereIsNoItemWithId : String -> String
thereIsNoItemWithId itemId =
    "There is no item with ID \"" ++ itemId ++ "\""


thereIsAlreadyAnItemWithDisambiguatedPreferredTermId : String -> String
thereIsAlreadyAnItemWithDisambiguatedPreferredTermId fragmentIdentifier =
    "there is already an item with (disambiguated) preferred term identifier \""
        ++ fragmentIdentifier
        ++ "\""


useSetting : String
useSetting =
    "Use setting"


dragOrUseUpAndDownArrowsToMoveTerm : String
dragOrUseUpAndDownArrowsToMoveTerm =
    "Drag or use up/down arrows to move term"


close : String
close =
    "Close"


startingItemForNewLayout : String
startingItemForNewLayout =
    "Starting item (for new layout)"


switchBackToOldLayout : String
switchBackToOldLayout =
    "Switch back to old layout"


filter : String
filter =
    "Filter"
