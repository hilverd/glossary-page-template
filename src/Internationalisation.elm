module Internationalisation exposing (about, addLinkButton, alphabetically, ankiDeck, areYouSureYouWantToDeleteThisItem, backToTop, builtUsingGlossaryPageTemplate, cancel, cardWidth, cardWidthCompact, cardWidthIntermediate, cardWidthWide, closeSidebar, createANewGlossaryItem, ctrlK, delete, deleteItem, edit, editTitleAndAboutSectionButton, editTitleAndAboutSectionHeading, explanationForFocusedOn, explanationForMostMentionedFirst, export, failedToSave, focusedOn, glossaryContainsTooManyItems, howToEnableMathSupport, howToMakeChangesTitle, httpErrorDescription, links, makeChanges, manageTags, markdownAndTeXSupported, mathSupportIsEnabled, mostMentionedFirst, noMatchingItemsFound, noResultsFound, onlyShowingItemsForTag, openSidebar, orderItems, pleaseSelect, preview, quickSearch, runTheFollowingCommand, sandboxModeMessage, save, search, searchPlaceholder, see, seeAlso, settings, showExportMenu, showLastUpdatedDates, showOrderItemsButtons, somethingWentWrong, tags, textLabel, themeDark, themeLight, themeSystem, thereAreErrorsOnThisFormSeeAbove, theseSettingsAreUpdatedInTheHtmlFile, title, updatedOn, url, webInterfaceDescription, youCanHideTheseInstructions)

{-| User interface text in a specific language (e.g. English).
-}

import Html exposing (Html)
import Http
import Internationalisation.Eng as I18n


{-| A message indicating to the user that the application is running in sandbox mode.
-}
sandboxModeMessage : String
sandboxModeMessage =
    I18n.sandboxModeMessage


{-| The text for a link that takes the user back to the top of the page.
-}
backToTop : String
backToTop =
    I18n.backToTop


search : String
search =
    I18n.search


{-| The text for a button that opens a "quick search" modal dialog.
-}
quickSearch : String
quickSearch =
    I18n.quickSearch


{-| The placeholder text for a search input field.
-}
searchPlaceholder : String
searchPlaceholder =
    I18n.searchPlaceholder


{-| A message indicating that no (search) results were found.
-}
noResultsFound : String
noResultsFound =
    I18n.noResultsFound


{-| The name of a menu for exporting to different formats.
-}
export : String
export =
    I18n.export


{-| A deck of flash cards for Anki.
-}
ankiDeck : String
ankiDeck =
    I18n.ankiDeck


{-| A light theme.
-}
themeLight : String
themeLight =
    I18n.themeLight


{-| A dark theme.
-}
themeDark : String
themeDark =
    I18n.themeDark


{-| The system theme.
-}
themeSystem : String
themeSystem =
    I18n.themeSystem


{-| The title of a section describing how to make changes to the glossary.
-}
howToMakeChangesTitle : String
howToMakeChangesTitle =
    I18n.howToMakeChangesTitle


{-| A description of how the web interface works.
-}
webInterfaceDescription : Html msg
webInterfaceDescription =
    I18n.webInterfaceDescription


{-| A note saying what command to run to make changes to the glossary.
-}
runTheFollowingCommand : Bool -> Html msg
runTheFollowingCommand =
    I18n.runTheFollowingCommand


{-| A note explaining how to hide the instructions for making changes to the glossary.
-}
youCanHideTheseInstructions : Html msg
youCanHideTheseInstructions =
    I18n.youCanHideTheseInstructions


{-| Tags.
-}
tags : String
tags =
    I18n.tags


{-| An indication that only items for the following tag are shown.
-}
onlyShowingItemsForTag : String
onlyShowingItemsForTag =
    I18n.onlyShowingItemsForTag


{-| A label for how to order items.
-}
orderItems : String
orderItems =
    I18n.orderItems


alphabetically : String
alphabetically =
    I18n.alphabetically


mostMentionedFirst : String
mostMentionedFirst =
    I18n.mostMentionedFirst


focusedOn : String
focusedOn =
    I18n.focusedOn


pleaseSelect : String
pleaseSelect =
    I18n.pleaseSelect


builtUsingGlossaryPageTemplate : Bool -> Html msg
builtUsingGlossaryPageTemplate =
    I18n.builtUsingGlossaryPageTemplate


updatedOn : String -> Html msg
updatedOn =
    I18n.updatedOn


makeChanges : String
makeChanges =
    I18n.makeChanges


settings : String
settings =
    I18n.settings


seeAlso : String
seeAlso =
    I18n.seeAlso


see : String
see =
    I18n.see


explanationForMostMentionedFirst : String
explanationForMostMentionedFirst =
    I18n.explanationForMostMentionedFirst


explanationForFocusedOn : Html msg -> Html msg
explanationForFocusedOn =
    I18n.explanationForFocusedOn


cardWidth : String
cardWidth =
    I18n.cardWidth


cardWidthCompact : String
cardWidthCompact =
    I18n.cardWidthCompact


cardWidthIntermediate : String
cardWidthIntermediate =
    I18n.cardWidthIntermediate


cardWidthWide : String
cardWidthWide =
    I18n.cardWidthWide


showExportMenu : String
showExportMenu =
    I18n.showExportMenu


showOrderItemsButtons : String
showOrderItemsButtons =
    I18n.showOrderItemsButtons


showLastUpdatedDates : String
showLastUpdatedDates =
    I18n.showLastUpdatedDates


editTitleAndAboutSectionButton : String
editTitleAndAboutSectionButton =
    I18n.editTitleAndAboutSectionButton


editTitleAndAboutSectionHeading : String
editTitleAndAboutSectionHeading =
    I18n.editTitleAndAboutSectionHeading


createANewGlossaryItem : String
createANewGlossaryItem =
    I18n.createANewGlossaryItem


manageTags : String
manageTags =
    I18n.manageTags


edit : String
edit =
    I18n.edit


delete : String
delete =
    I18n.delete


deleteItem : String
deleteItem =
    I18n.deleteItem


areYouSureYouWantToDeleteThisItem : String
areYouSureYouWantToDeleteThisItem =
    I18n.areYouSureYouWantToDeleteThisItem


cancel : String
cancel =
    I18n.cancel


title : String
title =
    I18n.title


about : String
about =
    I18n.about


addLinkButton : String
addLinkButton =
    I18n.addLinkButton


url : String
url =
    I18n.url


textLabel : String
textLabel =
    I18n.textLabel


markdownAndTeXSupported : Bool -> Html msg
markdownAndTeXSupported =
    I18n.markdownAndTeXSupported


preview : String
preview =
    I18n.preview


save : String
save =
    I18n.save


thereAreErrorsOnThisFormSeeAbove : String
thereAreErrorsOnThisFormSeeAbove =
    I18n.thereAreErrorsOnThisFormSeeAbove


somethingWentWrong : String
somethingWentWrong =
    I18n.somethingWentWrong


links : String
links =
    I18n.links


glossaryContainsTooManyItems : Int -> Html msg
glossaryContainsTooManyItems =
    I18n.glossaryContainsTooManyItems


noMatchingItemsFound : String
noMatchingItemsFound =
    I18n.noMatchingItemsFound


openSidebar : String
openSidebar =
    I18n.openSidebar


closeSidebar : String
closeSidebar =
    I18n.closeSidebar


ctrlK : String
ctrlK =
    I18n.ctrlK


theseSettingsAreUpdatedInTheHtmlFile : String
theseSettingsAreUpdatedInTheHtmlFile =
    I18n.theseSettingsAreUpdatedInTheHtmlFile


failedToSave : String
failedToSave =
    I18n.failedToSave


httpErrorDescription : Http.Error -> String
httpErrorDescription =
    I18n.httpErrorDescription


howToEnableMathSupport : Html msg
howToEnableMathSupport =
    I18n.howToEnableMathSupport


mathSupportIsEnabled : Html msg
mathSupportIsEnabled =
    I18n.mathSupportIsEnabled
