module Internationalisation exposing (abbreviation, about, addLinkButton, addRelatedItem, addTagButton, addTermButton, alphabetically, alternativeTerm, ankiDeck, areYouSureYouWantToDeleteThisItem, backToTop, builtUsingGlossaryPageTemplate, builtUsingGlossaryPageTemplateHtmlTree, cancel, cardWidth, cardWidthCompact, cardWidthIntermediate, cardWidthWide, chooseWhichTagShouldBeUsedToDistinguishThisItem, closeSidebar, commandK, controlK, copyToClipboard, createANewGlossaryItem, createANewGlossaryItemCapitalised, defaultTheme, definition, delete, deleteItem, description, disambiguationTag, disambiguationTagOptional, dragOrUseUpAndDownArrowsToMoveTerm, edit, editGlossaryItemCapitalised, editTitleAndAboutSectionButton, editTitleAndAboutSectionHeading, elementNotFound, example, explanationForFocusedOn, explanationForMostMentionedFirst, export, failedToParseMarkdown, failedToRenderMarkdown, failedToSave, filteringByTag, focusOnTerm, focusedOn, glossaryCapitalised, glossaryContainsTooManyItems, howToEnableMathSupport, howToMakeChangesTitle, httpErrorDescription, json, links, listTheGroupOfTermsBeingDefined, loadingEllipsis, manageTags, manageTagsTitle, markdownAndTeXSupported, mathSupportIsEnabled, miscellaneous, mostMentionedFirst, moveDown, moveUp, needsUpdating, noMatchingItemsFound, noResultsFound, none, openOptions, openSidebar, orderItems, otherChangesWereMadePleaseReload, otherItems, pleaseSelect, pleaseSelectAnItem, pointToAnyRelatedItems, preferredTerm, preferredTermCannotAlsoAppearAsAnAlternativeTerm, preview, provideADefinitionForThisGroupOfTerms, readMore, relatedItem, relatedItems, runTheFollowingCommand, save, savingChangesInMemoryMessage, search, searchPlaceholder, see, seeAlso, selectAllTagsThatApplyToThisItem, settings, showExportMenu, showLastUpdatedDates, showOrderItemsButtons, somethingWentWrong, startEditing, suggestions, tag, tagAppearsMultipleTimes, tags, term, terms, textLabel, themeDark, themeLight, themeSystem, thereAreErrorsOnThisFormSeeAbove, thereAreMultipleItemsWithDisambiguatedPreferredTerm, thereAreMultipleItemsWithTheSameDisambiguatedPreferredTerm, thereIsAlreadyATag, thereIsAlreadyATagWithId, thereIsAlreadyAnItemWithDisambiguatedPreferredTermId, thereIsAlreadyAnItemWithId, thereIsNoItemWithId, thereIsNoTagWithId, theseSettingsAreUpdatedInTheHtmlFile, thisAlternativeTermOccursMultipleTimes, thisFieldCannotBeEmpty, thisFileIsMeantToBeImportedIntoAnki, thisTagIsADuplicateOfAnEarlierOne, thisTermAlreadyExistsElsewhere, thisTermIsReserved, thisTermOccursMultipleTimes, title, unableToSaveAsItWouldResultInTheFollowing, unauthorisedPleaseReload, unknownTheme, updatedOn, url, useSetting, viewAsSingleItem, webInterfaceDescription, whyTagsMayBeUseful, youCanHideTheseInstructions, youCanUseTagsToAttachLabels)

{-| User interface text in a specific language (e.g. English).
-}

import Extras.HtmlTree as HtmlTree
import Html exposing (Html)
import Http
import Internationalisation.Eng as I18n


{-| A message indicating to the user that the application is running in sandbox mode.
-}
savingChangesInMemoryMessage : String
savingChangesInMemoryMessage =
    I18n.showSavingChangesInMemoryMessage


{-| The text for a link that takes the user back to the top of the page.
-}
backToTop : String
backToTop =
    I18n.backToTop


search : String
search =
    I18n.search


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


json : String
json =
    I18n.json


{-| The default colour theme for the web site.
-}
defaultTheme : String
defaultTheme =
    I18n.defaultTheme


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


tag : String
tag =
    I18n.tag


{-| Tags.
-}
tags : String
tags =
    I18n.tags


{-| An indication that only items for the following tag are shown.
-}
filteringByTag : String
filteringByTag =
    I18n.filteringByTag


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


updatedOn : Maybe String -> Maybe String -> String -> Html msg
updatedOn =
    I18n.updatedOn


startEditing : String
startEditing =
    I18n.startEditing


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


manageTagsTitle : String
manageTagsTitle =
    I18n.manageTagsTitle


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


controlK : String
controlK =
    I18n.controlK


commandK : String
commandK =
    I18n.commandK


theseSettingsAreUpdatedInTheHtmlFile : String
theseSettingsAreUpdatedInTheHtmlFile =
    I18n.theseSettingsAreUpdatedInTheHtmlFile


failedToSave : String
failedToSave =
    I18n.failedToSave


unauthorisedPleaseReload : String
unauthorisedPleaseReload =
    I18n.unauthorisedPleaseReload


otherChangesWereMadePleaseReload : String
otherChangesWereMadePleaseReload =
    I18n.otherChangesWereMadePleaseReload


httpErrorDescription : Http.Error -> String
httpErrorDescription =
    I18n.httpErrorDescription


howToEnableMathSupport : Html msg
howToEnableMathSupport =
    I18n.howToEnableMathSupport


mathSupportIsEnabled : Html msg
mathSupportIsEnabled =
    I18n.mathSupportIsEnabled


copyToClipboard : String
copyToClipboard =
    I18n.copyToClipboard


focusOnTerm : String
focusOnTerm =
    I18n.focusOnTerm


glossaryCapitalised : String
glossaryCapitalised =
    I18n.glossaryCapitalised


otherItems : String
otherItems =
    I18n.otherItems


description : String
description =
    I18n.description


addTagButton : String
addTagButton =
    I18n.addTagButton


unableToSaveAsItWouldResultInTheFollowing : String
unableToSaveAsItWouldResultInTheFollowing =
    I18n.unableToSaveAsItWouldResultInTheFollowing


thereAreMultipleItemsWithDisambiguatedPreferredTerm : String -> String
thereAreMultipleItemsWithDisambiguatedPreferredTerm =
    I18n.thereAreMultipleItemsWithDisambiguatedPreferredTerm


thereAreMultipleItemsWithTheSameDisambiguatedPreferredTerm : String
thereAreMultipleItemsWithTheSameDisambiguatedPreferredTerm =
    I18n.thereAreMultipleItemsWithTheSameDisambiguatedPreferredTerm


tagAppearsMultipleTimes : String -> String
tagAppearsMultipleTimes =
    I18n.tagAppearsMultipleTimes


youCanUseTagsToAttachLabels : String
youCanUseTagsToAttachLabels =
    I18n.youCanUseTagsToAttachLabels


readMore : String
readMore =
    I18n.readMore


whyTagsMayBeUseful : String
whyTagsMayBeUseful =
    I18n.whyTagsMayBeUseful


example : String
example =
    I18n.example


abbreviation : String
abbreviation =
    I18n.abbreviation


terms : String
terms =
    I18n.terms


listTheGroupOfTermsBeingDefined : List (Html msg)
listTheGroupOfTermsBeingDefined =
    I18n.listTheGroupOfTermsBeingDefined


addTermButton : String
addTermButton =
    I18n.addTermButton


definition : String
definition =
    I18n.definition


provideADefinitionForThisGroupOfTerms : String
provideADefinitionForThisGroupOfTerms =
    I18n.provideADefinitionForThisGroupOfTerms


selectAllTagsThatApplyToThisItem : String
selectAllTagsThatApplyToThisItem =
    I18n.selectAllTagsThatApplyToThisItem


disambiguationTagOptional : String
disambiguationTagOptional =
    I18n.disambiguationTagOptional


disambiguationTag : String
disambiguationTag =
    I18n.disambiguationTag


none : String
none =
    I18n.none


chooseWhichTagShouldBeUsedToDistinguishThisItem : String
chooseWhichTagShouldBeUsedToDistinguishThisItem =
    I18n.chooseWhichTagShouldBeUsedToDistinguishThisItem


relatedItem : String
relatedItem =
    I18n.relatedItem


moveUp : String
moveUp =
    I18n.moveUp


moveDown : String
moveDown =
    I18n.moveDown


addRelatedItem : String
addRelatedItem =
    I18n.addRelatedItem


relatedItems : String
relatedItems =
    I18n.relatedItems


pointToAnyRelatedItems : String
pointToAnyRelatedItems =
    I18n.pointToAnyRelatedItems


suggestions : String
suggestions =
    I18n.suggestions


miscellaneous : String
miscellaneous =
    I18n.miscellaneous


needsUpdating : String
needsUpdating =
    I18n.needsUpdating


createANewGlossaryItemCapitalised : String
createANewGlossaryItemCapitalised =
    I18n.createANewGlossaryItemCapitalised


editGlossaryItemCapitalised : String
editGlossaryItemCapitalised =
    I18n.editGlossaryItemCapitalised


preferredTerm : String
preferredTerm =
    I18n.preferredTerm


alternativeTerm : String
alternativeTerm =
    I18n.alternativeTerm


term : String
term =
    I18n.term


builtUsingGlossaryPageTemplateHtmlTree : List HtmlTree.HtmlTree
builtUsingGlossaryPageTemplateHtmlTree =
    I18n.builtUsingGlossaryPageTemplateHtmlTree


openOptions : String
openOptions =
    I18n.openOptions


viewAsSingleItem : String
viewAsSingleItem =
    I18n.viewAsSingleItem


loadingEllipsis : String
loadingEllipsis =
    I18n.loadingEllipsis


failedToRenderMarkdown : String
failedToRenderMarkdown =
    I18n.failedToRenderMarkdown


failedToParseMarkdown : String
failedToParseMarkdown =
    I18n.failedToParseMarkdown


unknownTheme : String
unknownTheme =
    I18n.unknownTheme


thisFileIsMeantToBeImportedIntoAnki : String
thisFileIsMeantToBeImportedIntoAnki =
    I18n.thisFileIsMeantToBeImportedIntoAnki


elementNotFound : String
elementNotFound =
    I18n.elementNotFound


thisFieldCannotBeEmpty : String
thisFieldCannotBeEmpty =
    I18n.thisFieldCannotBeEmpty


thisTermAlreadyExistsElsewhere : String
thisTermAlreadyExistsElsewhere =
    I18n.thisTermAlreadyExistsElsewhere


thisTermOccursMultipleTimes : String
thisTermOccursMultipleTimes =
    I18n.thisTermOccursMultipleTimes


thisAlternativeTermOccursMultipleTimes : String -> String -> String
thisAlternativeTermOccursMultipleTimes =
    I18n.thisAlternativeTermOccursMultipleTimes


preferredTermCannotAlsoAppearAsAnAlternativeTerm : String -> String
preferredTermCannotAlsoAppearAsAnAlternativeTerm =
    I18n.preferredTermCannotAlsoAppearAsAnAlternativeTerm


thisTermIsReserved : String
thisTermIsReserved =
    I18n.thisTermIsReserved


pleaseSelectAnItem : String
pleaseSelectAnItem =
    I18n.pleaseSelectAnItem


thisTagIsADuplicateOfAnEarlierOne : String
thisTagIsADuplicateOfAnEarlierOne =
    I18n.thisTagIsADuplicateOfAnEarlierOne


thereIsAlreadyATagWithId : String -> String
thereIsAlreadyATagWithId =
    I18n.thereIsAlreadyATagWithId


thereIsAlreadyATag : String -> String
thereIsAlreadyATag =
    I18n.thereIsAlreadyATag


thereIsNoTagWithId : String -> String
thereIsNoTagWithId =
    I18n.thereIsNoTagWithId


thereIsAlreadyAnItemWithId : String -> String
thereIsAlreadyAnItemWithId =
    I18n.thereIsAlreadyAnItemWithId


thereIsNoItemWithId : String -> String
thereIsNoItemWithId =
    I18n.thereIsNoItemWithId


thereIsAlreadyAnItemWithDisambiguatedPreferredTermId : String -> String
thereIsAlreadyAnItemWithDisambiguatedPreferredTermId =
    I18n.thereIsAlreadyAnItemWithDisambiguatedPreferredTermId


useSetting : String
useSetting =
    I18n.useSetting


dragOrUseUpAndDownArrowsToMoveTerm : String
dragOrUseUpAndDownArrowsToMoveTerm =
    I18n.dragOrUseUpAndDownArrowsToMoveTerm
