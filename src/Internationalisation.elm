module Internationalisation exposing (alphabetically, ankiDeck, backToTop, builtUsingGlossaryPageTemplate, cardWidth, cardWidthCompact, cardWidthIntermediate, cardWidthWide, explanationForFocusedOn, explanationForMostMentionedFirst, export, focusedOn, howToMakeChangesTitle, makeChanges, mostMentionedFirst, noResultsFound, onlyShowingItemsForTag, orderItems, pleaseSelect, quickSearch, runTheFollowingCommand, sandboxModeMessage, searchPlaceholder, see, seeAlso, settings, showExportMenu, showLastUpdatedDates, showOrderItemsButtons, tags, themeDark, themeLight, themeSystem, updatedOn, webInterfaceDescription, youCanHideTheseInstructions)

{-| User interface text in a specific language (e.g. English).
-}

import Html exposing (Html)
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
