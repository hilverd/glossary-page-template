module Internationalisation.Eng exposing (alphabetically, ankiDeck, backToTop, builtUsingGlossaryPageTemplate, cardWidth, cardWidthCompact, cardWidthIntermediate, cardWidthWide, createANewGlossaryItem, delete, edit, editTitleAndAboutSection, explanationForFocusedOn, explanationForMostMentionedFirst, export, focusedOn, howToMakeChangesTitle, makeChanges, manageTags, mostMentionedFirst, noResultsFound, onlyShowingItemsForTag, orderItems, pleaseSelect, quickSearch, runTheFollowingCommand, sandboxModeMessage, searchPlaceholder, see, seeAlso, settings, showExportMenu, showLastUpdatedDates, showOrderItemsButtons, tags, themeDark, themeLight, themeSystem, updatedOn, webInterfaceDescription, youCanHideTheseInstructions)

{-| User interface text in the English language.
-}

import Accessibility.Key
import ElementIds
import Extras.Html
import Html exposing (Html, a, code, div, p, span, text)
import Html.Attributes exposing (class, href)


sandboxModeMessage : String
sandboxModeMessage =
    "[Sandbox mode — changes are lost when you reload the page]"


backToTop : String
backToTop =
    "Back to top"


quickSearch : String
quickSearch =
    "Quick search..."


searchPlaceholder : String
searchPlaceholder =
    "Search..."


noResultsFound : String
noResultsFound =
    "No results found."


export : String
export =
    "Export"


ankiDeck : String
ankiDeck =
    "Anki deck"


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
        [ class "mt-3 max-w-xl" ]
        [ text "You can hide these instructions altogether by setting the "
        , Extras.Html.inlineCode "data-enable-help-for-making-changes"
        , text " attribute to "
        , Extras.Html.inlineCode "false"
        , text " on the "
        , code [] [ text <| "<div id=\"" ++ ElementIds.container ++ "\">" ]
        , text " element."
        ]


tags : String
tags =
    "Tags"


onlyShowingItemsForTag : String
onlyShowingItemsForTag =
    "Only showing items for tag:"


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


updatedOn : String -> Html msg
updatedOn date =
    div
        [ class "text-right text-sm mt-1.5 mb-2.5 text-gray-500 dark:text-gray-400" ]
        [ text "Updated: "
        , Html.node "last-updated"
            [ Html.Attributes.attribute "datetime" date ]
            []
        ]


makeChanges : String
makeChanges =
    "Make changes"


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


editTitleAndAboutSection : String
editTitleAndAboutSection =
    "Edit title and about section"


createANewGlossaryItem : String
createANewGlossaryItem =
    "Create a new glossary item"


manageTags : String
manageTags =
    "Manage tags"


edit : String
edit =
    "Edit"


delete : String
delete =
    "Delete"
