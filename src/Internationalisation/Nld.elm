module Internationalisation.Nld exposing (about, addLinkButton, alphabetically, ankiDeck, areYouSureYouWantToDeleteThisItem, backToTop, builtUsingGlossaryPageTemplate, cancel, cardWidth, cardWidthCompact, cardWidthIntermediate, cardWidthWide, createANewGlossaryItem, delete, deleteItem, edit, editTitleAndAboutSectionButton, editTitleAndAboutSectionHeading, explanationForFocusedOn, explanationForMostMentionedFirst, export, focusedOn, howToMakeChangesTitle, makeChanges, manageTags, mostMentionedFirst, noResultsFound, onlyShowingItemsForTag, orderItems, pleaseSelect, quickSearch, runTheFollowingCommand, sandboxModeMessage, searchPlaceholder, see, seeAlso, settings, showExportMenu, showLastUpdatedDates, showOrderItemsButtons, tags, textLabel, themeDark, themeLight, themeSystem, title, updatedOn, url, webInterfaceDescription, youCanHideTheseInstructions)

{-| User interface text in the Dutch language.
-}

import Accessibility.Key
import ElementIds
import Extras.Html
import Html exposing (Html, a, code, div, p, span, text)
import Html.Attributes exposing (class, href)


sandboxModeMessage : String
sandboxModeMessage =
    "[Zandbak-modus — veranderingen gaan verloren als je de pagina ververst]"


backToTop : String
backToTop =
    "Terug naar boven"


quickSearch : String
quickSearch =
    "Snel zoeken..."


searchPlaceholder : String
searchPlaceholder =
    "Zoek..."


noResultsFound : String
noResultsFound =
    "Geen resultaten gevonden."


export : String
export =
    "Exporteer"


ankiDeck : String
ankiDeck =
    "Anki deck"


themeLight : String
themeLight =
    "Licht"


themeDark : String
themeDark =
    "Donker"


themeSystem : String
themeSystem =
    "Systeem"


howToMakeChangesTitle : String
howToMakeChangesTitle =
    "Hoe je wijzigingen kunt maken"


webInterfaceDescription : Html msg
webInterfaceDescription =
    p
        [ class "mt-3" ]
        [ text "Deze pagina bevat een web-interface voor het maken van veranderingen die worden teruggeschreven naar het HTML-bestand zelf."
        , text " Deze is bedoeld om "
        , span [ class "font-semibold" ] [ text "lokaal" ]
        , text " te worden gebruikt door "
        , span [ class "font-semibold" ] [ text "één gebruiker" ]
        , text " tegelijkertijd en werkt het beste als het bestand onder version control wordt bewaard."
        ]


runTheFollowingCommand : Bool -> Html msg
runTheFollowingCommand tabbable =
    p []
        [ text "Als je macOS, Linux, of Cygwin gebruikt en "
        , a
            [ href "https://nodejs.org/"
            , Html.Attributes.target "_blank"
            , Accessibility.Key.tabbable tabbable
            ]
            [ text "Node.js" ]
        , text " geïnstalleerd hebt, geef dan het volgende commando."
        ]


youCanHideTheseInstructions : Html msg
youCanHideTheseInstructions =
    p
        [ class "mt-3 max-w-xl" ]
        [ text "Je kunt deze instructies geheel verbergen door het attribuut "
        , Extras.Html.inlineCode "data-enable-help-for-making-changes"
        , text " op "
        , Extras.Html.inlineCode "false"
        , text " te zetten op het "
        , code [] [ text <| "<div id=\"" ++ ElementIds.container ++ "\">" ]
        , text " -element."
        ]


tags : String
tags =
    "Tags"


onlyShowingItemsForTag : String
onlyShowingItemsForTag =
    "Alleen items voor deze tag worden getoond:"


orderItems : String
orderItems =
    "Sorteer items"


alphabetically : String
alphabetically =
    "alfabetisch"


mostMentionedFirst : String
mostMentionedFirst =
    "meest genoemd eerst"


focusedOn : String
focusedOn =
    "gefocused op"


pleaseSelect : String
pleaseSelect =
    "Selecteer a.u.b."


builtUsingGlossaryPageTemplate : Bool -> Html msg
builtUsingGlossaryPageTemplate tabbable =
    p []
        [ text "Gemaakt met "
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
        [ text "Bijgewerkt: "
        , Html.node "last-updated"
            [ Html.Attributes.attribute "datetime" date ]
            []
        ]


makeChanges : String
makeChanges =
    "Wijzig"


settings : String
settings =
    "Instellingen"


seeAlso : String
seeAlso =
    "Zie ook"


see : String
see =
    "Zie"


explanationForMostMentionedFirst : String
explanationForMostMentionedFirst =
    "Items die in veel andere items worden genoemd verschijnen eerder."


explanationForFocusedOn : Html msg -> Html msg
explanationForFocusedOn termHtml =
    p
        [ class "mt-2 text-gray-700 dark:text-gray-300" ]
        [ text "Items die nauw verwant zijn aan \""
        , termHtml
        , text "\" worden eerst getoond. Dit wordt bepaald door \"Zie ook\"-koppelingen."
        ]


cardWidth : String
cardWidth =
    "Breedte van kaarten"


cardWidthCompact : String
cardWidthCompact =
    "Compact"


cardWidthIntermediate : String
cardWidthIntermediate =
    "Gemiddeld"


cardWidthWide : String
cardWidthWide =
    "Breed"


showExportMenu : String
showExportMenu =
    "Toon \"Exporteer\"-menu"


showOrderItemsButtons : String
showOrderItemsButtons =
    "Toon knoppen voor het sorteren van items"


showLastUpdatedDates : String
showLastUpdatedDates =
    "Toon laatste bewerkingsdatum voor ieder item"


editTitleAndAboutSectionButton : String
editTitleAndAboutSectionButton =
    "Wijzig titel en algemene informatie"


editTitleAndAboutSectionHeading : String
editTitleAndAboutSectionHeading =
    "Wijzig titel en algemene informatie"


createANewGlossaryItem : String
createANewGlossaryItem =
    "Maak een nieuw item aan"


manageTags : String
manageTags =
    "Beheer tags"


edit : String
edit =
    "Wijzig"


delete : String
delete =
    "Verwijder"


deleteItem : String
deleteItem =
    "Verwijder item"


areYouSureYouWantToDeleteThisItem : String
areYouSureYouWantToDeleteThisItem =
    "Weet je zeker dat je dit item wil verwijderen?"


cancel : String
cancel =
    "Annuleer"


title : String
title =
    "Titel"


about : String
about =
    "Algemene informatie"


addLinkButton : String
addLinkButton =
    "Voeg link toe"


url : String
url =
    "URL"


textLabel : String
textLabel =
    "Tekst"
