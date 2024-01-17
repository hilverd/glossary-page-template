module Internationalisation.Nld exposing (abbreviation, about, addLinkButton, addRelatedItem, addTagButton, addTermButton, alphabetically, alternativeTerm, ankiDeck, areYouSureYouWantToDeleteThisItem, backToTop, builtUsingGlossaryPageTemplate, builtUsingGlossaryPageTemplateHtmlTree, cancel, cardWidth, cardWidthCompact, cardWidthIntermediate, cardWidthWide, chooseWhichTagShouldBeUsedToDistinguishThisItem, closeSidebar, copyToClipboard, createANewGlossaryItem, createANewGlossaryItemCapitalised, ctrlK, definition, delete, deleteItem, description, disambiguationTag, disambiguationTagOptional, edit, editGlossaryItemCapitalised, editTitleAndAboutSectionButton, editTitleAndAboutSectionHeading, elementNotFound, example, explanationForFocusedOn, explanationForMostMentionedFirst, export, failedToParseMarkdown, failedToRenderMarkdown, failedToSave, focusOnTerm, focusedOn, glossaryCapitalised, glossaryContainsTooManyItems, howToEnableMathSupport, howToMakeChangesTitle, httpErrorDescription, json, links, listTheGroupOfTermsBeingDefined, loadingEllipsis, makeChanges, manageTags, manageTagsTitle, markdownAndTeXSupported, mathSupportIsEnabled, miscellaneous, mostMentionedFirst, moveDown, moveUp, needsUpdating, noMatchingItemsFound, noResultsFound, none, onlyShowingItemsForTag, openOptions, openSidebar, orderItems, otherChangesWereMadePleaseReload, otherItems, pleaseSelect, pleaseSelectAnItem, pointToAnyRelatedItems, preferredTerm, preview, provideADefinitionForThisGroupOfTerms, quickSearch, readMore, relatedItem, relatedItems, runTheFollowingCommand, save, search, searchPlaceholder, see, seeAlso, selectAllTagsThatApplyToThisItem, settings, showExportMenu, showLastUpdatedDates, showOrderItemsButtons, showSavingChangesInMemoryMessage, somethingWentWrong, suggestions, tag, tagAppearsMultipleTimes, tags, term, terms, textLabel, themeDark, themeLight, themeSystem, thereAreErrorsOnThisFormSeeAbove, thereAreMultipleItemsWithDisambiguatedPreferredTerm, thereAreMultipleItemsWithTheSameDisambiguatedPreferredTerm, theseSettingsAreUpdatedInTheHtmlFile, thisFieldCannotBeEmpty, thisFileIsMeantToBeImportedIntoAnki, thisTagIsADuplicateOfAnEarlierOne, thisTermAlreadyExistsElsewhere, thisTermIsReserved, thisTermOccursMultipleTimes, title, unableToSaveAsItWouldResultInTheFollowing, unauthorisedPleaseReload, unknownCardWidth, unknownTheme, updatedOn, url, viewAsSingleItem, webInterfaceDescription, whyTagsMayBeUseful, youCanHideTheseInstructions, youCanUseTagsToAttachLabels)

{-| User interface text in the Dutch language.
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
    "[Zandbak-modus — veranderingen gaan verloren als je de pagina ververst]"


backToTop : String
backToTop =
    "Terug naar boven"


search : String
search =
    "Zoek"


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


json : String
json =
    "JSON"


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


tag : String
tag =
    "Etiket"


tags : String
tags =
    "Etiketten"


onlyShowingItemsForTag : String
onlyShowingItemsForTag =
    "Alleen items voor dit etiket worden getoond:"


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


builtUsingGlossaryPageTemplateHtmlTree : List HtmlTree.HtmlTree
builtUsingGlossaryPageTemplateHtmlTree =
    [ HtmlTree.Leaf "Gemaakt met "
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
        [ text "Bijgewerkt: "
        , Html.node "last-updated"
            [ Html.Attributes.attribute "datetime" date ]
            []
        , Extras.Html.showMaybe
            (\name_ ->
                span
                    []
                    [ text " door "
                    , span
                        [ Extras.HtmlAttribute.showMaybe Html.Attributes.title emailAddress ]
                        [ text name_ ]
                    ]
            )
            name
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
    "Beheer etiketten"


manageTagsTitle : String
manageTagsTitle =
    "Beheer etiketten"


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
                [ Html.text " en "
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
        , Html.text " ondersteund."
        ]


preview : String
preview =
    "Voorbeschouwing"


save : String
save =
    "Bewaar"


thereAreErrorsOnThisFormSeeAbove : String
thereAreErrorsOnThisFormSeeAbove =
    "Er bevinden zich fouten in dit formulier — zie boven."


somethingWentWrong : String
somethingWentWrong =
    "Er is iets misgegaan."


links : String
links =
    "Links"


glossaryContainsTooManyItems : Int -> Html msg
glossaryContainsTooManyItems recommendedMaximumNumberOfItems =
    div
        [ class "mt-4 text-red-600 dark:text-red-400 flex items-center max-w-prose" ]
        [ span
            [ class "font-medium" ]
            [ text "⚠ Deze woordenlijst bevat meer dan "
            , text <| String.fromInt recommendedMaximumNumberOfItems
            , text " items, en dat wordt momenteel "
            , a
                [ href "https://github.com/hilverd/glossary-page-template#known-limitations" ]
                [ text "afgeraden" ]
            , text " vanwege traagheid."
            ]
        ]


noMatchingItemsFound : String
noMatchingItemsFound =
    "Geen passende items gevonden."


openSidebar : String
openSidebar =
    "Open paneel"


closeSidebar : String
closeSidebar =
    "Sluit paneel"


ctrlK : String
ctrlK =
    "Ctrl K"


theseSettingsAreUpdatedInTheHtmlFile : String
theseSettingsAreUpdatedInTheHtmlFile =
    "Als je deze instellingen verandert dan worden ze bewaard in het HTML-bestand en wordt de pagina opnieuw geladen."


failedToSave : String
failedToSave =
    "Bewaren mislukt"


unauthorisedPleaseReload : String
unauthorisedPleaseReload =
    "niet geautoriseerd, laad de pagina a.u.b. opnieuw en probeer het nogmaals"


otherChangesWereMadePleaseReload : String
otherChangesWereMadePleaseReload =
    "er zijn andere wijzigingen aangebracht, laad de pagina a.u.b. opnieuw en probeer het nogmaals"


httpErrorDescription : Http.Error -> String
httpErrorDescription error =
    case error of
        Http.BadUrl urlString ->
            "verkeerde URL: " ++ urlString

        Http.Timeout ->
            "het verzoek is verlopen"

        Http.NetworkError ->
            "er is een netwerkfout opgetreden"

        Http.BadStatus statusCode ->
            "onverwachte statuscode " ++ String.fromInt statusCode

        Http.BadBody body ->
            "onverwachte inhoud van antwoord: " ++ body


howToEnableMathSupport : Html msg
howToEnableMathSupport =
    div
        [ class "mt-2 max-w-prose" ]
        [ text "Om ondersteuning voor wiskundige notatie toe te voegen, voeg je het stylesheet en script van KaTeX toe aan het "
        , code [] [ text "<head>" ]
        , text "-element zoals getoond in het "
        , a
            [ class "font-semibold"
            , href "https://github.com/hilverd/glossary-page-template/releases/latest/download/glossary.html"
            , Html.Attributes.download "glossary.html"
            ]
            [ text "glossary.html" ]
        , text "-sjabloon."
        ]


mathSupportIsEnabled : Html msg
mathSupportIsEnabled =
    div
        [ class "mt-2 max-w-prose" ]
        [ text "Ondersteuning voor wiskundige notatie is ingeschakeld. Binnen een regel wordt dit geschreven als"
        , pre
            [ class "mt-4" ]
            [ code
                []
                [ text "`$e = mc^2$`" ]
            ]
        , p [ class "mt-4" ] [ text "en in een aparte alinea zoals hieronder:" ]
        , pre
            [ class "mt-4" ]
            [ code
                []
                [ text "```math\ne = mc^2\n```" ]
            ]
        ]


copyToClipboard : String
copyToClipboard =
    "Kopieer naar klembord"


focusOnTerm : String
focusOnTerm =
    "Focus op term"


glossaryCapitalised : String
glossaryCapitalised =
    "Woordenlijst"


otherItems : String
otherItems =
    "Andere items"


description : String
description =
    "Beschrijving"


addTagButton : String
addTagButton =
    "Voeg etiket toe"


unableToSaveAsItWouldResultInTheFollowing : String
unableToSaveAsItWouldResultInTheFollowing =
    "Kan niet opslaan, omdat dit het volgende zou opleveren"


thereAreMultipleItemsWithDisambiguatedPreferredTerm : String -> String
thereAreMultipleItemsWithDisambiguatedPreferredTerm rawTerm =
    "er zijn meerdere items met (ondubbelzinnige) voorkeursterm \""
        ++ rawTerm
        ++ "\""


thereAreMultipleItemsWithTheSameDisambiguatedPreferredTerm : String
thereAreMultipleItemsWithTheSameDisambiguatedPreferredTerm =
    "er zijn meerdere items met dezelfde (ondubbelzinnige) voorkeursterm"


tagAppearsMultipleTimes : String -> String
tagAppearsMultipleTimes rawTag =
    "etiket \"" ++ rawTag ++ "\" komt meerdere keren voor"


youCanUseTagsToAttachLabels : String
youCanUseTagsToAttachLabels =
    "Je kunt etiketten aan items toevoegen, zodat mensen er vervolgens op kunnen filteren."


readMore : String
readMore =
    "Lees meer"


whyTagsMayBeUseful : String
whyTagsMayBeUseful =
    "Dit kan handig zijn voor grote woordenlijsten die meerdere onderwerpen bestrijken, waarbij het nodig is om items te categoriseren of te groeperen."
        ++ " Tags kunnen ook worden gebruikt om items \"ondubbelzinnig\" te maken die dezelfde voorkeursterm hebben, maar waarvan de betekenis afhangt van een bepaalde \"context\"."
        ++ " De term 'standaard' heeft bijvoorbeeld een andere betekenis in de context van muziek dan in de context van de informatica."


example : String
example =
    "Voorbeeld"


abbreviation : String
abbreviation =
    "Afkorting"


terms : String
terms =
    "Termen"


listTheGroupOfTermsBeingDefined : List (Html msg)
listTheGroupOfTermsBeingDefined =
    [ text "Maak een lijst van de groep termen die worden gedefinieerd. De eerste wordt beschouwd als de "
    , Html.em
        []
        [ text "voorkeursterm" ]
    , text "."
    ]


addTermButton : String
addTermButton =
    "Voeg term toe"


definition : String
definition =
    "Definitie"


provideADefinitionForThisGroupOfTerms : String
provideADefinitionForThisGroupOfTerms =
    "Geef een definitie (optioneel) voor deze groep termen."


selectAllTagsThatApplyToThisItem : String
selectAllTagsThatApplyToThisItem =
    "Selecteer alle etiketten die op dit item van toepassing zijn."


disambiguationTagOptional : String
disambiguationTagOptional =
    "Ondubbelzinnig makend etiket (optioneel)"


disambiguationTag : String
disambiguationTag =
    "Ondubbelzinnig makend etiket"


none : String
none =
    "Geen"


chooseWhichTagShouldBeUsedToDistinguishThisItem : String
chooseWhichTagShouldBeUsedToDistinguishThisItem =
    "Als een ander item dezelfde voorkeursterm heeft, kies dan welk etiket gebruikt moet worden om dit item te onderscheiden."


relatedItem : String
relatedItem =
    "Gerelateerd item"


moveUp : String
moveUp =
    "Verplaats naar boven"


moveDown : String
moveDown =
    "Verplaats naar beneden"


addRelatedItem : String
addRelatedItem =
    "Voeg gerelateerd item toe"


relatedItems : String
relatedItems =
    "Gerelateerde items"


pointToAnyRelatedItems : String
pointToAnyRelatedItems =
    "Verwijs naar alle gerelateerde items die de lezer mogelijk wil raadplegen."


suggestions : String
suggestions =
    "Suggesties"


miscellaneous : String
miscellaneous =
    "Overig"


needsUpdating : String
needsUpdating =
    "Moet worden bijgewerkt"


createANewGlossaryItemCapitalised : String
createANewGlossaryItemCapitalised =
    "Maak een nieuw woordenlijstitem"


editGlossaryItemCapitalised : String
editGlossaryItemCapitalised =
    "Wijzig woordenlijstitem"


preferredTerm : String
preferredTerm =
    "Voorkeursterm"


alternativeTerm : String
alternativeTerm =
    "Alternatieve term"


term : String
term =
    "Term"


openOptions : String
openOptions =
    "Open opties"


viewAsSingleItem : String
viewAsSingleItem =
    "Bekijk als enkel item"


loadingEllipsis : String
loadingEllipsis =
    "Bezig met laden..."


failedToRenderMarkdown : String
failedToRenderMarkdown =
    "Weergave van Markdown mislukt"


failedToParseMarkdown : String
failedToParseMarkdown =
    "Ontleden van Markdown mislukt"


unknownCardWidth : String
unknownCardWidth =
    "Onbekende kaartbreedte"


unknownTheme : String
unknownTheme =
    "Onbekend thema"


thisFileIsMeantToBeImportedIntoAnki : String
thisFileIsMeantToBeImportedIntoAnki =
    "Dit bestand is bedoeld om in Anki te worden geïmporteerd (https://docs.ankiweb.net/importing.html#text-files)."


elementNotFound : String
elementNotFound =
    "Element niet gevonden"


thisFieldCannotBeEmpty : String
thisFieldCannotBeEmpty =
    "Dit veld mag niet leeg zijn"


thisTermAlreadyExistsElsewhere : String
thisTermAlreadyExistsElsewhere =
    "Deze term bestaat elders al. Kies een andere of gebruik een ondubbelzinnig makend etiket."


thisTermOccursMultipleTimes : String
thisTermOccursMultipleTimes =
    "Deze term komt meerdere keren voor"


thisTermIsReserved : String
thisTermIsReserved =
    "Deze term is gereserveerd"


pleaseSelectAnItem : String
pleaseSelectAnItem =
    "Selecteer a.u.b. een item"


thisTagIsADuplicateOfAnEarlierOne : String
thisTagIsADuplicateOfAnEarlierOne =
    "Dit etiket is een duplicaat van een eerder etiket"
