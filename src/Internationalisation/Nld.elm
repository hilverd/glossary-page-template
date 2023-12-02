module Internationalisation.Nld exposing (alphabetically, ankiDeck, backToTop, export, focusedOn, howToMakeChangesTitle, mostMentionedFirst, noResultsFound, onlyShowingItemsForTag, orderItems, pleaseSelect, quickSearch, runTheFollowingCommand, sandboxModeMessage, searchPlaceholder, tags, themeDark, themeLight, themeSystem, webInterfaceDescription, youCanHideTheseInstructions)

{-| User interface text in the Dutch language.
-}

import Accessibility.Key
import ElementIds
import Extras.Html
import Html exposing (Html, a, code, p, span, text)
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
    "Hoe je veranderingen kunt maken"


webInterfaceDescription : Html msg
webInterfaceDescription =
    p
        [ class "mt-3" ]
        [ text "Deze pagina bevat een web-interface voor het maken van veranderingen die worden teruggeschreven naar het HTML-bestand zelf."
        , text " Deze is bedoeld om "
        , span [ class "font-semibold" ] [ text "lokaal" ]
        , text " te worden gebruikt door "
        , span [ class "font-semibold" ] [ text "een gebruiker" ]
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
        , text " geinstalleerd hebt, geef dan het volgende commando."
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
