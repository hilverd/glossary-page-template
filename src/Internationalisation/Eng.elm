module Internationalisation.Eng exposing (alphabetically, ankiDeck, backToTop, export, focusedOn, howToMakeChangesTitle, mostMentionedFirst, noResultsFound, onlyShowingItemsForTag, orderItems, pleaseSelect, quickSearch, runTheFollowingCommand, sandboxModeMessage, searchPlaceholder, tags, themeDark, themeLight, themeSystem, webInterfaceDescription, youCanHideTheseInstructions)

{-| User interface text in the English language.
-}

import Accessibility.Key
import ElementIds
import Extras.Html
import Html exposing (Html, a, code, p, span, text)
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
