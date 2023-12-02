module Internationalisation.Nld exposing (backToTop, export, noResultsFound, quickSearch, sandboxModeMessage, searchPlaceholder)

{-| User interface text in the Dutch language.
-}


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
