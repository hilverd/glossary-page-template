module Internationalisation exposing (backToTop, export, noResultsFound, quickSearch, sandboxModeMessage, searchPlaceholder)

{-| User interface text in a specific language (e.g. English).
-}

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
