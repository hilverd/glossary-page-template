module Internationalisation exposing (sandboxModeMessage)

{-| User interface text in a specific language (e.g. English).
-}

import Internationalisation.Eng as I18n


{-| A message indicating to the user that the application is running in sandbox mode.
-}
sandboxModeMessage : String
sandboxModeMessage =
    I18n.sandboxModeMessage
