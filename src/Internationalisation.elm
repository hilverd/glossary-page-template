module Internationalisation exposing (sandboxModeMessage)

{-| User interface text in a specific language (e.g. English).
-}

import Internationalisation.Eng as I18n


sandboxModeMessage : String
sandboxModeMessage =
    I18n.sandboxModeMessage
