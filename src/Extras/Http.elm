module Extras.Http exposing (httpErrorDescriptionAskingToReloadOnUnauthorisedOrConflict)

import Http
import Internationalisation as I18n


httpErrorDescriptionAskingToReloadOnUnauthorisedOrConflict : Http.Error -> String
httpErrorDescriptionAskingToReloadOnUnauthorisedOrConflict error =
    case error of
        Http.BadStatus 401 ->
            I18n.unauthorisedPleaseReload

        Http.BadStatus 409 ->
            I18n.otherChangesWereMadePleaseReload

        _ ->
            I18n.httpErrorDescription error
