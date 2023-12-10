module Extras.Http exposing (httpErrorDescriptionAskingToReloadOnConflict)

import Http
import Internationalisation as I18n


httpErrorDescriptionAskingToReloadOnConflict : Http.Error -> String
httpErrorDescriptionAskingToReloadOnConflict error =
    case error of
        Http.BadStatus 409 ->
            I18n.otherChangesWereMadePleaseReload

        _ ->
            I18n.httpErrorDescription error
