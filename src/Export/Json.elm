module Export.Json exposing (download)

{-| Functionality for exporting as JSON.

@docs download

-}

import Codec
import Data.GlossaryForUi as GlossaryForUi exposing (GlossaryForUi)
import Data.GlossaryFromDom as GlossaryFromDom
import Data.GlossaryTitle as GlossaryTitle
import File.Download as Download


{-| Export a glossary to a JSON file.
This is achieved by producing a [command for downloading](https://package.elm-lang.org/packages/elm/file/latest/File.Download) this file.
-}
download : GlossaryForUi -> Cmd msg
download glossaryForUi =
    let
        filename : String
        filename =
            glossaryForUi |> GlossaryForUi.title |> GlossaryTitle.toFilename ".json"

        content : String
        content =
            glossaryForUi
                |> GlossaryForUi.toGlossaryFromDom
                |> Codec.encodeToString 2 GlossaryFromDom.codec
    in
    Download.string filename "application/json" content
