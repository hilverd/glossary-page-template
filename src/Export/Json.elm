module Export.Json exposing (download)

{-| Functionality for exporting as JSON.

@docs download

-}

import Codec
import Data.Glossary as Glossary exposing (Glossary)
import Data.GlossaryTitle as GlossaryTitle
import File.Download as Download


{-| Export a glossary to a JSON file.
This is achieved by producing a [command for downloading](https://package.elm-lang.org/packages/elm/file/latest/File.Download) this file.
-}
download : Glossary -> Cmd msg
download glossary =
    let
        filename : String
        filename =
            glossary |> Glossary.title |> GlossaryTitle.toFilename ".json"

        content : String
        content =
            Codec.encodeToString 2 Glossary.codec glossary
    in
    Download.string filename "application/json" content
