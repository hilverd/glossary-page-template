module Data.GlossaryChangelist exposing
    ( GlossaryChangelist
    , create, codec, setLastUpdatedBy
    , startedFromVersionNumber, body
    )

{-| A representation of a sequence of changes to be made to a glossary.


# Glossary Changelists

@docs GlossaryChangelist


# Build

@docs create, codec, setLastUpdatedBy


# Query

@docs startedFromVersionNumber, body

-}

import Codec exposing (Codec)
import Data.GlossaryChange as GlossaryChange
import Data.GlossaryChangeWithChecksum as GlossaryChangeWithChecksum exposing (GlossaryChangeWithChecksum)
import Data.GlossaryVersionNumber as GlossaryVersionNumber exposing (GlossaryVersionNumber)


{-| A changelist.
-}
type GlossaryChangelist
    = GlossaryChangelist
        { startedFromVersionNumber : GlossaryVersionNumber
        , body : List GlossaryChangeWithChecksum
        }


{-| Construct a changelist from a list and a version number for the glossary that the changes are to be applied to.
-}
create : GlossaryVersionNumber -> List GlossaryChangeWithChecksum -> GlossaryChangelist
create startedFromVersionNumber_ changeList_ =
    GlossaryChangelist
        { startedFromVersionNumber = startedFromVersionNumber_
        , body = changeList_
        }


{-| Return the version number for the glossary that the changes are to be applied to.
-}
startedFromVersionNumber : GlossaryChangelist -> GlossaryVersionNumber
startedFromVersionNumber (GlossaryChangelist changelist) =
    changelist.startedFromVersionNumber


{-| Return the changelist as a list.
-}
body : GlossaryChangelist -> List GlossaryChangeWithChecksum
body (GlossaryChangelist changelist) =
    changelist.body


{-| An encoder/decoder for a changelist.
-}
codec : Codec GlossaryChangelist
codec =
    Codec.object create
        |> Codec.field "startedFromVersionNumber" startedFromVersionNumber GlossaryVersionNumber.codec
        |> Codec.field "changeList" body (Codec.list GlossaryChangeWithChecksum.codec)
        |> Codec.buildObject


{-| Set the name and email address of the person these changes.
-}
setLastUpdatedBy : { name : String, emailAddress : String } -> GlossaryChangelist -> GlossaryChangelist
setLastUpdatedBy nameAndEmailAddress (GlossaryChangelist changelist) =
    GlossaryChangelist
        { changelist
            | body =
                changelist.body
                    |> List.map
                        (\{ glossaryChange, checksum } ->
                            { glossaryChange = GlossaryChange.setLastUpdatedBy nameAndEmailAddress glossaryChange
                            , checksum = checksum
                            }
                        )
        }
