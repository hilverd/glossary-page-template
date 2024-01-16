module Data.GlossaryChangelist exposing
    ( GlossaryChangelist
    , create, codec, setLastUpdatedBy
    , applyToVersionNumber, body
    )

{-| A representation of a sequence of changes to be made to a glossary.


# Glossary Changelists

@docs GlossaryChangelist


# Build

@docs create, codec, setLastUpdatedBy


# Query

@docs applyToVersionNumber, body

-}

import Codec exposing (Codec)
import Data.GlossaryChange as GlossaryChange exposing (GlossaryChange)
import Data.GlossaryVersionNumber as GlossaryVersionNumber exposing (GlossaryVersionNumber)


{-| A changelist.
-}
type GlossaryChangelist
    = GlossaryChangelist
        { applyToVersionNumber : GlossaryVersionNumber
        , body : List GlossaryChange
        }


{-| Construct a changelist from a list and a version number for the glossary that the changes are to be applied to.
-}
create : GlossaryVersionNumber -> List GlossaryChange -> GlossaryChangelist
create applyToVersionNumber_ changeList_ =
    GlossaryChangelist
        { applyToVersionNumber = applyToVersionNumber_
        , body = changeList_
        }


{-| Return the version number for the glossary that the changes are to be applied to.
-}
applyToVersionNumber : GlossaryChangelist -> GlossaryVersionNumber
applyToVersionNumber (GlossaryChangelist changelist) =
    changelist.applyToVersionNumber


{-| Return the changelist as a list.
-}
body : GlossaryChangelist -> List GlossaryChange
body (GlossaryChangelist changelist) =
    changelist.body


{-| An encoder/decoder for a changelist.
-}
codec : Codec GlossaryChangelist
codec =
    Codec.object create
        |> Codec.field "applyToVersionNumber" applyToVersionNumber GlossaryVersionNumber.codec
        |> Codec.field "changeList" body (Codec.list GlossaryChange.codec)
        |> Codec.buildObject


{-| Set the name and email address of the person these changes.
-}
setLastUpdatedBy : { name : String, emailAddress : String } -> GlossaryChangelist -> GlossaryChangelist
setLastUpdatedBy nameAndEmailAddress (GlossaryChangelist changelist) =
    GlossaryChangelist
        { changelist
            | body =
                changelist.body
                    |> List.map (GlossaryChange.setLastUpdatedBy nameAndEmailAddress)
        }
